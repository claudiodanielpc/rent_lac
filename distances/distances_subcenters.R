#Librerías necesarias para el análisis
if (!require('pacman')) install.packages('pacman')
pacman::p_load(sf, dplyr, tibble, spdep, igraph, readr, fs, archive, fixest)

# ============================================================
#Directorios de trabajo
housing_dir <- fs::path("~/OneDrive - WBG",
                         "Giuliana De Mendiola Ramirez's files - Housing report", "Data")
if (!dir.exists(housing_dir)) {
  housing_dir <- fs::path("G:/Mi unidad/consultoría/world_bank")
}
# ============================================================

# ============================================================
# PARTE 0 -- Descarga del marco geoestadístico de INEGI (si es que no existe en el directorio)
# ============================================================
inegi_zip_url <- "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/794551067314/mg_2023_integrado.zip"
destino  <- housing_dir
zip_path <- fs::path(destino, "mg_2023_integrado.zip")
ageb_shp <- fs::path(destino, "conjunto_de_datos", "00a.shp")

if (!file.exists(ageb_shp)) {

  cat("No encontré el shapefile de AGEB en disco -- descargando el marco geoestadístico de INEGI (~3 GB, puede tardar varios minutos)...\n")
  fs::dir_create(destino)

  if (file.exists(zip_path) && file.info(zip_path)$size / 1e9 < 2.9) {
    cat("Encontré un zip incompleto de una descarga anterior -- lo borro y vuelvo a descargar.\n")
    file.remove(zip_path)
  }

  if (!file.exists(zip_path)) {
    old_timeout <- getOption("timeout")
    options(timeout = 7200)  
    download.file(inegi_zip_url, zip_path, mode = "wb", method = "libcurl")
    options(timeout = old_timeout)
  }

  cat("Descarga lista (", round(file.info(zip_path)$size / 1e9, 2), "GB). Extrayendo...\n")
  archive::archive_extract(zip_path, dir = destino)

  if (!file.exists(ageb_shp)) {
    stop("Extraje el zip pero no encuentro ", ageb_shp,
         " -- revisa la estructura de carpetas dentro de ", destino,
         " (puede que INEGI haya cambiado el nombre/estructura del archivo).")
  }
  cat("Listo -- shapefile de AGEB en", ageb_shp, "\n\n")

} else {
  cat("Ya existe el shapefile de AGEB en disco, no hace falta descargar de nuevo:", ageb_shp, "\n\n")
}

## --- 1. Polígonos de AGEB oficiales (INEGI) ------------------------------
ageb_poly  <- st_read(ageb_shp) |> select(CVEGEO)
target_crs <- st_crs(ageb_poly)

## --- 2. Censo de empresas por AGEB -----------------------
firmcensus2023 <- st_read(file.path(housing_dir, "firmcensus2023.geojson")) |>
  st_drop_geometry()


## --- 3. Consolidación de ciudades -----------------------------------------
city_consolidation <- tribble(
  ~City,                       ~City_consolidated, ~City_ID_consolidated,
  "Ciudad de México",          "Ciudad de México",          84,
  "Monterrey",                 "Monterrey",                255,
  "García",                    "Monterrey",                255,
  "Salinas Victoria",          "Monterrey",                255,
  "Guadalajara",               "Guadalajara",              152,
  "León",                      "León",                      90,
  "Puebla",                    "Puebla",                   284,
  "Querétaro",                 "Querétaro",                307,
  "Tijuana",                   "Tijuana",                    6,
  "Toluca",                    "Toluca",                   193
)

firmcensus_ok <- firmcensus2023 |>
  left_join(city_consolidation, by = "City") |>
  mutate(
    City    = coalesce(City_consolidated, City),
    City_ID = coalesce(City_ID_consolidated, City_ID)
  ) |>
  select(-City_consolidated, -City_ID_consolidated) |>
  filter(City_ID %in% c(84, 255, 152, 90, 284, 307, 6, 193))  # solo las 8 ciudades

## --- 4. Une geometría + empleo ---------------------------------------------
ageb_data_sp <- ageb_poly |> inner_join(firmcensus_ok, by = "CVEGEO")

cat("AGEB con geometría + empleo, en las 8 ciudades:", nrow(ageb_data_sp), "\n")
print(table(ageb_data_sp$City))

## --- 5. Mapeo region -> City/City_ID para el scraping de propiedades -----------
region_to_city <- tribble(
  ~region,               ~City,               ~City_ID,
  "ZM CDMX",             "Ciudad de México",   84,
  "ZM Guadalajara",      "Guadalajara",        152,
  "ZM León",             "León",               90,
  "ZM Monterrey",        "Monterrey",          255,
  "ZM Puebla-Tlaxcala",  "Puebla",             284,
  "ZM Querétaro",        "Querétaro",          307,
  "ZM Tijuana",          "Tijuana",              6,
  "ZM Toluca",           "Toluca",             193
)
###Leer datos del scraping de propiedades
housing <- read_csv(file.path(housing_dir, "clean_scrap_cities.csv")) |>
  filter(operation == "sell", country == "Mexico") |>
  mutate(
    bedrooms  = as.numeric(bedrooms),
    bathrooms = as.numeric(bathrooms),
    size      = as.numeric(size)
  ) |>
  inner_join(region_to_city, by = "region") |>
  filter(!is.na(longitude), !is.na(latitude))

pts_sf <- housing |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# ============================================================
# PARTE A -- Identificar subcentros (Gi* y top-5%)
# ============================================================

## --- Método 1: Getis-Ord Gi* (hot-spot espacial de empleo) ---------------
gi_by_city <- list()

for (city in unique(ageb_data_sp$City_ID)) {
  city_data <- ageb_data_sp |> filter(City_ID == city) |> st_as_sf()

  coords    <- st_coordinates(st_centroid(city_data))
  knn_nb    <- knn2nb(knearneigh(coords, k = 5))
  knn_listw <- nb2listw(knn_nb, style = "W")
  gi        <- localG(city_data$personal_total, knn_listw)

  gi_by_city[[as.character(city)]] <- city_data |>
    st_drop_geometry() |>
    mutate(gi_score = as.numeric(gi), subcenter_Gi = gi_score > 1.96) |>
    select(CVEGEO, gi_score, subcenter_Gi)
}
result_Gi <- do.call(rbind, gi_by_city)

## --- Método 2: top 5% de empleo + contigüidad espacial ---------
igraph_by_city <- list()

for (city in unique(ageb_data_sp$City)) {
  city_data <- ageb_data_sp |> filter(City == city) |> st_as_sf()

  emp_threshold <- quantile(city_data$personal_total, 0.95, na.rm = TRUE)
  high_emp      <- city_data |> filter(personal_total > emp_threshold)

  city_data <- city_data |> mutate(subcenter_id = NA_integer_, subcenter_95 = FALSE)

  if (nrow(high_emp) > 0) {
    nb   <- st_touches(high_emp, sparse = FALSE)
    g    <- graph_from_adjacency_matrix(nb, mode = "undirected")
    comp <- components(g)

    match_idx <- match(high_emp$CVEGEO, city_data$CVEGEO)
    city_data$subcenter_id[match_idx] <- comp$membership
    city_data$subcenter_95[match_idx] <- TRUE
  }

  igraph_by_city[[as.character(city)]] <- city_data |>
    st_drop_geometry() |>
    select(CVEGEO, subcenter_id, subcenter_95)
}
result_igraph <- do.call(rbind, igraph_by_city)

## --- Merge de los dos métodos, frescos --------------------------------------
subcenters_frescos <- result_Gi |> left_join(result_igraph, by = "CVEGEO")

chequeo <- ageb_data_sp |>
  st_drop_geometry() |>
  select(CVEGEO, City) |>
  left_join(subcenters_frescos, by = "CVEGEO") |>
  group_by(City) |>
  summarise(
    n_Gi           = sum(subcenter_Gi, na.rm = TRUE),
    n_95           = sum(subcenter_95, na.rm = TRUE),
    n_overlap      = sum(subcenter_Gi & subcenter_95, na.rm = TRUE),
    sets_identicos = identical(sort(CVEGEO[subcenter_Gi]), sort(CVEGEO[subcenter_95]))
  )

cat("\n=== Comparación Gi* vs top-5%/igraph, recién calculados ===\n")
print(chequeo)

# ============================================================
#  Recalcular median_price / median_bedrooms / median_bathrooms
# / median_sup_m2 / n_obs a nivel AGEB
# ============================================================
joined <- st_join(st_transform(pts_sf, target_crs), ageb_poly, join = st_within) |>
  st_drop_geometry()


block_prices <- joined |>
  filter(!is.na(CVEGEO)) |>
  group_by(CVEGEO) |>
  summarise(
    median_price     = median(price_per_sq_meter, na.rm = TRUE),
    n_obs            = n(),
    median_bedrooms  = median(bedrooms, na.rm = TRUE),
    median_bathrooms = median(bathrooms, na.rm = TRUE),
    median_sup_m2    = median(size, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_obs >= 3)

cat("\nAGEB con >= 3 propiedades para recalcular median_price:", nrow(block_prices), "\n")


## --- Reemplaza las banderas viejas en subcenters.geojson --------------------
subcenters_viejo <- st_read(file.path(housing_dir, "subcenters.geojson"))

crs_viejo <- st_crs(subcenters_viejo)$input
if (is.na(crs_viejo)) crs_viejo <- "(sin CRS)"
cat("CRS de la geometría vieja en subcenters.geojson:", crs_viejo, "\n")



#Corregir proyección de subcenters
subcenters_corregido <- subcenters_viejo |>
  st_drop_geometry() |>
  select(-any_of(c("gi_score", "subcenter_Gi", "subcenter_95", "subcenter_id", "num_subcenters",
                    "median_price", "n_obs", "median_bedrooms", "median_bathrooms", "median_sup_m2"))) |>
  left_join(subcenters_frescos, by = "CVEGEO") |>
  left_join(block_prices, by = "CVEGEO") |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)  


#Salvar geojson actualizado
ruta_final <- file.path(housing_dir, "subcenters.geojson")
if (file.exists(ruta_final)) file.remove(ruta_final)
st_write(subcenters_corregido, ruta_final, driver = "GeoJSON")
cat("\nsubcenters.geojson actualizado con subcentros recalculados desde los polígonos de INEGI.\n\n")

# ============================================================
# PARTE B -- Distancia ponderada por empleo a subcentros
# ============================================================

subcenters <- subcenters_corregido |>
  st_drop_geometry() |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)



## Distancia ponderada por empleo, por ciudad ------------------------------
compute_weighted_subcenter_distance <- function(unit_sf, subcenters_sf,
                                                 subcenter_flag,
                                                 weight_col = "personal_total") {
  out <- rep(NA_real_, nrow(unit_sf))

  for (city in unique(unit_sf$City_ID)) {
    idx        <- which(unit_sf$City_ID == city)
    units_city <- unit_sf[idx, ]

    subc_city <- subcenters_sf |>
      filter(City_ID == city, .data[[subcenter_flag]])

    if (nrow(subc_city) == 0) next  # ciudad sin subcentros -> NA

    # sf calcula distancia geodésica (metros) directo sobre lon/lat con s2
    dist_mat <- matrix(
      as.numeric(st_distance(units_city, subc_city)),
      nrow = nrow(units_city)
    )

    w <- subc_city[[weight_col]]
    out[idx] <- as.numeric(dist_mat %*% w) / sum(w)  # promedio ponderado
  }

  out
}

## --- Nivel propiedad --------------------------------------------------------
pts_sf$dist_subcenter_Gi_km <- compute_weighted_subcenter_distance(pts_sf, subcenters, "subcenter_Gi") / 1000
pts_sf$dist_subcenter_95_km <- compute_weighted_subcenter_distance(pts_sf, subcenters, "subcenter_95") / 1000



## Filtrar propiedadades con coordenadas mal geocodificadas
n_antes <- nrow(pts_sf)
pts_sf  <- pts_sf |> filter(dist_subcenter_Gi_km < 100, dist_subcenter_95_km < 100)
cat("Propiedades descartadas por coordenadas mal geocodificadas:",
    n_antes - nrow(pts_sf), "de", n_antes, "\n")


##Salvar archivos

write_excel_csv(st_drop_geometry(pts_sf), file.path(housing_dir, "distance_subcenters_properties.csv"))

## --- Nivel AGEB (usa el mismo subcenters ya corregido) ----------------------
subcenters$dist_subcenter_Gi_km <- compute_weighted_subcenter_distance(subcenters, subcenters, "subcenter_Gi") / 1000
subcenters$dist_subcenter_95_km <- compute_weighted_subcenter_distance(subcenters, subcenters, "subcenter_95") / 1000

write_excel_csv(st_drop_geometry(subcenters), file.path(housing_dir, "distance_subcenters_agebs.csv"))

cat("\nListo:\n")
cat(" -", file.path(housing_dir, "distance_subcenters_properties.csv"), "\n")
cat(" -", file.path(housing_dir, "distance_subcenters_agebs.csv"), "\n")