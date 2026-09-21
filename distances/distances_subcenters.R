#Librerías necesarias para el análisis
if (!require('pacman')) install.packages('pacman')
pacman::p_load(sf, dplyr, tibble, spdep, igraph, readr, fs, archive, fixest)

# ============================================================
# Directorios de trabajo
# ============================================================

housing_dir <- fs::path("~/OneDrive - WBG",
                         "Giuliana De Mendiola Ramirez's files - Housing report", "Data")
if (!dir.exists(housing_dir)) {
  housing_dir <- fs::path("G:/Mi unidad/consultoría/world_bank")
}

# ============================================================
# PARTE 0 -- Descarga del marco geoestadístico de INEGI (si no existe en el directorio de trabajo)
# ============================================================
inegi_zip_url <- "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/794551067314/mg_2023_integrado.zip"
destino  <- housing_dir
zip_path <- fs::path(destino, "mg_2023_integrado.zip")
ageb_shp <- fs::path(destino, "conjunto_de_datos", "00a.shp")

if (!file.exists(ageb_shp)) {

  cat("No se encontra el shapefile de AGEB en disco -- descargando el marco geoestadístico de INEGI...\n")
  fs::dir_create(destino)

  
  if (file.exists(zip_path) && file.info(zip_path)$size / 1e9 < 2.9) {
    cat("Se encontró un zip incompleto de una descarga anterior -- se borra y se vuelve a descargar.\n")
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
    stop("Se extrajo el zip pero no se encuentra ", ageb_shp,
         " -- revisa la estructura de carpetas dentro de ", destino,
         " (puede que INEGI haya cambiado el nombre/estructura del archivo).")
  }
  cat("Listo -- shapefile de AGEB en", ageb_shp, "\n\n")

} else {
  cat("Ya existe el shapefile de AGEB en disco, no hace falta descargar de nuevo:", ageb_shp, "\n\n")
}

## --- 1. Polígonos de AGEB oficiales (INEGI) ------------------------------
ageb_poly  <- st_read(ageb_shp) |> select(CVEGEO)


ageb_poly  <- st_make_valid(ageb_poly)
target_crs <- st_crs(ageb_poly)

#Centroides de AGEB para calcular distancias a subcentros
centroids <- ageb_poly |>
  st_transform(4326) |>
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  rename(lon = X, lat = Y) |>
  mutate(CVEGEO = ageb_poly$CVEGEO)

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

## --- 5. Utilizar los datos de webscraping de propiedades para calcular distancias a subcentros
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

housing <- read_csv(file.path(housing_dir, "clean_scrap_cities.csv")) |>
  filter(operation == "sell", country == "Mexico") |>
  mutate(
    bedrooms  = as.numeric(bedrooms),
    bathrooms = as.numeric(bathrooms),
    size      = as.numeric(size)
  ) |>
  inner_join(region_to_city, by = "region") |>   # descarta listings fuera de las 8 ciudades
  filter(!is.na(longitude), !is.na(latitude))

pts_sf <- housing |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# ============================================================
# PARTE A -- Identificar subcentros (Gi* y top-5%/igraph)
# ============================================================

## --- Método 1: Getis-Ord Gi* (hot-spot espacial de empleo) ---------------

gi_by_city <- list()

for (city in unique(ageb_data_sp$City_ID)) {
  city_data <- ageb_data_sp |> filter(City_ID == city) |> st_as_sf()

  coords    <- st_coordinates(st_centroid(city_data))
  knn_nb    <- knn2nb(knearneigh(coords, k = 5)) |> include.self()
  knn_listw <- nb2listw(knn_nb, style = "W")
  gi        <- localG(city_data$personal_total, knn_listw)

  gi_by_city[[as.character(city)]] <- city_data |>
    st_drop_geometry() |>
    mutate(gi_score = as.numeric(gi), subcenter_Gi = gi_score > 1.96) |>
    select(CVEGEO, gi_score, subcenter_Gi)
}
result_Gi <- do.call(rbind, gi_by_city)

## --- Método 2: top 5% de empleo + AGEBs vecinos (contigüidad, igraph) ---
igraph_by_city <- list()

for (city in unique(ageb_data_sp$City)) {
  city_data <- ageb_data_sp |> filter(City == city) |> st_as_sf()

  emp_threshold <- quantile(city_data$personal_total, 0.95, na.rm = TRUE)
  high_emp_idx  <- which(city_data$personal_total > emp_threshold)

  city_data <- city_data |> mutate(subcenter_id = NA_integer_, subcenter_95 = FALSE)

  if (length(high_emp_idx) > 0) {
    nb_all       <- st_touches(city_data)  # vecinos de TODOS los AGEB de la ciudad
    neighbor_idx <- unique(unlist(nb_all[high_emp_idx]))
    subcenter_95_idx <- union(high_emp_idx, neighbor_idx)

    city_data$subcenter_95[subcenter_95_idx] <- TRUE

    sub_data <- city_data[subcenter_95_idx, ]
    nb_sub   <- st_touches(sub_data, sparse = FALSE)
    g        <- graph_from_adjacency_matrix(nb_sub, mode = "undirected")
    comp     <- components(g)
    city_data$subcenter_id[subcenter_95_idx] <- comp$membership
  }

  igraph_by_city[[as.character(city)]] <- city_data |>
    st_drop_geometry() |>
    select(CVEGEO, subcenter_id, subcenter_95)
}
result_igraph <- do.call(rbind, igraph_by_city)

## --- Merge de los dos métodos --------------------------------------
subcenters_nuevos <- result_Gi |> left_join(result_igraph, by = "CVEGEO")

chequeo <- ageb_data_sp |>
  st_drop_geometry() |>
  select(CVEGEO, City) |>
  left_join(subcenters_nuevos, by = "CVEGEO") |>
  group_by(City) |>
  summarise(
    n_Gi           = sum(subcenter_Gi, na.rm = TRUE),
    n_95           = sum(subcenter_95, na.rm = TRUE),
    n_overlap      = sum(subcenter_Gi & subcenter_95, na.rm = TRUE),
    sets_identicos = identical(sort(CVEGEO[subcenter_Gi]), sort(CVEGEO[subcenter_95]))
  )

cat("\n=== Comparación Gi* vs top-5%/igraph, recién calculados ===\n")
print(chequeo)

## --- Reemplaza las banderas viejas en subcenters.geojson --------------------
ruta_final <- file.path(housing_dir, "subcenters.geojson")

if (file.exists(ruta_final)) {

  subcenters_viejo <- st_read(ruta_final)

  crs_viejo <- st_crs(subcenters_viejo)$input
  if (is.na(crs_viejo)) crs_viejo <- "(sin CRS)"
  cat("CRS de la geometría vieja en subcenters.geojson:", crs_viejo, "\n")

#Cambiar proyección de subcenters
  subcenters_corregido <- subcenters_viejo |>
    st_drop_geometry() |>
    select(-any_of(c("gi_score", "subcenter_Gi", "subcenter_95", "subcenter_id", "num_subcenters",
                      "median_price", "n_obs", "median_bedrooms", "median_bathrooms", "median_sup_m2"))) |>
    left_join(subcenters_nuevos, by = "CVEGEO") |>
    filter(!is.na(lon), !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

} else {

  cat("\nNo se encuentra subcenters.geojson en", housing_dir, "-- se arma de cero con lo que sí se puede calcular sin el gpkg.\n")

  subcenters_corregido <- ageb_data_sp |>
    st_drop_geometry() |>
    left_join(subcenters_nuevos, by = "CVEGEO") |>
    
    left_join(centroids, by = "CVEGEO") |>
    filter(!is.na(lon), !is.na(lat)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)  # remove=FALSE: conserva lon/lat como columnas
}

#Salvar subcenters.geojson actualizado
if (file.exists(ruta_final)) file.remove(ruta_final)
st_write(subcenters_corregido, ruta_final, driver = "GeoJSON")
cat("\nsubcenters.geojson actualizado con subcentros recalculados desde los polígonos de INEGI.\n\n")

# ============================================================
# PARTE B -- Distancia ponderada por empleo a subcentros
# ============================================================

subcenters <- subcenters_corregido |>
  st_drop_geometry() |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)  # remove=FALSE: conserva lon/lat como columnas


## Distancia ponderada por empleo, por ciudad ------------------------------
compute_weighted_subcenter_distance <- function(unit_sf, subcenters_sf,
                                                 subcenter_flag,
                                                 weight_col = "personal_total",
                                                 chunk_size = 2000) {
  out <- rep(NA_real_, nrow(unit_sf))

  for (city in unique(unit_sf$City_ID)) {
    idx        <- which(unit_sf$City_ID == city)
    units_city <- unit_sf[idx, ]

    subc_city <- subcenters_sf |>
      filter(City_ID == city, .data[[subcenter_flag]])

    if (nrow(subc_city) == 0) next  # ciudad sin subcentros -> NA

    w      <- subc_city[[weight_col]]
    dist_w <- numeric(nrow(units_city))

    for (inicio in seq(1, nrow(units_city), by = chunk_size)) {
      fin   <- min(inicio + chunk_size - 1, nrow(units_city))
      bloque <- units_city[inicio:fin, ]

      # sf calcula distancia geodésica (metros) directo sobre lon/lat
      dist_mat <- matrix(
        as.numeric(st_distance(bloque, subc_city)),
        nrow = nrow(bloque)
      )

      dist_w[inicio:fin] <- as.numeric(dist_mat %*% w) / sum(w)  # promedio ponderado
    }

    out[idx] <- dist_w
  }

  out
}

## --- Nivel propiedad --------------------------------------------------------
pts_sf$dist_subcenter_Gi_km <- compute_weighted_subcenter_distance(pts_sf, subcenters, "subcenter_Gi") / 1000
pts_sf$dist_subcenter_95_km <- compute_weighted_subcenter_distance(pts_sf, subcenters, "subcenter_95") / 1000

##Filtrar propiedades con coordenadas mal geocodificadas
n_antes <- nrow(pts_sf)
pts_sf  <- pts_sf |> filter(dist_subcenter_Gi_km < 100, dist_subcenter_95_km < 100)
cat("Propiedades descartadas por coordenadas mal geocodificadas:",
    n_antes - nrow(pts_sf), "de", n_antes, "\n")

#Salvar csv de distancias a subcentros
write_excel_csv(st_drop_geometry(pts_sf), file.path(housing_dir, "distance_subcenters_properties.csv"))

## --- Nivel AGEB ----------------------
subcenters$dist_subcenter_Gi_km <- compute_weighted_subcenter_distance(subcenters, subcenters, "subcenter_Gi") / 1000
subcenters$dist_subcenter_95_km <- compute_weighted_subcenter_distance(subcenters, subcenters, "subcenter_95") / 1000

write_excel_csv(st_drop_geometry(subcenters), file.path(housing_dir, "distance_subcenters_agebs.csv"))

cat("\nListo:\n")
cat(" -", file.path(housing_dir, "distance_subcenters_properties.csv"), "\n")
cat(" -", file.path(housing_dir, "distance_subcenters_agebs.csv"), "\n")
