#Librerías necesarias para el análisis


options(java.parameters = "-Xmx8G")

if (!require('pacman')) install.packages('pacman')
pacman::p_load(sf, dplyr, tibble, spdep, igraph, readr, fs, archive, fixest, r5r, data.table)

## Memoria para Java
rJava::.jinit()
rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
cat("Memoria máxima de Java:", round(rJava::.jcall(rt, "J", "maxMemory") / 1e9, 1), "GB\n")

## Hilos que usa r5r en paralelo. Cada hilo guarda su propia búsqueda sobre
## la red

r5r_threads <- 4L

# ============================================================
# Directorios de trabajo
# ============================================================

housing_dir <- fs::path("~/OneDrive - WBG",
                         "Giuliana De Mendiola Ramirez's files - Housing report", "Data")
if (!dir.exists(housing_dir)) {
  housing_dir <- fs::path("G:/Mi unidad/consultoria/world_bank")
}



# ============================================================
# Ciudades del análisis
# ============================================================

##   84  = Ciudad de México   152 = Guadalajara   307 = Querétaro
##   255 = Monterrey            6 = Tijuana       284 = Puebla
##   193 = Toluca              90 = León
ciudades_ids <- c(84, 152, 307, 255, 6, 284, 193, 90)

# ============================================================
# Descarga del marco geoestadístico de INEGI (si no existe en el directorio de trabajo)
# ============================================================
inegi_zip_url <- "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/794551067314/mg_2023_integrado.zip"
destino  <- housing_dir
zip_path <- fs::path(destino, "mg_2023_integrado.zip")
ageb_shp <- fs::path(destino, "conjunto_de_datos", "00a.shp")

if (!file.exists(ageb_shp)) {

  cat("No se encuentra el shapefile de AGEB en disco -- descargando el marco geoestadístico de INEGI...\n")
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

n_antes_dissolve <- nrow(ageb_poly)
ageb_poly <- ageb_poly |> group_by(CVEGEO) |> summarise(.groups = "drop")
cat("Polígonos de AGEB:", n_antes_dissolve, "-> después de unir por CVEGEO (1 por AGEB):", nrow(ageb_poly), "\n")

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
  filter(City_ID %in% ciudades_ids)  # solo las ciudades del análisis

## --- 4. Une geometría + empleo ---------------------------------------------
ageb_data_sp <- ageb_poly |> inner_join(firmcensus_ok, by = "CVEGEO")

cat("AGEB con geometría + empleo, en las ciudades del análisis:", nrow(ageb_data_sp), "\n")
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
) |>
  filter(City_ID %in% ciudades_ids)

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
# PARTE A -- Identificar subcentros (Gi* y top-5%/igraph)
# ============================================================

## --- Método 1: Getis-Ord Gi* ---------------

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

## --- Método 2: top 5% de empleo + AGEBs vecinos ---
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
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)  
}


#Salvar subcenters.geojson actualizado
if (file.exists(ruta_final)) file.remove(ruta_final)
st_write(subcenters_corregido, ruta_final, driver = "GeoJSON")
cat("\nsubcenters.geojson actualizado con subcentros recalculados desde los polígonos de INEGI.\n\n")

# ============================================================
# PARTE B -- Distancia y tiempo ponderados por empleo a subcentros,
# POR RED VIAL (auto) con r5r
# ============================================================


subcenters <- subcenters_corregido |>
  st_drop_geometry() |>
  filter(City_ID %in% ciudades_ids) |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>  # remove=FALSE: conserva lon/lat como columnas
  mutate(id = CVEGEO)  

pts_sf <- pts_sf |>
  filter(City_ID %in% ciudades_ids) |>
  mutate(id = as.character(row_number()))

cat("Ciudades para el cálculo de distancias por red vial:\n")
print(table(subcenters$City_ID))

## --- 0. Osmosis (para recortar la red vial por ciudad) --------------------
## Instalar Osmosis (una sola vez, no lo instala R):
##   1. Descargar el zip desde https://github.com/openstreetmap/osmosis/releases
##      (osmosis-x.x.x.zip, no hace falta instalador)
##   2. Descomprimirlo en cualquier carpeta, ej. C:/osmosis
##   3. Poner la ruta a bin/osmosis.bat abajo en osmosis_bat
osmosis_bat <- "C:\\Users\\claud\\Documents\\osmosis-0.49.2\\bin\\osmosis.bat"  # <- AJUSTAR de acuerdo a cada computadora

if (!file.exists(osmosis_bat)) {
  stop("No se encuentra Osmosis en ", osmosis_bat,
       " -- descargalo de https://github.com/openstreetmap/osmosis/releases, ",
       "descomprimilo, y corregí la ruta osmosis_bat arriba.")
}

## --- 1. Red vial de México (OSM), descarga completa -- se recorta después

osm_url <- "https://download.geofabrik.de/north-america/mexico-latest.osm.pbf"
red_dir <- tools::R_user_dir("r5r_mexico", which = "cache")
osm_pbf <- fs::path(red_dir, "mexico-latest.osm.pbf")

if (!file.exists(osm_pbf)) {
  cat("No se encuentra la red vial de México en disco -- descargando de Geofabrik (~1-1.5 GB)...\n")
  fs::dir_create(red_dir)
  old_timeout <- getOption("timeout")
  options(timeout = 7200)
  download.file(osm_url, osm_pbf, mode = "wb", method = "libcurl")
  options(timeout = old_timeout)
  cat("Descarga lista (", round(file.info(osm_pbf)$size / 1e9, 2), "GB).\n\n")
} else {
  cat("Ya existe la red vial de México en disco:", osm_pbf, "\n\n")
}

## --- 2. Bounding box por ciudad --------------------------------------------
## El bbox se arma SOLO con los centroides de los AGEB de cada ciudad (marco de INEGI, sin errores de geocodificación) + un buffer de ~0.3 grados
## (~30 km) para que las rutas no se corten justo en el borde.

## Las propiedades mal geocodificadas quedan fuera de la red de su ciudad,
## reciben NA y se descartan al final con el filtro de <100 km.
buffer_deg <- 0.3

city_bbox <- subcenters |>
  st_drop_geometry() |>
  group_by(City_ID) |>
  summarise(
    xmin = min(lon, na.rm = TRUE) - buffer_deg,
    xmax = max(lon, na.rm = TRUE) + buffer_deg,
    ymin = min(lat, na.rm = TRUE) - buffer_deg,
    ymax = max(lat, na.rm = TRUE) + buffer_deg,
    .groups = "drop"
  ) |>
  mutate(area_km2 = (xmax - xmin) * 111 * cos(((ymin + ymax) / 2) * pi / 180) *
                    (ymax - ymin) * 111)

cat("\n=== Bounding box por ciudad ===\n")
print(city_bbox)

## Chequeo: una zona metropolitana + 30 km de buffer debería estar
## en el orden de miles o decenas de miles de km2, nunca cientos de miles.
if (any(city_bbox$area_km2 > 100000)) {
  stop("Hay un bbox de ciudad sospechosamente grande (> 100,000 km2) -- revisá city_bbox arriba.")
}

clip_pbf_ciudad <- function(city_id, bbox_row) {
  city_dir <- fs::path(red_dir, paste0("ciudad_", city_id))
  city_pbf <- fs::path(city_dir, "red.osm.pbf")
  bbox_txt <- fs::path(city_dir, "bbox.txt")

  bbox_str <- sprintf("%.6f,%.6f,%.6f,%.6f",
                      bbox_row$xmin, bbox_row$ymin, bbox_row$xmax, bbox_row$ymax)

  ## Si ya hay un recorte de una corrida anterior pero con OTRO bbox, se borra la carpeta completa de la ciudad (pbf
  ## + network.dat + demás archivos de r5r) para que se vuelva a recortar y a armar la red con el bbox correcto.
  if (dir.exists(city_dir)) {
    bbox_previo <- if (file.exists(bbox_txt)) readLines(bbox_txt, warn = FALSE) else ""
    if (!file.exists(city_pbf) || !identical(bbox_previo, bbox_str)) {
      cat("Caché de red vial de la ciudad", city_id, "desactualizada -- se borra y se recorta de nuevo.\n")
      fs::dir_delete(city_dir)
    }
  }
  fs::dir_create(city_dir)

  if (!file.exists(city_pbf)) {

    osmosis_run  <- if (.Platform$OS.type == "windows") utils::shortPathName(osmosis_bat) else osmosis_bat
    osm_pbf_run  <- if (.Platform$OS.type == "windows") utils::shortPathName(osm_pbf)     else osm_pbf
    city_dir_run <- if (.Platform$OS.type == "windows") utils::shortPathName(city_dir)    else city_dir
    city_pbf_run <- file.path(city_dir_run, "red.osm.pbf")

    cat("Recortando red vial para ciudad", city_id, "...\n")
    salida <- system2(
      osmosis_run,
      args = c(
        "--read-pbf", osm_pbf_run,
        "--bounding-box",
        sprintf("left=%f", bbox_row$xmin),
        sprintf("bottom=%f", bbox_row$ymin),
        sprintf("right=%f", bbox_row$xmax),
        sprintf("top=%f", bbox_row$ymax),
        "clipIncompleteEntities=true",
        "--write-pbf", city_pbf_run
      ),
      stdout = TRUE, stderr = TRUE
    )
    cat(paste(salida, collapse = "\n"), "\n")

    if (!file.exists(city_pbf)) {
      stop("Osmosis no generó el archivo esperado: ", city_pbf,
           " -- revisá el mensaje de la consola de arriba para ver qué falló exactamente.")
    }
    writeLines(bbox_str, bbox_txt)
  }
  city_dir
}

## --- 3. Distancia y tiempo ponderados por empleo, red por ciudad ----------

## Con eso se sabe qué puntos están conectados a la red. Se descartan:
##   - subcentros que no llegan a NINGÚN punto, y
##   - puntos a los que no llega NINGÚN subcentro.
## Son puntos pegados a tramos de calle aislados (o mal geocodificados).
## Con ellos, detailed_itineraries() explora TODA la red antes de rendirse

## El resultado se guarda en disco (conectividad_ciudad_<ID>.rds), así que
## si la sesión se cae no se vuelve a hacer.
chequeo_conectividad <- function(puntos, subcentros, r5r_network, archivo,
                                 mode = "CAR",
                                 departure_datetime = as.POSIXct("2026-10-06 14:00:00", tz = "America/Mexico_City"),
                                 max_trip_duration = 180L,
                                 n_threads = r5r_threads,
                                 bloque_subcentros = 10L) {

  if (file.exists(archivo)) {
    cat("Chequeo de conectividad ya hecho -- se lee de", archivo, "\n")
    return(readRDS(archivo))
  }

  cat("Chequeando qué puntos están conectados a la red (una sola vez)...\n")
  bloques <- split(seq_len(nrow(subcentros)), ceiling(seq_len(nrow(subcentros)) / bloque_subcentros))
  ids_puntos_ok     <- character(0)
  ids_subcentros_ok <- character(0)

  for (j in seq_along(bloques)) {
    cat("   chequeo", j, "de", length(bloques), format(Sys.time(), "(%H:%M)"), "\n")
    ttm <- r5r::travel_time_matrix(
      r5r_network        = r5r_network,
      origins            = subcentros[bloques[[j]], ],
      destinations       = puntos,
      mode               = mode,
      departure_datetime = departure_datetime,
      max_trip_duration  = max_trip_duration,
      n_threads          = n_threads,
      verbose            = FALSE,
      progress           = FALSE
    )
    ids_subcentros_ok <- union(ids_subcentros_ok, unique(ttm$from_id))
    ids_puntos_ok     <- union(ids_puntos_ok,     unique(ttm$to_id))
    rm(ttm); gc()
  }

  res <- list(puntos = ids_puntos_ok, subcentros = ids_subcentros_ok)
  saveRDS(res, archivo)
  cat("   puntos conectados:", length(ids_puntos_ok), "de", nrow(puntos),
      "| subcentros conectados:", length(ids_subcentros_ok), "de", nrow(subcentros), "\n")
  res
}

compute_weighted_subcenter_r5r <- function(unit_sf, lon_col, lat_col,
                                            subcenters_sf, subcenter_flag,
                                            conectividad,
                                            weight_col = "personal_total",
                                            r5r_network,
                                            mode = "CAR",
                                            departure_datetime = as.POSIXct("2026-10-06 14:00:00", tz = "America/Mexico_City"),
                                            max_trip_duration = 180L,
                                            chunk_size = 200L,
                                            n_threads = r5r_threads,
                                            checkpoint_dir = NULL) {
  units_city <- unit_sf |> st_drop_geometry()

  subc_city <- subcenters_sf |>
    st_drop_geometry() |>
    filter(.data[[subcenter_flag]])

  if (nrow(subc_city) == 0) {
    return(list(dist_km = rep(NA_real_, nrow(units_city)), time_min = rep(NA_real_, nrow(units_city))))
  }

  origenes_todos <- units_city |> transmute(id = id, lon = .data[[lon_col]], lat = .data[[lat_col]])
  destinos_todos <- subc_city  |> transmute(id = id, lon = lon, lat = lat)
  pesos          <- subc_city  |> select(id, w = all_of(weight_col))

  ## Solo puntos que pasaron el chequeo de conectividad de la ciudad
  origenes <- origenes_todos |> filter(id %in% conectividad$puntos)
  destinos <- destinos_todos |> filter(id %in% conectividad$subcentros)

  cat("   ", subcenter_flag, "- orígenes conectados:", nrow(origenes), "de", nrow(origenes_todos),
      "| destinos conectados:", nrow(destinos), "de", nrow(destinos_todos), "\n")

  if (nrow(origenes) == 0 || nrow(destinos) == 0) {
    return(list(dist_km = rep(NA_real_, nrow(units_city)), time_min = rep(NA_real_, nrow(units_city))))
  }

  ## --- Rutas detalladas (distancia + tiempo) solo entre puntos conectados --
  bloques <- split(seq_len(nrow(origenes)), ceiling(seq_len(nrow(origenes)) / chunk_size))
  resumen_bloques <- vector("list", length(bloques))

  ## Guardado por bloque: cada bloque terminado se guarda en checkpoint_dir.
  ## Si la sesión se cae, al volver a correr solo se calculan los bloques
  ## que faltan. 
  if (!is.null(checkpoint_dir)) fs::dir_create(checkpoint_dir)
  archivo_bloque <- function(k) {
    fs::path(checkpoint_dir, sprintf("%s_cs%d_n%d_b%05d.rds",
                                     subcenter_flag, chunk_size, length(bloques), k))
  }

  for (k in seq_along(bloques)) {

    if (!is.null(checkpoint_dir) && file.exists(archivo_bloque(k))) {
      resumen_bloques[k] <- list(readRDS(archivo_bloque(k)))
      next
    }

    cat("   ", subcenter_flag, "- bloque", k, "de", length(bloques), format(Sys.time(), "(%H:%M)"), "\n")

    it <- r5r::detailed_itineraries(
      r5r_network        = r5r_network,
      origins            = origenes[bloques[[k]], ],
      destinations       = destinos,
      mode               = mode,
      departure_datetime = departure_datetime,
      max_trip_duration  = max_trip_duration,
      all_to_all         = TRUE,
      shortest_path      = TRUE,
      drop_geometry      = TRUE,
      n_threads          = n_threads,
      verbose            = FALSE,
      progress           = FALSE
    ) |> as.data.frame()

    if (nrow(it) == 0) {
      if (!is.null(checkpoint_dir)) saveRDS(NULL, archivo_bloque(k))  # bloque sin rutas: también cuenta como hecho
      next
    }

    if (!all(c("total_distance", "total_duration") %in% names(it))) {
      stop("detailed_itineraries() no devolvió total_distance/total_duration. Columnas recibidas: ",
           paste(names(it), collapse = ", "))
    }

    ## total_distance y total_duration se repiten en cada segmento del
    ## itinerario 
    resumen_bloques[[k]] <- it |>
      distinct(from_id, to_id, .keep_all = TRUE) |>
      transmute(from_id, to_id, distance_m = total_distance, total_time = total_duration) |>
      left_join(pesos, by = c("to_id" = "id")) |>
      group_by(from_id) |>
      summarise(
        dist_ponderada_m     = sum(distance_m * w) / sum(w),
        tiempo_ponderado_min = sum(total_time * w) / sum(w),
        .groups = "drop"
      )

    if (!is.null(checkpoint_dir)) saveRDS(resumen_bloques[[k]], archivo_bloque(k))
  }

  resumen <- bind_rows(resumen_bloques)
  if (nrow(resumen) == 0) {
    return(list(dist_km = rep(NA_real_, nrow(units_city)), time_min = rep(NA_real_, nrow(units_city))))
  }

  match_idx <- match(origenes_todos$id, resumen$from_id)
  list(
    dist_km  = resumen$dist_ponderada_m[match_idx] / 1000,
    time_min = resumen$tiempo_ponderado_min[match_idx]
  )
}

## --- 4. Loop ciudad por ciudad: recorta red, arma red, calcula, cierra ----
pts_sf$dist_subcenter_Gi_km   <- NA_real_
pts_sf$time_subcenter_Gi_min  <- NA_real_
pts_sf$dist_subcenter_95_km   <- NA_real_
pts_sf$time_subcenter_95_min  <- NA_real_

subcenters$dist_subcenter_Gi_km   <- NA_real_
subcenters$time_subcenter_Gi_min  <- NA_real_
subcenters$dist_subcenter_95_km   <- NA_real_
subcenters$time_subcenter_95_min  <- NA_real_

## Resultados por ciudad: cada ciudad terminada se guarda en
## un .rds en red_dir/resultados. 

## El checkpoint se reutiliza solo si coincide el número de propiedades y de
## AGEB de la ciudad. 

res_dir  <- fs::path(red_dir, "resultados")
fs::dir_create(res_dir)
res_cols <- c("dist_subcenter_Gi_km", "time_subcenter_Gi_min",
              "dist_subcenter_95_km", "time_subcenter_95_min")

## Ciudades que se procesan en esta corrida: SOLO CDMX (84).

orden_ciudades <- 84
cat("Orden de ejecución de ciudades:", orden_ciudades, "\n")

for (city_id in orden_ciudades) {

  cat("\n=== Ciudad", city_id, "===\n")

  idx_prop <- which(pts_sf$City_ID == city_id)
  idx_ageb <- which(subcenters$City_ID == city_id)
  res_file <- fs::path(res_dir, paste0("ciudad_", city_id, ".rds"))

  if (file.exists(res_file)) {
    res_prev <- readRDS(res_file)
    if (identical(res_prev$n_prop, length(idx_prop)) && identical(res_prev$n_ageb, length(idx_ageb))) {
      cat("Ya calculada en una corrida anterior -- se lee de", res_file, "\n")
      m_prop <- match(pts_sf$id[idx_prop], res_prev$prop$id)
      m_ageb <- match(subcenters$id[idx_ageb], res_prev$ageb$id)
      for (col in res_cols) {
        pts_sf[[col]][idx_prop]     <- res_prev$prop[[col]][m_prop]
        subcenters[[col]][idx_ageb] <- res_prev$ageb[[col]][m_ageb]
      }
      next
    }
    cat("El checkpoint de esta ciudad no coincide con los datos actuales -- se recalcula.\n")
  }

  bbox_row <- city_bbox |> filter(City_ID == city_id)
  city_dir <- clip_pbf_ciudad(city_id, bbox_row)

  ## build_network() arma la red routable -- 
  r5r_network <- r5r::build_network(data_path = city_dir, verbose = FALSE)

  ## --- Chequeo de conectividad: una sola vez por ciudad, para todos los
  ## puntos (propiedades + AGEB) contra todos los subcentros (Gi* o top 5%)
  puntos_ciudad <- bind_rows(
    st_drop_geometry(pts_sf[idx_prop, ])     |> transmute(id, lon = longitude, lat = latitude),
    st_drop_geometry(subcenters[idx_ageb, ]) |> transmute(id, lon, lat)
  )
  subcentros_ciudad <- st_drop_geometry(subcenters[idx_ageb, ]) |>
    filter(subcenter_Gi | subcenter_95) |>
    transmute(id, lon, lat)

  conectividad <- chequeo_conectividad(
    puntos      = puntos_ciudad,
    subcentros  = subcentros_ciudad,
    r5r_network = r5r_network,
    archivo     = fs::path(res_dir, paste0("conectividad_ciudad_", city_id, ".rds"))
  )

  ## Carpetas para el guardado por bloque de esta ciudad (se borran al
  ## terminar la ciudad, cuando ya existe su .rds completo)
  bloques_dir  <- fs::path(res_dir, paste0("bloques_ciudad_", city_id))
  bloques_prop <- fs::path(bloques_dir, "propiedades")
  bloques_ageb <- fs::path(bloques_dir, "agebs")

  if (length(idx_prop) > 0) {
    cat(length(idx_prop), "propiedades en esta ciudad...\n")
    res_Gi  <- compute_weighted_subcenter_r5r(pts_sf[idx_prop, ], "longitude", "latitude", subcenters[idx_ageb, ], "subcenter_Gi", conectividad = conectividad, r5r_network = r5r_network, checkpoint_dir = bloques_prop)
    res_95  <- compute_weighted_subcenter_r5r(pts_sf[idx_prop, ], "longitude", "latitude", subcenters[idx_ageb, ], "subcenter_95", conectividad = conectividad, r5r_network = r5r_network, checkpoint_dir = bloques_prop)
    pts_sf$dist_subcenter_Gi_km[idx_prop]  <- res_Gi$dist_km
    pts_sf$time_subcenter_Gi_min[idx_prop] <- res_Gi$time_min
    pts_sf$dist_subcenter_95_km[idx_prop]  <- res_95$dist_km
    pts_sf$time_subcenter_95_min[idx_prop] <- res_95$time_min
  }

  if (length(idx_ageb) > 0) {
    cat(length(idx_ageb), "AGEB en esta ciudad...\n")
    res_Gi_a <- compute_weighted_subcenter_r5r(subcenters[idx_ageb, ], "lon", "lat", subcenters[idx_ageb, ], "subcenter_Gi", conectividad = conectividad, r5r_network = r5r_network, checkpoint_dir = bloques_ageb)
    res_95_a <- compute_weighted_subcenter_r5r(subcenters[idx_ageb, ], "lon", "lat", subcenters[idx_ageb, ], "subcenter_95", conectividad = conectividad, r5r_network = r5r_network, checkpoint_dir = bloques_ageb)
    subcenters$dist_subcenter_Gi_km[idx_ageb]  <- res_Gi_a$dist_km
    subcenters$time_subcenter_Gi_min[idx_ageb] <- res_Gi_a$time_min
    subcenters$dist_subcenter_95_km[idx_ageb]  <- res_95_a$dist_km
    subcenters$time_subcenter_95_min[idx_ageb] <- res_95_a$time_min
  }

  r5r::stop_r5(r5r_network)  # libera la red de esta ciudad antes de pasar a la siguiente
  rm(r5r_network)
  gc()

  ## --- Guarda el checkpoint de la ciudad
  saveRDS(
    list(
      n_prop = length(idx_prop),
      n_ageb = length(idx_ageb),
      prop   = st_drop_geometry(pts_sf[idx_prop, ])[, c("id", res_cols)],
      ageb   = st_drop_geometry(subcenters[idx_ageb, ])[, c("id", res_cols)]
    ),
    res_file
  )
  cat("Ciudad", city_id, "guardada en", res_file, "\n")

  ## Con la ciudad completa guardada, los bloques individuales ya no hacen falta
  if (dir.exists(bloques_dir)) fs::dir_delete(bloques_dir)
  archivo_con <- fs::path(res_dir, paste0("conectividad_ciudad_", city_id, ".rds"))
  if (file.exists(archivo_con)) file.remove(archivo_con)
}

## --- 5. Filtro de propiedades mal geocodificadas + exportar (solo CDMX) --
## Las propiedades fuera de la red quedan con NA (el filtro las descarta) y
## las que tengan distancias >= 100 km también.
pts_out <- pts_sf |>
  filter(City_ID %in% orden_ciudades) |>
  filter(dist_subcenter_Gi_km < 100, dist_subcenter_95_km < 100)
agebs_out <- subcenters |> filter(City_ID %in% orden_ciudades)

cat("Propiedades de CDMX con distancia:", nrow(pts_out), "de",
    sum(pts_sf$City_ID %in% orden_ciudades), "\n")

#Salvar csvs de distancias a subcentros de CDMX (red vial, r5r)
archivo_prop <- file.path(housing_dir, "distance_subcenters_properties_r5r_cdmx.csv")
archivo_ageb <- file.path(housing_dir, "distance_subcenters_agebs_r5r_cdmx.csv")
write_excel_csv(st_drop_geometry(pts_out)   |> select(-id), archivo_prop)
write_excel_csv(st_drop_geometry(agebs_out) |> select(-id), archivo_ageb)

cat("\nListo:\n")
cat(" -", archivo_prop, "\n")
cat(" -", archivo_ageb, "\n")