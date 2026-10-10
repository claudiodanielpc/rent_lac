# ============================================================
# BRASIL -- Tiempo de viaje en auto (red vial, r5r) de cada píxel de
# vivienda a los subcentros de empleo de su ciudad
#
#
# Tiempos en la dirección del viaje casa -> trabajo (píxel -> subcentro).
#
# ============================================================

## Memoria para Java (r5r). Tiene que ir ANTES de cargar r5r.
## Ajustar a ~60-70% de la RAM de la computadora.
options(java.parameters = "-Xmx8G")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, tidyr, tibble, fs, r5r, data.table)

rJava::.jinit()
rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
cat("Memoria máxima de Java:", round(rJava::.jcall(rt, "J", "maxMemory") / 1e9, 1), "GB\n")

# ============================================================
# Parámetros
# ============================================================

## Paralelización: r5r reparte las búsquedas entre varios núcleos.
n_threads <- max(1L, parallel::detectCores() - 1L)
cat("Núcleos disponibles para r5r:", n_threads, "\n")

modo              <- "CAR"
max_trip_duration <- 180L   # minutos; más allá se considera "sin ruta"
departure_dt      <- as.POSIXct("2026-10-06 08:00:00", tz = "America/Sao_Paulo")

## Píxeles por bloque (cada bloque se guarda en disco) y núcleos.

bloque_pixeles  <- 250L
n_threads_pixel <- min(4L, n_threads)

## Peso de cada subcentro en el promedio ponderado.
col_peso <- "n_SETOR"

## Subcentros más lejos que esto (km, en línea recta) del centro de los
## píxeles de su ciudad se descartan por estar probablemente mal ubicados.
max_dist_subc_km <- 150

## Margen alrededor de cada ciudad para recortar la red (grados; 0.3 ~ 30 km)
buffer_deg <- 0.3

## Orden: São Paulo (19) y Rio de Janeiro (16) al final, por ser las más
## pesadas. 
ciudades_al_final <- c(16, 19)

# ============================================================
# Rutas
# ============================================================
housing_dir <- fs::path("~/OneDrive - WBG",
                        "Giuliana De Mendiola Ramirez's files - Housing report", "Data")
if (!dir.exists(housing_dir)) {
  housing_dir <- fs::path("G:/Mi unidad/consultoria/world_bank")
}
datos_dir <- fs::path(housing_dir, "distances")


red_dir <- tools::R_user_dir("r5r_brasil", which = "cache")
fs::dir_create(red_dir)

res_dir <- fs::path(red_dir, "resultados_tiempos_pixel_a_subcentro")
fs::dir_create(res_dir)

## Osmosis 
osmosis_bat <- "C:\\Users\\claud\\Documents\\osmosis-0.49.2\\bin\\osmosis.bat"  # <- AJUSTAR si cambia
if (!file.exists(osmosis_bat)) stop("No se encuentra Osmosis en ", osmosis_bat)

# ============================================================
# 1. Datos
# ============================================================
subcentros <- read_csv(fs::path(datos_dir, "BRA_emp_5k.csv"), show_col_types = FALSE) |>
  mutate(id = paste0("sc_", City_ID, "_", cluster_id))

if (is.null(col_peso)) {
  subcentros$peso <- 1
} else {
  if (!col_peso %in% names(subcentros)) stop("No existe la columna ", col_peso, " en BRA_emp_5k.csv")
  subcentros$peso <- subcentros[[col_peso]]
}
cat("Peso de los subcentros:", if (is.null(col_peso)) "ninguno (promedio simple)" else col_peso, "\n")

## pixel_id como texto
pixels_raw <- read_csv(fs::path(datos_dir, "BRA_pixel_prices.csv"), show_col_types = FALSE,
                       col_types = cols(pixel_id = col_character()))

## Nombres de columnas en minúscula y sin espacios, y coordenadas con nombre estándar
names(pixels_raw) <- tolower(trimws(names(pixels_raw)))
alias_lon <- c("longitude", "lon", "long", "lng", "x")
alias_lat <- c("latitude", "lat", "y")
col_lon <- intersect(alias_lon, names(pixels_raw))[1]
col_lat <- intersect(alias_lat, names(pixels_raw))[1]
if (is.na(col_lon) || is.na(col_lat) || !"city" %in% names(pixels_raw)) {
  stop("BRA_pixel_prices.csv no trae columnas de ciudad/coordenadas reconocibles. Columnas: ",
       paste(names(pixels_raw), collapse = ", "))
}
pixels_raw <- pixels_raw |> rename(longitude = all_of(col_lon), latitude = all_of(col_lat))
pixels_raw <- pixels_raw |> mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude))

## city -> City_ID usando la tabla de subcentros
ciudades <- subcentros |> distinct(City_ID, city)
pixels_raw <- pixels_raw |> left_join(ciudades, by = "city")
if (any(is.na(pixels_raw$City_ID))) {
  stop("Hay píxeles con una ciudad que no aparece en BRA_emp_5k.csv: ",
       paste(unique(pixels_raw$city[is.na(pixels_raw$City_ID)]), collapse = ", "))
}

## Un píxel aparece varias veces (casa/depto, venta/renta): se rutea UNA vez
pixels <- pixels_raw |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  distinct(City_ID, pixel_id, longitude, latitude) |>
  mutate(id = paste0("px_", City_ID, "_", pixel_id))

cat("\nFilas en BRA_pixel_prices:", nrow(pixels_raw),
    "| píxeles únicos a rutear:", nrow(pixels),
    "| subcentros:", nrow(subcentros), "\n")

# ============================================================
# 2. Revisión de subcentros mal ubicados
# ============================================================
## Centro de cada ciudad = mediana de las coordenadas de sus píxeles
centro_ciudad <- pixels |>
  group_by(City_ID) |>
  summarise(lon_c = median(longitude), lat_c = median(latitude), .groups = "drop")

dist_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
       cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  6371 * 2 * asin(sqrt(a))
}

subcentros <- subcentros |>
  left_join(centro_ciudad, by = "City_ID") |>
  mutate(dist_centro_km = dist_km(lon, lat, lon_c, lat_c))

descartados <- subcentros |> filter(dist_centro_km > max_dist_subc_km)
if (nrow(descartados) > 0) {
  cat("\n*** AVISO: subcentros a más de", max_dist_subc_km,
      "km del centro de su ciudad -- se DESCARTAN (revisar con Olivia):\n")
  print(descartados |> select(City_ID, city, cluster_id, lon, lat, dist_centro_km))
}
subcentros <- subcentros |> filter(dist_centro_km <= max_dist_subc_km)

## Ciudades que se quedaron sin subcentros no se pueden calcular
sin_subc <- setdiff(unique(pixels$City_ID), unique(subcentros$City_ID))
if (length(sin_subc) > 0) {
  cat("\n*** AVISO: ciudades sin subcentros válidos (sus píxeles quedan en NA):",
      paste(ciudades$city[ciudades$City_ID %in% sin_subc], collapse = ", "), "\n")
}

# ============================================================
# 3. Red vial: OSM por región + recorte por ciudad
# ============================================================
## Geofabrik divide Brasil en 5 regiones; se descarga solo la que hace falta
region_ciudad <- tribble(
  ~city,             ~region_osm,
  "Belo Horizonte",  "sudeste",
  "Rio de Janeiro",  "sudeste",
  "São Paulo",       "sudeste",
  "Vitória",         "sudeste",
  "Curitiba",        "sul",
  "Florianópolis",   "sul",
  "Porto Alegre",    "sul",
  "Fortaleza",       "nordeste",
  "João Pessoa",     "nordeste",
  "Maceió",          "nordeste",
  "Natal",           "nordeste",
  "Recife",          "nordeste",
  "Salvador",        "nordeste",
  "São Luís",        "nordeste",
  "Teresina",        "nordeste",
  "Belém",           "norte",
  "Manaus",          "norte",
  "Brasília",        "centro-oeste",
  "Campo Grande",    "centro-oeste",
  "Cuiabá",          "centro-oeste",
  "Goiânia",         "centro-oeste"
)
ciudades <- ciudades |> left_join(region_ciudad, by = "city")
if (any(is.na(ciudades$region_osm))) {
  stop("Falta la región OSM de: ", paste(ciudades$city[is.na(ciudades$region_osm)], collapse = ", "),
       " -- agregarla a region_ciudad.")
}

descargar_region <- function(region) {
  pbf <- fs::path(red_dir, paste0(region, "-latest.osm.pbf"))
  if (!file.exists(pbf)) {
    url <- paste0("https://download.geofabrik.de/south-america/brazil/", region, "-latest.osm.pbf")
    cat("Descargando red vial de la región", region, "...\n")
    old <- getOption("timeout"); options(timeout = 7200)
    download.file(url, pbf, mode = "wb", method = "libcurl")
    options(timeout = old)
  }
  pbf
}

## Bounding box por ciudad: píxeles entre el percentil 1 y 99 (para que un
## píxel mal ubicado no estire la caja) + subcentros válidos + margen
pct <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
city_bbox <- bind_rows(
  pixels |>
    group_by(City_ID) |>
    summarise(xmin = pct(longitude, 0.01), xmax = pct(longitude, 0.99),
              ymin = pct(latitude, 0.01),  ymax = pct(latitude, 0.99), .groups = "drop"),
  subcentros |>
    group_by(City_ID) |>
    summarise(xmin = min(lon), xmax = max(lon), ymin = min(lat), ymax = max(lat), .groups = "drop")
) |>
  group_by(City_ID) |>
  summarise(xmin = min(xmin) - buffer_deg, xmax = max(xmax) + buffer_deg,
            ymin = min(ymin) - buffer_deg, ymax = max(ymax) + buffer_deg, .groups = "drop") |>
  mutate(area_km2 = (xmax - xmin) * 111 * cos(((ymin + ymax) / 2) * pi / 180) * (ymax - ymin) * 111)

cat("\n=== Bounding box por ciudad ===\n")
print(city_bbox |> left_join(ciudades, by = "City_ID") |> select(City_ID, city, area_km2))
if (any(city_bbox$area_km2 > 100000)) {
  stop("Hay un bbox de ciudad sospechosamente grande (> 100,000 km2) -- revisar arriba.")
}

clip_pbf_ciudad <- function(city_id) {
  bbox_row <- city_bbox |> filter(City_ID == city_id)
  region   <- ciudades$region_osm[ciudades$City_ID == city_id]
  city_dir <- fs::path(red_dir, paste0("ciudad_", city_id))
  city_pbf <- fs::path(city_dir, "red.osm.pbf")
  bbox_txt <- fs::path(city_dir, "bbox.txt")
  bbox_str <- sprintf("%.6f,%.6f,%.6f,%.6f", bbox_row$xmin, bbox_row$ymin, bbox_row$xmax, bbox_row$ymax)

  ## Si existe un recorte con otro bbox, se borra y se rehace
  if (dir.exists(city_dir)) {
    previo <- if (file.exists(bbox_txt)) readLines(bbox_txt, warn = FALSE) else ""
    if (!file.exists(city_pbf) || !identical(previo, bbox_str)) fs::dir_delete(city_dir)
  }
  fs::dir_create(city_dir)

  if (!file.exists(city_pbf)) {
    region_pbf <- descargar_region(region)
    win <- .Platform$OS.type == "windows"
    osmosis_run  <- if (win) utils::shortPathName(osmosis_bat) else osmosis_bat
    region_run   <- if (win) utils::shortPathName(region_pbf)  else region_pbf
    city_dir_run <- if (win) utils::shortPathName(city_dir)    else city_dir

    cat("  recortando red vial (región", region, ")...\n")
    salida <- system2(
      osmosis_run,
      args = c("--read-pbf", region_run,
               "--bounding-box",
               sprintf("left=%f", bbox_row$xmin), sprintf("bottom=%f", bbox_row$ymin),
               sprintf("right=%f", bbox_row$xmax), sprintf("top=%f", bbox_row$ymax),
               "clipIncompleteEntities=true",
               "--write-pbf", file.path(city_dir_run, "red.osm.pbf")),
      stdout = TRUE, stderr = TRUE
    )
    if (!file.exists(city_pbf)) {
      cat(paste(salida, collapse = "\n"), "\n")
      stop("Osmosis no generó ", city_pbf)
    }
    writeLines(bbox_str, bbox_txt)
  }
  city_dir
}

# ============================================================
# 4. Tiempos por ciudad
# ============================================================
tiempos_ciudad <- function(city_id) {

  archivo <- fs::path(res_dir, paste0("tiempos_ciudad_", city_id, ".rds"))
  if (file.exists(archivo)) {
    cat("  ya calculada en una corrida anterior -- se lee de disco\n")
    return(readRDS(archivo))
  }

  subc <- subcentros |> filter(City_ID == city_id) |> select(id, lon, lat)
  pix  <- pixels     |> filter(City_ID == city_id) |> select(id, lon = longitude, lat = latitude)

  ## Solo se rutean píxeles dentro del bbox de la ciudad 
  bb  <- city_bbox |> filter(City_ID == city_id)
  pix <- pix |> filter(lon >= bb$xmin, lon <= bb$xmax, lat >= bb$ymin, lat <= bb$ymax)

  cat("  ", nrow(subc), "subcentros x", nrow(pix), "píxeles\n")

  city_dir    <- clip_pbf_ciudad(city_id)
  r5r_network <- r5r::build_network(data_path = city_dir, verbose = FALSE)
  on.exit({ r5r::stop_r5(r5r_network); gc() }, add = TRUE)

  ttm_std <- function(origins, destinations, progreso, hilos = n_threads) {
    ttm <- r5r::travel_time_matrix(
      r5r_network        = r5r_network,
      origins            = origins,
      destinations       = destinations,
      mode               = modo,
      departure_datetime = departure_dt,
      max_trip_duration  = max_trip_duration,
      n_threads          = hilos,
      verbose            = FALSE,
      progress           = progreso
    ) |> as.data.frame()
    col_t <- grep("^travel_time", names(ttm), value = TRUE)[1]
    ttm |> transmute(from_id, to_id, tiempo_min = .data[[col_t]])
  }

  ## Píxeles como origen 
  {
    bloques_dir <- fs::path(res_dir, paste0("bloques_ciudad_", city_id))
    fs::dir_create(bloques_dir)
    bloques <- split(seq_len(nrow(pix)), ceiling(seq_len(nrow(pix)) / bloque_pixeles))
    partes  <- vector("list", length(bloques))
    t_city  <- Sys.time()
    n_hechos_ahora <- 0L
    hilos_actuales <- n_threads_pixel

    for (k in seq_along(bloques)) {
      archivo_k <- fs::path(bloques_dir, sprintf("b%d_n%d_k%04d.rds", bloque_pixeles, length(bloques), k))
      if (file.exists(archivo_k)) {
        partes[[k]] <- readRDS(archivo_k)
        next
      }

      ## Si Java se queda sin memoria: liberar y reintentar con la mitad de núcleos
      repeat {
        out <- tryCatch(
          ttm_std(pix[bloques[[k]], ], subc, progreso = FALSE, hilos = hilos_actuales),
          error = function(e) e
        )
        if (!inherits(out, "error")) break
        if (!grepl("OutOfMemory", conditionMessage(out)) || hilos_actuales == 1L) stop(out)
        hilos_actuales <- max(1L, hilos_actuales %/% 2L)
        cat("   ** Java se quedó sin memoria -- se reintenta el bloque", k,
            "con", hilos_actuales, "núcleo(s)\n")
        gc(); rJava::.jcall("java/lang/System", "V", "gc")
      }
      partes[[k]] <- out |> transmute(subcentro = to_id, pixel = from_id, tiempo_min)
      saveRDS(partes[[k]], archivo_k)

      n_hechos_ahora <- n_hechos_ahora + 1L
      min_por_bloque <- as.numeric(difftime(Sys.time(), t_city, units = "mins")) / n_hechos_ahora
      cat(sprintf("   bloque %d de %d (%s) | %.1f min por bloque | faltan ~%.0f min\n",
                  k, length(bloques), format(Sys.time(), "%H:%M"),
                  min_por_bloque, (length(bloques) - k) * min_por_bloque))
    }
    res <- bind_rows(partes)
  }

  saveRDS(res, archivo)
  fs::dir_delete(fs::path(res_dir, paste0("bloques_ciudad_", city_id)))
  res
}

ids_ciudades <- sort(intersect(unique(pixels$City_ID), unique(subcentros$City_ID)))
ids_ciudades <- c(setdiff(ids_ciudades, ciudades_al_final), intersect(ciudades_al_final, ids_ciudades))
cat("\nOrden de ejecución:", ids_ciudades, "\n")

t_inicio <- Sys.time()
lista    <- vector("list", length(ids_ciudades))

for (i in seq_along(ids_ciudades)) {
  cid <- ids_ciudades[i]
  t0  <- Sys.time()
  cat(sprintf("\n[%d/%d] Ciudad %s (%s) -- %s\n", i, length(ids_ciudades), cid,
              ciudades$city[ciudades$City_ID == cid], format(t0, "%H:%M:%S")))

  lista[[i]] <- tiempos_ciudad(cid)

  cat(sprintf("  listo en %.1f min | %d pares con ruta | avance total %.1f min\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              nrow(lista[[i]]),
              as.numeric(difftime(Sys.time(), t_inicio, units = "mins"))))
}

largo <- bind_rows(lista)

# ============================================================
# 5. Resumen por píxel
# ============================================================
largo <- largo |>
  left_join(subcentros |> select(subcentro = id, City_ID, cluster_id, peso), by = "subcentro")

peso_ciudad <- subcentros |> group_by(City_ID) |> summarise(peso_total = sum(peso), .groups = "drop")

resumen <- largo |>
  group_by(pixel) |>
  summarise(
    t_subcentro_cercano_min    = min(tiempo_min),
    cluster_cercano            = cluster_id[which.min(tiempo_min)],
    t_subcentros_ponderado_min = sum(tiempo_min * peso) / sum(peso),
    n_subcentros_alcanzados    = n(),
    peso_alcanzado             = sum(peso),
    City_ID                    = first(City_ID),
    .groups = "drop"
  ) |>
  left_join(peso_ciudad, by = "City_ID") |>
  mutate(
    share_peso_alcanzado = peso_alcanzado / peso_total,
    ## si no se llega a TODOS los subcentros de la ciudad -> NA
    t_subcentros_ponderado_min = if_else(share_peso_alcanzado < 1, NA_real_, t_subcentros_ponderado_min)
  ) |>
  select(pixel, t_subcentro_cercano_min, cluster_cercano,
         t_subcentros_ponderado_min, n_subcentros_alcanzados, share_peso_alcanzado)

ancho <- largo |>
  select(pixel, cluster_id, tiempo_min) |>
  pivot_wider(names_from = cluster_id, values_from = tiempo_min, names_prefix = "t_cluster_")

# ============================================================
# 6. Unir a BRA_pixel_prices y exportar
# ============================================================
salida <- pixels_raw |>
  mutate(pixel = paste0("px_", City_ID, "_", pixel_id)) |>
  left_join(resumen, by = "pixel") |>
  left_join(ancho,   by = "pixel") |>
  select(-pixel)

cat("\n=== Píxeles sin ruta a ningún subcentro (fuera de la red o mal ubicados) ===\n")
print(
  salida |>
    distinct(city, pixel_id, t_subcentro_cercano_min) |>
    group_by(city) |>
    summarise(pixeles = n(), sin_ruta = sum(is.na(t_subcentro_cercano_min)),
              pct_sin_ruta = round(100 * sin_ruta / pixeles, 1), .groups = "drop"),
  n = Inf
)

archivo_salida <- fs::path(datos_dir, "BRA_pixel_prices_tiempos_pixel_a_subcentro.csv")
write_excel_csv(salida, archivo_salida)

cat(sprintf("\nListo en %.1f min:\n - %s\n",
            as.numeric(difftime(Sys.time(), t_inicio, units = "mins")),
            archivo_salida))