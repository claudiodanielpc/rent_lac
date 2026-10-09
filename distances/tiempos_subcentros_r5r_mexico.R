# ============================================================
# Tiempo de viaje en auto (red vial, r5r) de cada píxel de vivienda
# a los subcentros de empleo de su ciudad
#
# Salida (en la carpeta de datos):
#   - tiempos_pixel_subcentro_largo_<direccion>.csv :
#       un renglón por píxel x subcentro (pares con ruta)
# ============================================================

## Memoria para Java (r5r). Tiene que ir ANTES de cargar r5r.
## Ajustar a ~60-70% de la RAM de la computadora.
options(java.parameters = "-Xmx8G")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, tidyr, fs, r5r, data.table)

rJava::.jinit()
rt <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
cat("Memoria máxima de Java:", round(rJava::.jcall(rt, "J", "maxMemory") / 1e9, 1), "GB\n")

# ============================================================
# Parámetros
# ============================================================

## Paralelización: r5r reparte las búsquedas entre varios núcleos del
## procesador. Se deja un núcleo libre para que la computadora siga usable.
n_threads <- max(1L, parallel::detectCores() - 1L)
cat("Núcleos usados por r5r:", n_threads, "\n")

modo              <- "CAR"
max_trip_duration <- 180L   # minutos; más allá se considera "sin ruta"
departure_dt      <- as.POSIXct("2026-10-06 08:00:00", tz = "America/Mexico_City")

## Dirección del viaje:
##   "subcentro_a_pixel" -> rápido (minutos): una búsqueda por subcentro.
##   "pixel_a_subcentro" -> dirección exacta del viaje casa -> trabajo, pero
##                          una búsqueda por píxel 
direccion <- "pixel_a_subcentro"
stopifnot(direccion %in% c("subcentro_a_pixel", "pixel_a_subcentro"))
cat("Dirección del cálculo:", direccion, "\n")

## Solo para "pixel_a_subcentro": píxeles por bloque. Cada bloque se guarda
## en disco al terminar (si la sesión se cae, se retoma desde ahí).
bloque_pixeles <- 250L

## Solo para "pixel_a_subcentro": núcleos para r5r. 
n_threads_pixel <- min(4L, n_threads)


# ============================================================
# Rutas
# ============================================================
housing_dir <- fs::path("~/OneDrive - WBG",
                        "Giuliana De Mendiola Ramirez's files - Housing report", "Data")
if (!dir.exists(housing_dir)) {
  housing_dir <- fs::path("G:/Mi unidad/consultoria/world_bank")
}
datos_dir <- fs::path(housing_dir, "distances")  

## Redes viales por ciudad
red_dir <- tools::R_user_dir("r5r_mexico", which = "cache")

## Resultados parciales por ciudad (si la sesión se cae, no se repite lo hecho)
res_dir <- fs::path(red_dir, paste0("resultados_tiempos_", direccion))
fs::dir_create(res_dir)

# ============================================================
# 1. Datos
# ============================================================
subcentros <- read_csv(fs::path(datos_dir, "emp_5k.csv"), show_col_types = FALSE) |>
  mutate(id = paste0("sc_", City_ID, "_", cluster_id))

## pixel_id como texto: son números grandes y, como número, R podría
## escribirlos en notación científica (2e+09) y romper los cruces
pixels_raw <- read_csv(fs::path(datos_dir, "pixel_prices.csv"), show_col_types = FALSE,
                       col_types = cols(pixel_id = col_character()))

## City -> City_ID usando la tabla de subcentros
ciudades <- subcentros |> distinct(City_ID, City)

pixels_raw <- pixels_raw |> left_join(ciudades, by = "City")
if (any(is.na(pixels_raw$City_ID))) {
  stop("Hay píxeles con un nombre de ciudad que no aparece en emp_5k.csv: ",
       paste(unique(pixels_raw$City[is.na(pixels_raw$City_ID)]), collapse = ", "))
}

## Un mismo píxel aparece varias veces (casa/departamento, venta/renta), pero
## la ubicación es la misma: se rutea UNA vez por píxel.
pixels <- pixels_raw |>
  distinct(City_ID, pixel_id, longitude, latitude) |>
  mutate(id = paste0("px_", City_ID, "_", pixel_id))

cat("\nFilas en pixel_prices:", nrow(pixels_raw),
    "| píxeles únicos a rutear:", nrow(pixels),
    "| subcentros:", nrow(subcentros), "\n")
print(count(pixels, City_ID, name = "pixeles"))

# ============================================================
# 2. Tiempos por ciudad
# ============================================================
## r5r::travel_time_matrix() hace UNA búsqueda por cada ORIGEN y en esa
## búsqueda encuentra el tiempo a TODOS los destinos.
##   - "subcentro_a_pixel": subcentros como origen -> unas cuantas búsquedas
##     por ciudad (minutos).
##   - "pixel_a_subcentro": píxeles como origen -> una búsqueda por píxel
##     (horas), pero es la dirección exacta del viaje casa -> trabajo.


tiempos_ciudad <- function(city_id) {

  archivo <- fs::path(res_dir, paste0("tiempos_ciudad_", city_id, ".rds"))
  if (file.exists(archivo)) {
    cat("  ya calculada en una corrida anterior -- se lee de disco\n")
    return(readRDS(archivo))
  }

  city_dir <- fs::path(red_dir, paste0("ciudad_", city_id))
  if (!file.exists(fs::path(city_dir, "network.dat"))) {
    stop("No existe la red vial de la ciudad ", city_id, " en ", city_dir,
         " -- primero hay que armarla con distances_r5r.R.")
  }

  r5r_network <- r5r::build_network(data_path = city_dir, verbose = FALSE)
  on.exit({ r5r::stop_r5(r5r_network); gc() }, add = TRUE)

  subc <- subcentros |> filter(City_ID == city_id) |> select(id, lon, lat)
  pix  <- pixels     |> filter(City_ID == city_id) |> select(id, lon = longitude, lat = latitude)

  cat("  ", nrow(subc), "subcentros x", nrow(pix), "píxeles\n")

  ## Llama a r5r y deja columnas estándar: subcentro, pixel, tiempo_min
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
    ## el nombre de la columna de tiempo cambia según la versión de r5r
    col_t <- grep("^travel_time", names(ttm), value = TRUE)[1]
    ttm |> transmute(from_id, to_id, tiempo_min = .data[[col_t]])
  }

  if (direccion == "subcentro_a_pixel") {

    res <- ttm_std(subc, pix, progreso = TRUE) |>
      transmute(subcentro = from_id, pixel = to_id, tiempo_min)

  } else {

    ## Píxeles como origen, en bloques con guardado en disco y estimación
    ## del tiempo restante
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

      ## Si Java se queda sin memoria, se libera memoria y se reintenta el
      ## MISMO bloque con la mitad de núcleos (hasta 1)
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
      partes[[k]] <- out |>
        transmute(subcentro = to_id, pixel = from_id, tiempo_min)
      saveRDS(partes[[k]], archivo_k)

      n_hechos_ahora <- n_hechos_ahora + 1L
      min_por_bloque <- as.numeric(difftime(Sys.time(), t_city, units = "mins")) / n_hechos_ahora
      faltan         <- length(bloques) - k
      cat(sprintf("   bloque %d de %d (%s) | %.1f min por bloque | faltan ~%.0f min\n",
                  k, length(bloques), format(Sys.time(), "%H:%M"),
                  min_por_bloque, faltan * min_por_bloque))
    }

    res <- bind_rows(partes)
  }

  saveRDS(res, archivo)
  if (direccion == "pixel_a_subcentro") fs::dir_delete(fs::path(res_dir, paste0("bloques_ciudad_", city_id)))
  res
}

## Orden: Ciudad de México (84) al final, por ser la más pesada. Así, si se
## cae ahí, las demás ciudades ya quedaron guardadas.
ids_ciudades <- sort(unique(pixels$City_ID))
ids_ciudades <- c(setdiff(ids_ciudades, 84), intersect(ids_ciudades, 84))
cat("Orden de ejecución:", ids_ciudades, "\n")
t_inicio     <- Sys.time()
lista        <- vector("list", length(ids_ciudades))

for (i in seq_along(ids_ciudades)) {
  cid <- ids_ciudades[i]
  t0  <- Sys.time()
  cat(sprintf("\n[%d/%d] Ciudad %s (%s) -- %s\n", i, length(ids_ciudades), cid,
              ciudades$City[ciudades$City_ID == cid], format(t0, "%H:%M:%S")))

  lista[[i]] <- tiempos_ciudad(cid)

  cat(sprintf("  listo en %.1f min | %d pares con ruta | avance total %.1f min\n",
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              nrow(lista[[i]]),
              as.numeric(difftime(Sys.time(), t_inicio, units = "mins"))))
}

# ============================================================
# 3. Tabla larga píxel x subcentro y exportación
# ============================================================
largo <- bind_rows(lista) |>
  left_join(subcentros |> select(subcentro = id, City_ID, cluster_id, emp), by = "subcentro") |>
  mutate(pixel_id = sub("^px_\\d+_", "", pixel)) |>
  select(City_ID, pixel_id, cluster_id, emp, tiempo_min)

cat("\n=== Píxeles sin ruta a ningún subcentro (fuera de la red o mal ubicados) ===\n")
con_ruta <- largo |>
  filter(!is.na(tiempo_min)) |>
  distinct(City_ID, pixel_id) |>
  mutate(con_ruta = TRUE)
print(
  pixels |>
    distinct(City_ID, pixel_id) |>
    left_join(con_ruta, by = c("City_ID", "pixel_id")) |>
    left_join(ciudades, by = "City_ID") |>
    group_by(City) |>
    summarise(pixeles = n(), sin_ruta = sum(is.na(con_ruta)),
              pct_sin_ruta = round(100 * sin_ruta / pixeles, 1), .groups = "drop")
)

archivo_largo <- fs::path(datos_dir, paste0("tiempos_pixel_subcentro_largo_", direccion, ".csv"))
write_excel_csv(largo, archivo_largo)

cat(sprintf("\nListo en %.1f min:\n - %s\n",
            as.numeric(difftime(Sys.time(), t_inicio, units = "mins")),
            archivo_largo))
