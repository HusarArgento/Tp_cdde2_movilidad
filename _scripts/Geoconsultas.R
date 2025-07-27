library(osmdata)
library(tidyverse)
library(sf)
library(dodgr)


# Es una consulta a la base de datos de OpenStreetMap
argentina <- opq("Argentina") %>%  #  buscar dentro del área de Argentina
  add_osm_feature(key = "admin_level", value = "4") %>% #buscar provincias (nivel administrativo 4)
  osmdata_sf()   #= convertir a formato espacial


# PASO 1: Definir el área de Buenos Aires
# bbox = bounding box (caja que delimita el área)
# Formato: c(oeste, sur, este, norte) en coordenadas
bbox_bsas <- c(-58.5, -34.7, -58.3, -34.5)

# PASO 2: Crear la consulta para esa área
query_bsas <- opq(bbox_bsas)

# PASO 3: Ahora sí podemos explorar qué hay disponible
print("Área definida para Buenos Aires")
print(paste("Coordenadas:", paste(bbox_bsas, collapse = ", ")))

print(query_bsas)



hospitales <- query_bsas %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()
print(paste("Hospitales encontrados:", nrow(hospitales$osm_points)))
colnames(hospitales$osm_points)
print(hospitales$osm_points[1:5, c("name", "addr:street", "emergency")])
print(hospitales$osm_points[1:5, c("name", "operator", "healthcare")])


# Hospitales solo con nombre
hospitales_con_nombre <- hospitales$osm_points[!is.na(hospitales$osm_points$name), ]

# Ver cuántos quedan
print(paste("Hospitales CON nombre:", nrow(hospitales_con_nombre)))

# Ver los primeros 5
print(hospitales_con_nombre[1:27, c("name", "addr:street")])


bb <- getbb("Ciudad Autónoma de Buenos Aires")
print(bb)
graph <- dodgr_streetnet(bb, expand = 0.05)
foot <- weight_streetnet(graph, wt_profile = "foot")


#OSRM

# Usa el servidor público de OSRM - GRATIS
tiempo_caminando <- osrmTable(
  src = barrios_puntos,
  dst = educ_puntos,
  osrm.profile = "foot",
  osrm.server = "https://router.project-osrm.org/"  # servidor público
)


# Test con solo 2 barrios y 2 escuelas
test_barrios <- barrios_puntos[1:2,]
test_escuelas <- educ_puntos[1:2,]

tiempo_test <- osrmTable(
  src = test_barrios,
  dst = test_escuelas,
  osrm.profile = "foot"
)



library(openrouteservice)

# Configurar tu API key
ors_api_key("TU_API_KEY_AQUI")

# IMPORTANTE: Usar ors_matrix (no otras funciones)
# Matrix V2 permite máximo 50x50 ubicaciones por request

# Test con pocos datos primero
test_barrios <- barrios_puntos[1:2,]
test_escuelas <- educ_puntos[1:5,]

# Combinar coordenadas (barrios primero, luego escuelas)
coords_combinadas <- rbind(
  st_coordinates(test_barrios),
  st_coordinates(test_escuelas)
)

# Test básico
test_matriz <- ors_matrix(
  locations = coords_combinadas,
  profile = "foot-walking",
  metrics = c("duration"),
  units = "m"
)

print(test_matriz)



# Test con coordenadas en WGS84
test_barrios_wgs84 <- barrios_wgs84[1:1,]
test_escuelas_wgs84 <- escuelas_wgs84[1:3,]

# Combinar coordenadas (ahora deberían estar en formato lon/lat)
coords_wgs84 <- rbind(
  st_coordinates(test_barrios_wgs84),
  st_coordinates(test_escuelas_wgs84)
)

print("Coordenadas en WGS84 (deberían ser ~-58, -34):")
print(coords_wgs84)

# Test con OpenRouteService
matriz_wgs84 <- ors_matrix(
  locations = coords_wgs84,
  profile = "foot-walking",
  metrics = "duration"
)

print("¡Funciona! Matriz de tiempos:")
print(matriz_wgs84$durations)

# Tiempos a escuelas
tiempos_escuelas <- matriz_wgs84$durations[1, -1]
print(paste("Tiempos a escuelas:", paste(round(tiempos_escuelas/60, 1), collapse = ", "), "minutos"))


# Función para procesar todos los barrios
calcular_accesibilidad_completa <- function(barrios_wgs84, escuelas_wgs84, max_escuelas = 20) {

  n_barrios <- nrow(barrios_wgs84)

  resultados <- data.frame(
    barrio_id = 1:n_barrios,
    tiempo_escuela_cercana_min = NA,
    tiempo_escuela_cercana_seg = NA,
    num_escuelas_evaluadas = NA
  )

  for(i in 1:n_barrios) {

    cat("Procesando barrio", i, "de", n_barrios, "\n")

    tryCatch({

      # Tomar hasta 20 escuelas para no exceder límites
      n_escuelas <- min(max_escuelas, nrow(escuelas_wgs84))

      # Combinar coordenadas: 1 barrio + N escuelas
      coords <- rbind(
        st_coordinates(barrios_wgs84[i,]),
        st_coordinates(escuelas_wgs84[1:n_escuelas,])
      )

      # Calcular matriz
      matriz <- ors_matrix(
        locations = coords,
        profile = "foot-walking",
        metrics = "duration"
      )

      # Extraer tiempos a escuelas (fila 1, columnas 2 en adelante)
      tiempos <- matriz$durations[1, -1]

      # Encontrar el mínimo
      tiempo_min_seg <- min(tiempos, na.rm = TRUE)
      tiempo_min_min <- tiempo_min_seg / 60

      # Guardar resultados
      resultados$tiempo_escuela_cercana_seg[i] <- tiempo_min_seg
      resultados$tiempo_escuela_cercana_min[i] <- tiempo_min_min
      resultados$num_escuelas_evaluadas[i] <- n_escuelas

      # Pausa para respetar rate limits (40 per minute = 1.5 seg)
      Sys.sleep(2)

    }, error = function(e) {
      cat("Error en barrio", i, ":", e$message, "\n")
    })
  }

  return(resultados)
}

# Ejecutar análisis completo
accesibilidad_final <- calcular_accesibilidad_completa(barrios_wgs84, escuelas_wgs84)

# Ver resultados
head(accesibilidad_final)
summary(accesibilidad_final$tiempo_escuela_cercana_min)

# Estadísticas básicas
cat("Tiempo promedio a escuela más cercana:",
    round(mean(accesibilidad_final$tiempo_escuela_cercana_min, na.rm = TRUE), 1),
    "minutos\n")
