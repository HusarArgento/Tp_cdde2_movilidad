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
