library(sf)
library(osmdata)
library(ggplot2)
library(tidyverse)

gye_1 = opq(bbox = c( -79.9174,-2.1748, -79.8757 ,-2.1425)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() 

gye_2 = opq(bbox = c( -79.9174,-2.1748, -79.8757 ,-2.1425)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() 

gye_3 = opq(bbox = c( -79.9174,-2.1748, -79.8757 ,-2.1425)) %>%
  add_osm_feature(key = "aeroway") %>%
  osmdata_sf() 

building = opq(bbox = c( -79.9174,-2.1748, -79.8757 ,-2.1425)) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf() 

tienda = opq(bbox = c( -79.9174,-2.1748, -79.8757 ,-2.1425)) %>%
  add_osm_feature(key = "shop") %>%
  osmdata_sf() 

gye_vias = gye_1$osm_lines %>% select(highway) %>% st_transform(4326)
gye_park = gye_2$osm_polygons %>% select(leisure) %>% st_transform(4326)
gye_aero = gye_3$osm_polygons %>% select(aeroway) %>% st_transform(4326)
gye_edificios = building$osm_polygons %>% select(name) %>% st_transform(4326)
tiendas_p = tienda$osm_points %>% select(name) %>% st_transform(4326)
tiendas_p = tiendas_p[!is.na(tiendas_p$name),]

edificios_p = st_centroid(gye_edificios)

ggplot() + 
  geom_sf(data = gye_vias, col = "black", size= 0.1) + 
  geom_sf(data = gye_park, fill = "darkgreen")+
  geom_sf(data = gye_aero, fill = "darkgrey")+
  geom_sf(data = gye_edificios, fill = "orange")+
  geom_sf(data = tiendas_p, col= "red", size =5, alpha = 0.4)+
  coord_sf(xlim = c(-79.9174, -79.8757) , ylim = c(-2.1748, -2.1425))+
  theme_classic()


