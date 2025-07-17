comunas <- st_read(url_comunas)
st_crs(comunas)
sum(!st_is_valid(comunas))


barrios_selec <- barrios_selec %>%
  mutate(NOMBRE = coalesce(NOMBRE, "Villa_31"))
centroides_bar <- st_centroid(barrios_selec)
distancias <- st_distance(centroides_bar, educ)
cercanias <-  st_nearest_feature(centroides_bar, educ)
centroides_bar2 <- centroides_bar %>%
  mutate(escuela_cercana = educ$nam[cercanias])

centroides_bar3 <- centroides_bar2 %>% mutate(distancia_m = as.numeric (st_distance(geometry, educ[cercanias, ], by_element = TRUE)),
                                              distancia_km = distancia_m / 1000
)
#da numeros con unidades sin el as.numeric y segun claude y gpt eso dificulta para operaciones posteriores (preguntar a Pablo)

#Ahora ya tenemos los colegios con las distancias mas cercanas!

#Unimos un join espacial con uno no espacial

# barrios_cmya <- com_ingreso %>% filter(barrios %in% c("Caballito", "Almagro, Boedo", "Recoleta")) %>% st_centroid()
#Seleccionamos barrios peroy como es una geometria multipoligon le hacems centroides (esto no va porque necesito los barrios cmya como multipoligons para leafleet)

barrios_cmym <- com_ingreso %>% filter(barrios %in% c("Caballito", "Almagro, Boedo", "Recoleta"))
barrios_cmya <- st_centroid(barrios_cmym)


cmya_cercanias <-  st_nearest_feature(barrios_cmya, educ)
barrios_cmyaed2 <- barrios_cmya %>% mutate(escuela_cercana = educ$nam[cmya_cercanias],
                                           distancia_m = as.numeric(st_distance(geometry,
                                                                                educ[cmya_cercanias, ],
                                                                                by_element =TRUE)),
                                           distancia_km = as.numeric(st_distance(geometry,                                                                                educ[cmya_cercanias,],
                                                                                 by_element = TRUE)
                                           ) / 1000
)

escuelas_cercanas <- educ %>% filter(nam %in% barrios_cmyaed2$escuela_cercana) #Para Leafleet
escuelas_cercanas2 <- educ %>% filter(nam %in% barrios_pcomp$escuela_cercana)
comparacion_distancias <- bind_rows(centroides_bar3 %>% mutate(niv_socioeco = "Barrio Popular"),
                                    barrios_cmyaed2 %>% mutate(niv_socioeco = case_when
                                                               (barrios== "Recoleta" ~ "Clase Alta",
                                                                 barrios == "Almagro, Boedo" ~ "Clase Media",
                                                                 barrios == "Caballito" ~ "Clase Media Alta",
                                                                 TRUE ~ "Desconocido" )))


barrios_cmyaed3 <- barrios_cmyaed2 %>%
  left_join(educ %>% select(nam, geometry), by = c("escuela_cercana" = "nam")) %>%
  rename(geom_escuela = geometry.y) %>%
  st_as_sf(crs = st_crs(barrios_cmyaed2))



barrios_selec <- barrios_selec %>%
  mutate(NOMBRE = coalesce(NOMBRE, "Villa_31")) #esto para que renombre los NA como Villa 31 que es el primer string
centroides_bar <- st_centroid(barrios_selec) #trazamos centroides en barrios para la posterior medicion de distancias
distancias <- st_distance(centroides_bar, educ) #Medimos (las escuelas estan en geometria punto)
cercanias <-  st_nearest_feature(centroides_bar, educ) #Vemos cuales son las mas cercanas, quedan en el vector
centroides_bar2 <- centroides_bar %>% #el vector almacena numero y posicionamiento, lo cruzamos con centroides y
  mutate(escuela_cercana = educ$nam[cercanias]) #...traemos los nombres de las escuelas más cercanas.

centroides_bar3 <- centroides_bar2 %>% mutate(distancia_m = as.numeric (st_distance(geometry, educ[cercanias, ], by_element = TRUE)),
                                              distancia_km = distancia_m / 1000)
#da numeros con unidades, si no pongo  el as.numeric y segun claude y gpt eso dificulta para operaciones posteriores (preguntar a Pablo)

#Ahora ya tenemos los colegios con las distancias mas cercanas!
