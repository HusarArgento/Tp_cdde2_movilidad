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



bloque1 <- barrios_vul_bloqp %>%
  filter(barrios_nom == "Villa_31", bloque == 1)

buffer_500 <- st_buffer(bloque1, dist = 500)
buffer_1000 <- st_buffer(bloque1, dist = 1000)
buffer_2000 <- st_buffer(bloque1, dist = 2000)
escuelas_500 <- educ_sna[st_intersects(educ_sna, st_union(buffer_500), sparse = FALSE), ]
escuelas_1000 <- educ_sna[st_intersects(educ_sna, st_union(buffer_1000), sparse = FALSE), ]
escuelas_2000 <- educ_sna[st_intersects(educ_sna, st_union(buffer_2000), sparse = FALSE), ]

tabla1_500 <- osrmTable(src = bloque1, dst = escuelas_500, osrm.profile = "foot")$durations

head(tabla1_500)

barrios_vul_bloqp <- barrios_vul_puntos %>%
  group_by(barrios_nom) %>%
  mutate(bloque = ceiling(row_number() / 50))



barrios_vul_bloqp %>%
  count(barrios_nom, bloque) %>%
  arrange(barrios_nom, bloque)

buff_cm_500  <- barrioscm_sjs_pts %>% st_buffer(500)  %>% mutate(buffer = "500m")
buff_cm_1000 <- barrioscm_sjs_pts %>% st_buffer(1000) %>% mutate(buffer = "1000m")
buff_cm_2000 <- barrioscm_sjs_pts %>% st_buffer(2000) %>% mutate(buffer = "2000m")
buffers_cm_bind <- bind_rows(buff_cm_500, buff_cm_1000, buff_cm_2000)
escuelas_cm_filtro <- st_filter(educ_sna, buffers_cm_bind)
escuelas_cmbuffer <- st_join(escuelas_cm_filtro, buffers_cm_bind, join = st_intersects)
conteo_escuelas_cm <- escuelas_cmbuffer %>%
  group_by(buffer, barrios_nom) %>%
  summarise(total = n(), .groups = "drop")
```


```{r}
#Trabajamos con anillos concentricos en vez de buffers

# Hacemos  buffers individuales, necesarios para esta operacion
buffer_500 <- barrioscm_fil %>% st_buffer(dist = 500)
buffer_1000 <- barrioscm_fil %>% st_buffer(dist = 1000)
buffer_2000 <- barrioscm_fil %>% st_buffer(dist = 2000)

# Crear los anillos concéntricos
barrioscm_anillos <- bind_rows(
  buffer_500 %>% mutate(distancia = "0-500m"),                              # Anillo interior completo
  st_difference(buffer_1000, buffer_500) %>% mutate(distancia = "500-1000m"),  # Solo la corona entre 500-1000m
  st_difference(buffer_2000, buffer_1000) %>% mutate(distancia = "1000-2000m")  # Solo la corona entre 1000-2000m
) %>%
  mutate(distancia = factor(distancia, levels = c("0-500m", "500-1000m", "1000-2000m")))

#Hacemos los joins
barrioscm_educ <- st_join(barrioscm_anillos, educ_sna, join = st_intersects)
educ_barrioscm <- st_join(educ_sna, barrioscm_anillos, join = st_intersects)%>%
  filter(!is.na(distancia))

