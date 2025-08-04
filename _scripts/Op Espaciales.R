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

#Cargamos el mapa de barrios y le transformamos el crs
barrioscm_caba <- st_read(url_barrioscaba)
st_crs(barrioscm_caba)
sum(!st_is_valid(barrioscm_caba))
barrioscm_caba <- st_transform(barrioscm_caba, 5347)

barrios_cm_sj <- st_join(bairescm_q, barrioscm_caba, join = st_intersects)

barrioscm_sjs <- barrios_cm_sj %>%
  select(CO_FRAC_RA, geometry, Remuneracion_media, cluster, nombre, comuna, perimetro_, area_metro) %>%
  rename(barrios_nom = nombre)
#rm(barrios_cm_sj)




# Calculo en Multipoligon

# 1. Simplificar barrios para reducir vértices
barrios_vul_simpl <- st_simplify(barrios_vul_selec, dTolerance = 50)

# 2. Buffers y anillos concéntricos
buff_500  <- st_buffer(barrios_vul_simpl, 500)
buff_1000 <- st_buffer(barrios_vul_simpl, 1000)
buff_2000 <- st_buffer(barrios_vul_simpl, 2000)

anillos_vul <- bind_rows(
  buff_500 %>% mutate(distancia = "0-500m"),
  st_difference(buff_1000, buff_500) %>% mutate(distancia = "500-1000m"),
  st_difference(buff_2000, buff_1000) %>% mutate(distancia = "1000-2000m")
) %>%
  mutate(distancia = factor(distancia, levels = c("0-500m", "500-1000m", "1000-2000m")))

# 3. Filtrar escuelas solo una vez (intersección directa)
escuelas_vul_buffers <- st_filter(educ_sna, anillos)

# 4. Contar escuelas por distancia
conteopoli <- escuelas_vul_buffers %>%
  st_join(anillos_vul, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(distancia) %>%
  summarise(total = n())

#Conteo
conteo_esc_vul <- barrios_vul_educ %>%
  group_by(NOMBRE, distancia) %>%  # reemplazá ID_barrio por la columna que identifica barrios
  summarize(cantidad_escuelas = n(), .groups = "drop") %>%
  arrange(NOMBRE, distancia)

# 1. Simplificar geometría de los barrios
barrios_simpl <- st_simplify(barrios_vul_selec, dTolerance = 50)

# 2. Buffers y anillos concéntricos
anillos <- bind_rows(
  st_buffer(barrios_simpl, 500) %>% mutate(buffer = "0-500m"),
  st_difference(st_buffer(barrios_simpl, 1000), st_buffer(barrios_simpl, 500)) %>% mutate(buffer = "500-1000m"),
  st_difference(st_buffer(barrios_simpl, 2000), st_buffer(barrios_simpl, 1000)) %>% mutate(buffer = "1000-2000m")
) %>%
  mutate(buffer = factor(buffer, levels = c("0-500m", "500-1000m", "1000-2000m")))

# 3. Filtrar y asignar buffer en un solo join
escuelas_vulbuffer <- st_join(
  educ_sna,   anillos %>%
    select(buffer, barrios_nom),
  join = st_intersects
)

# 4. Conteo único
conteo_escuelas <- escuelas_vulbuffer %>%
  st_drop_geometry() %>%
  group_by(buffer, barrios_nom) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(buffer = factor(buffer, levels = c("0-500m", "500-1000m", "1000-2000m"))) %>%
  arrange(barrios_nom, buffer)





barrios_vul_selec <- barrios_vul_rs %>% filter(NOMBRE %in% c("Villa 31","Villa 31 - Padre Mugica", "Villa 21-24", "Villa 15 - Ciudad Oculta")) %>%
  mutate(NOMBRE = case_when(
    NOMBRE == "Villa 15 - Ciudad Oculta" ~ "Villa_15",
    NOMBRE == "Villa 31 - Padre Mugica" ~ "Villa_31",
    NOMBRE == "Villa 31 - Padre Mugica" ~ "Villa_31",
    NOMBRE == "Villa 21-24" ~ "Villa_21_24"
  ))  %>%
  rename(barrios_nom = NOMBRE)


#Para visualizar no lo podemos hacer por puntos dado que queda muy superpuesto, hay que bufferear de vuelta

radios_union <- barrios_vul_selec %>%
  group_by(barrios_nom) %>%
  summarise(geometry = st_union(geometry))
buff_500  <- st_buffer(radios_union, 500) %>% mutate(buffer = "0-500m")
buff_1000 <- st_buffer(radios_union, 1000) %>% mutate(buffer = "0-1000m")
buff_2000 <- st_buffer(radios_union, 2000) %>% mutate(buffer = "0-2000m")
anillos2 <- bind_rows(
  buff_500,
  st_difference(buff_1000, buff_500) %>% mutate(buffer = "500-1000m"),
  st_difference(buff_2000, buff_1000) %>% mutate(buffer = "1000-2000m")
)
escuelas_cm_filtro <- st_filter(educ_sna, anillos2)
escuelas_cmbuffer <- st_join(escuelas_cm_filtro, buffers_cm_bind, join = st_intersects)


```{r Buffers}

buff_vul_500  <- barrios_vul_puntos %>% st_buffer(500)  %>% mutate(buffer = "500m")
buff_vul_1000 <- barrios_vul_puntos %>% st_buffer(1000) %>% mutate(buffer = "1000m")
buff_vul_2000 <- barrios_vul_puntos %>% st_buffer(2000) %>% mutate(buffer = "2000m")
buffers_vul_bind <- bind_rows(buff_vul_500, buff_vul_1000, buff_vul_2000)
escuelas_vul_filto <- st_filter(educ_sna, buffers_vul_bind)
escuelas_vulbuffer <- st_join(escuelas_vul_filto, buffers_vul_bind, join = st_intersects) %>%
  filter(!is.na (tipo_gestion))
conteo_escuelasvulpun <- escuelas_vulbuffer %>%
  group_by(escuela, buffer, barrios_nom) %>%
  summarise(total = n())

```

```{r}
tabla_resumen_vul <- table(conteo_escuelasvulpun$barrios_nom, conteo_escuelasvulpun$buffer)
addmargins(tabla_resumen_vul)
```

```{r Geoconsulta: Preparación}


#Agrupamos y tomamos los 8 radiocensales mas extensos en superficie de cada barrio popular
radios_vul8 <- barrios_vul_puntos %>%
  group_by(barrios_nom) %>%
  arrange(desc(Superficie)) %>%
  slice_head(n = 8) %>%
  ungroup()


```


