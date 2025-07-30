
names(socioeco_caba)
glimpse(socioeco_caba)
socioec_com <- group_by(socioeco_caba, comuna) %>%
  summarise(
    ingreso_mediano = median(ingresos_familiares, na.rm  = TRUE),
    ingreso_medio = mean(ingresos_familiares, nar.rm = TRUE ),
  )
com_ingreso <- left_join(comunas, socioec_com, by = "comuna")


#Ocurre que no tienen los mismos campos. Vamos a armar una tabla:
#1) Renombramos y colectamos las mismas columnas

barrios_pcomp <- centroides_bar3 %>%
  rename( barrio = NOMBRE,) %>%
  mutate(niv_socioeco = "Barrio Popular") %>%
  select(barrio, comuna, niv_socioeco, escuela_cercana,  distancia_m, distancia_km, geometry)

barrios_cmyaed2 <- barrios_cmyaed2 %>%
  rename( barrio = barrios,) %>%
  mutate( niv_socioeco = case_when(
    barrio == "Recoleta" ~ "Clase Alta",
    barrio == "Almagro, Boedo" ~ "Clase Media",
    barrio == "Caballito" ~ "Clase Media Alta", TRUE ~ "Desconocido")) %>%
  select(barrio, comuna, niv_socioeco, escuela_cercana,  distancia_m, distancia_km, geometry)
comparacion_distancias <- bind_rows(barrios_pcomp, barrios_cmyaed2)

#✅ Supongamos que tu objeto se llama radios_df y que tenés estas columnas:

#cluster: la clase social (ej: "Clase baja", "Clase media", etc.)

#BARRIO: el nombre del barrio

#geometry: las geometrías de los radios censales (porque es un sf)

#🔹 Ejemplo 1: Seleccionar radios censales de clase media en Almagro y Caballito

radios_alma_cabam <- radios_df %>%
  filter(cluster == "Clase media",
         BARRIO %in% c("ALMAGRO", "CABALLITO"))

#🔹 Ejemplo 2: Seleccionar radios de clase alta en Palermo

radios_palermo_alta <- radios_df %>%
  filter(cluster == "Clase alta",
         BARRIO == "PALERMO")

#🔎 ¿Querés explorar los barrios disponibles?

  unique(radios_df$BARRIO)

#🛠 ¿Querés ver cuántos radios hay por barrio y clase?

  radios_df %>%
  count(BARRIO, cluster)


  #Agrupamos y elegimos comunas
  barrios_q4 <- barrios_q3 %>%
    filter(barrios_nom == "Palermo" & cluster == "Clase alta" |
             barrios_nom == "Caballito" & cluster == "Clase media alta"|
             barrios_nom == "Almagro" & cluster == "Clase media" ) %>%
    group_by(barrios_nom) %>%
    arrange(desc (Remuneracion_media)) %>%
    slice_head(n=3)
  names(socioeco_caba)
  glimpse(socioeco_caba)
  socioec_com <- group_by(socioeco_caba, comuna) %>%
    summarise(
      ingreso_mediano = median(ingresos_familiares, na.rm  = TRUE),
      ingreso_medio = mean(ingresos_familiares, nar.rm = TRUE ),
    ) #Sacamos  medias y medianas de ingresos familiares luego de agrupar por comunas
  com_ingreso <- left_join(comunas, socioec_com, by = "comuna")
  #Unimos un join espacial con uno no espacial!

  # barrios_cmya <- com_ingreso %>% filter(barrios %in% c("Caballito", "Almagro, Boedo", "Recoleta")) %>% st_centroid()
  #Seleccionamos barrios peroy como es una geometria multipoligon le hacems centroides (esto no va porque necesito los barrios cmya como multipoligons para leafleet)

  barrios_cmym <- com_ingreso %>% filter(barrios %in% c("Caballito", "Almagro, Boedo", "Recoleta"))
  barrios_cmya <- st_centroid(barrios_cmym)
  #Esto lo hacemos para representar en leafleet

  cmya_cercanias <-  st_nearest_feature(barrios_cmya, educ)
  barrios_cmyaed2 <- barrios_cmya %>% mutate(escuela_cercana = educ$nam[cmya_cercanias],
                                             distancia_m = as.numeric(st_distance(geometry,
                                                                                  educ[cmya_cercanias, ],
                                                                                  by_element =TRUE)),
                                             distancia_km = as.numeric(st_distance(geometry,                                                                                educ[cmya_cercanias,],
                                                                                   by_element = TRUE)
                                             ) / 1000
  )
  #Aca  vamos a hacer integrar ambos datasets para ver la distancias
  escuelas_cercanas <- educ %>% filter(nam %in% barrios_cmyaed2$escuela_cercana) #Para Leafleet
  escuelas_cercanas2 <- educ %>% filter(nam %in% barrios_pcomp$escuela_cercana)
  comparacion_distancias <- bind_rows(centroides_bar3 %>% mutate(niv_socioeco = "Barrio Popular"),
                                      barrios_cmyaed2 %>% mutate(niv_socioeco = case_when
                                                                 (barrios== "Recoleta" ~ "Clase Alta",
                                                                   barrios == "Almagro, Boedo" ~ "Clase Media",
                                                                   barrios == "Caballito" ~ "Clase Media Alta",
                                                                   TRUE ~ "Desconocido" )))
  #Ocurre que no tienen los mismos campos. Vamos a armar una tabla:
  #1) Renombramos y colectamos las mismas columnas

  barrios_pcomp <- centroides_bar3 %>%
    rename( barrio = NOMBRE,) %>%
    mutate(niv_socioeco = "Barrio Popular") %>%
    select(barrio, comuna, niv_socioeco, escuela_cercana,  distancia_m, distancia_km, geometry)

  barrios_cmyaed2 <- barrios_cmyaed2 %>%
    rename( barrio = barrios,) %>%
    mutate( niv_socioeco = case_when(
      barrio == "Recoleta" ~ "Clase Alta",
      barrio == "Almagro, Boedo" ~ "Clase Media",
      barrio == "Caballito" ~ "Clase Media Alta", TRUE ~ "Desconocido")) %>%
    select(barrio, comuna, niv_socioeco, escuela_cercana,  distancia_m, distancia_km, geometry)

  #Ahora sí, unimos
  comparacion_distancias <- bind_rows(barrios_pcomp, barrios_cmyaed2)

  # Esto no
  #barrios_cmyaed3 <- barrios_cmyaed2 %>%
  #  left_join(educ %>% select(nam, geometry), by = c("escuela_cercana" = "nam")) %>%
  #  rename(geom_escuela = geometry.y) %>%
  #  st_as_sf(crs = st_crs(barrios_cmyaed2))


  proximidad <- st_join(barrios_puntos, educ_barrios_vul, join = st_nearest_feature)

  #buffers
  barrios_vul_puntos <- barrios_vul_selec %>%
    st_point_on_surface() %>%
    select(Id, barrios_nom, CO_FRAC_RA, TIPO_ASENT,MANZANA, Superficie, geometry)  # Ajustá según tus columnas

  radios_vul8 <- barrios_vul_puntos %>%
    group_by(barrios_nom) %>%
    arrange(desc(Superficie)) %>%
    slice_head(n = 8) %>%
    ungroup()
  buff_vul_500 <- st_buffer(radios_vul8, dist = 500)
  buff_vul_1000 <- st_buffer(radios_vul8, dist = 1000)
  buff_vul_2000 <- st_buffer(radios_vul8, dist = 2000)

  escuelasvul_500 <- st_filter(educ_sna, buff_vul_500)
  escuelasvul_1000 <- st_filter(educ_sna, buff_vul_1000)
  escuelasvul_2000 <- st_filter(educ_sna, buff_vul_2000)




  #Hacemos los buffers
  buffer_vul_500  <- barrios_vul_selec %>% st_buffer(500)  %>% mutate(distancia = "500m")
  buffer_vul_1000 <- barrios_vul_selec %>% st_buffer(1000) %>% mutate(distancia = "1000m")
  buffer_vul_2000 <- barrios_vul_selec %>% st_buffer(2000) %>% mutate(distancia = "2000m")

  # Anillos concéntricos para barrios vulnerables
  buffers_barrios_vul <- bind_rows(
    buffer_vul_500 %>% mutate(distancia = "0-500m"),
    st_difference(buffer_vul_1000, buffer_vul_500) %>% mutate(distancia = "500-1000m"),
    st_difference(buffer_vul_2000, buffer_vul_1000) %>% mutate(distancia = "1000-2000m")
  ) %>%
    mutate(distancia = factor(distancia, levels = c("0-500m", "500-1000m", "1000-2000m")))
  barrios_vul_educ <- st_join(buffers_barrios_vul, educ_sna, join = st_intersects)
  filter(!is.na(distancia))
  educ_puntos <- educ_barrios_vul %>%
    st_cast("POINT")
  st_geometry_type(educ_puntos) %>% unique()




  educ_barrios_vul <- st_join(educ_sna, buffers_barrios_vul, join = st_intersects)%>%
    filter(!is.na(distancia))
  #Joins: Unimos por interseccion los buffers con las escuelas para ver cuantas entran en cada radio

  barrios_vul_educ <- st_join(buffers_barrios_vul, educ_sna, join = st_intersects)
  filter(!is.na(distancia))

  educ_barrios_vul <- st_join(educ_sna, buffers_barrios_vul, join = st_intersects)%>%
    filter(!is.na(distancia))

  #para sacar escuelas que no pertenezcan a ningun buffer (mas adelante para grafico=
  educ_puntos <- educ_barrios_vul %>%
    st_cast("POINT")
  st_geometry_type(educ_puntos) %>% unique()


  #Ordenamos y clasificamos los clusters

  baires_ecorad <- mapa_eco  %>% filter(provincia_id == 2, Remuneracion_media > 30000 ) #Filtrado de CABA

  bairescm_q <- baires_ecorad %>% mutate(cluster = case_when(
    Remuneracion_media <= quantile(Remuneracion_media, 0.25, na.rm = TRUE) ~ "Clase baja",
    Remuneracion_media <= quantile(Remuneracion_media, 0.50, na.rm = TRUE) ~ "Clase media",
    Remuneracion_media <= quantile(Remuneracion_media, 0.75, na.rm = TRUE) ~ "Clase media alta",
    TRUE ~ "Clase alta"
  ))

  conteo_escuelasbarrios <- barrios_educ %>%
    group_by(barrios_nom, distancia) %>%  # reemplazá ID_barrio por la columna que identifica barrios
    summarize(cantidad_escuelas = n(), .groups = "drop")
