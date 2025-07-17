
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
