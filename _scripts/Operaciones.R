
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
