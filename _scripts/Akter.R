#Esto lo hacemos para no pagar tanto costo computacional en las oepraciones, sobre todo geoconsultas
barrios_vul_puntos <- barrios_vul_selec %>%
  st_point_on_surface() %>%
  select(Id, barrios_nom, CO_FRAC_RA, TIPO_ASENT,MANZANA, Superficie, geometry)  # Ajustá según tus columnas

#Agrupamos y tomamos los 8 radiocensales mas extensos en superficie de cada barrio popular
radios_vul8 <- barrios_vul_puntos %>%
  group_by(barrios_nom) %>%
  arrange(desc(Superficie)) %>%
  slice_head(n = 8) %>%
  ungroup()

buffcm_vul_500  <- barrios_vul_puntos %>% st_buffer(500)  %>% mutate(buffer = "500m")
buff_vul_1000 <- barrios_vul_puntos %>% st_buffer(1000) %>% mutate(buffer = "1000m")
buff_vul_2000 <- barrios_vul_puntos %>% st_buffer(2000) %>% mutate(buffer = "2000m")
buffers_vul_bind <- bind_rows(buff_vul_500, buff_vul_1000, buff_vul_2000)
escuelas_vul_filto <- st_filter(educ_sna, buffers_vul_bind)
escuelas_vulbuffer <- st_join(escuelas_vul_filto, buffers_vul_bind, join = st_intersects)
conteo_escuelasvulpun <- escuelas_vulbuffer %>%
  group_by(buffer) %>%
  summarise(total = n())






#Para visualizar no lo podemos hacer por puntos dado que queda muy superpuesto, hay que bufferear de vuelta

radios_union <- radios_vul8 %>%
  group_by(barrios_nom) %>%
  summarise(geometry = st_union(geometry))
buff_500  <- st_buffer(radios_union, 500) %>% mutate(buffer = "0-500m")
buff_1000 <- st_buffer(radios_union, 1000) %>% mutate(buffer = "0-1000m")
buff_2000 <- st_buffer(radios_union, 2000) %>% mutate(buffer = "0-2000m")
anillos2 <- bind_rowsws(
  buff_500,
  st_difference(buff_1000, buff_500) %>% mutate(buffer = "500-1000m"),
  st_difference(buff_2000, buff_1000) %>% mutate(buffer = "1000-2000m")
)

ggplot() +
  geom_sf(data = radio_bsas, fill = "grey95", color = "black") +
  geom_sf(
    data = anillos2,
    aes(fill = buffer),
    color = "black",
    alpha = 0.1
  ) +
  geom_sf(
    data = escuelas_vulbuffer,
    aes(color = tipo_gestion),
    size = 1
  ) +
  scale_fill_manual(
    values = c("0-500m" = "red", "500-1000m" = "orange", "1000-2000m" = "blue")
  ) +
  theme_minimal() +
  labs(
    title = "Escuelas dentro de anillos de barrios vulnerables",
    fill = "Rango de distancia",
    color = "Tipo de gestión"
  )



ggplot() +
  # Base: barrios seleccionados
  geom_sf(data = radio_bsas, fill = "grey90", color = "black", size = 0.1) +
  geom_sf(data = barrios_vul_selec %>% st_transform(5347),
          aes(fill = barrios_nom),
          color = "black", size = 0.3, alpha = 0.6) +

  # Anillos
  geom_sf(data = anillos2,
          aes(fill = buffer),
          color = NA, alpha = 0.3) +

  # Escuelas
  geom_sf(data = escuelas_cmbuffer,
          color = "red", size = 1, shape = 21, fill = "yellow") +

  # Personalización
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Escuelas dentro de anillos de 500, 1000 y 2000 m",
       fill = "Anillo / Barrio") +
  theme_minimal()



# 1. Unir geometría de los barrios seleccionados
radios_union <- barrios_vul_selec %>%
  st_transform(5347) %>%
  group_by(barrios_nom) %>%
  summarise(geometry = st_union(geometry))

# 2. Crear buffers
buff_500   <- st_buffer(radios_union, 500) %>% mutate(buffer = "0-500m")
buff_1000  <- st_buffer(radios_union, 1000)
buff_2000  <- st_buffer(radios_union, 2000)

# 3. Crear anillos sin superposición
anillo_500_1000 <- st_difference(buff_1000, buff_500) %>%
  mutate(buffer = "500-1000m")

anillo_1000_2000 <- st_difference(buff_2000, buff_1000) %>%
  mutate(buffer = "1000-2000m")

# 4. Unir todos los anillos
anillos2 <- bind_rows(
  buff_500,
  anillo_500_1000,
  anillo_1000_2000
)

# 5. Filtrar escuelas dentro de los anillos
escuelas_cmbuffer <- st_join(educ_sna, anillos2, join = st_intersects)

# 6. Contar cuántas escuelas hay por barrio y anillo
conteo_escuelas <- escuelas_cmbuffer %>%
  st_drop_geometry() %>%
  group_by(barrios_nom, buffer) %>%
  summarise(total_escuelas = n(), .groups = "drop")




```{r}

#ANILLOS CONCENTRICOS DEFINITIVO

barrioscm_fil <- barrioscm_sjs %>%
  filter(
    (barrios_nom == "Palermo" & cluster == "Clase alta") |
      (barrios_nom == "Caballito" & cluster == "Clase media alta") |
      (barrios_nom == "Almagro" & cluster == "Clase media")
  ) %>%
  group_by(barrios_nom) %>%
  arrange(desc(Remuneracion_media)) %>%
  ungroup()

barrios_simpl_fast <- barrioscm_fil %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 30)


# 1. Filtrar barrio Palermo
palermo <- barrios_simpl_fast %>% filter(barrios_nom == "Palermo")
palermo_500 <- st_buffer(palermo, 500)
palermo_1000 <- st_buffer(palermo, 1000)
palermo_2000 <- st_buffer(palermo, 2000)

anillos_palermo <- bind_rows(
  palermo_500 %>% mutate(distancia = "0-500m"),
  st_difference(palermo_1000, palermo_500) %>% mutate(distancia = "500-1000m"),
  st_difference(palermo_2000, palermo_1000) %>% mutate(distancia = "1000-2000m")
)

# 2. Filtrar barrio Caballito
caballito <- barrios_simpl_fast %>% filter(barrios_nom == "Caballito")
caballito_500 <- st_buffer(caballito, 500)
caballito_1000 <- st_buffer(caballito, 1000)
caballito_2000 <- st_buffer(caballito, 2000)

anillos_caballito <- bind_rows(
  caballito_500 %>% mutate(distancia = "0-500m"),
  st_difference(caballito_1000, caballito_500) %>% mutate(distancia = "500-1000m"),
  st_difference(caballito_2000, caballito_1000) %>% mutate(distancia = "1000-2000m")
)

# 3. Filtrar barrio Almagro
almagro <- barrios_simpl_fast %>% filter(barrios_nom == "Almagro")
almagro_500 <- st_buffer(almagro, 500)
almagro_1000 <- st_buffer(almagro, 1000)
almagro_2000 <- st_buffer(almagro, 2000)

anillos_almagro <- bind_rows(
  almagro_500 %>% mutate(distancia = "0-500m"),
  st_difference(almagro_1000, almagro_500) %>% mutate(distancia = "500-1000m"),
  st_difference(almagro_2000, almagro_1000) %>% mutate(distancia = "1000-2000m")
)

# 4. Unir todos los anillos
barrioscm_anillos <- bind_rows(
  anillos_palermo,
  anillos_caballito,
  anillos_almagro
) %>%
  mutate(distancia = factor(distancia, levels = c("0-500m", "500-1000m", "1000-2000m")))

#Intersectar con escuelas
escuelascm_anillos <- st_join(
  educ_sna,
  barrioscm_anillos %>% select(barrios_nom, distancia),
  join = st_intersects
)

