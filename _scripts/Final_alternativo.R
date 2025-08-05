#Barrios vul alternativo
df_tiemposvul_ord <- df_tiemposvul_larga_500 %>%
  mutate(
    radio_id_num = as.numeric(radio_id),  # paso 1
    barrio = case_when(
      radio_id_num >= 1 & radio_id_num <= 8   ~ "Villa 15",
      radio_id_num >= 9 & radio_id_num <= 16  ~ "Villa 31",
      radio_id_num >= 17 & radio_id_num <= 24 ~ "Villa 21-24",
      TRUE                                   ~ "Otro"
    )
  )



# Normalizamos columnas antes de unir
df_tiemposcm_larga_500 <- df_tiemposcm_larga_500 %>%
  mutate(grupo = "Clase media y alta")

#df_tiemposcm_larga_500 <- df_tiemposcm_larga_500 %>%
# mutate(grupo_detalle = case_when(
#  barrios_nom == "Almagro" ~ "Clase media baja",
# barrios_nom == "Caballito" ~ "Clase media",
#barrios_nom == "Palermo" ~ "Clase media alta",
#TRUE ~ "Otro"
#))
df_tiemposvul_larga_500 <- df_tiemposvul_larga_500 %>%
  mutate(grupo = "Vulnerable")

# Unificamos
df_tiempos_unificado_500 <- bind_rows(df_tiemposcm_larga_500, df_tiemposvul_larga_500)

# Verificamos conteo por grupo
df_tiempos_unificado_500 %>% count(grupo)
print(df_tiempos_unificado_500)




resumen_tiempos <- df_tiempos_unificado_500 %>%
  group_by(grupo, buffer) %>%
  summarise(
    tiempo_promedio = mean(tiempo_min, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_tiempos)
library(ggplot2)

ggplot(resumen_tiempos, aes(x = buffer, y = tiempo_promedio, fill = grupo)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Tiempo promedio a escuelas según grupo y buffer",
    x = "Buffer",
    y = "Tiempo promedio (minutos)",
    fill = "Grupo"
  ) +
  theme_minimal()


ggplot(df_tiempos_unificado_500, aes(x = reorder(radio_id, tiempo_min), y = tiempo_min, fill = grupo)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  labs(
    title = "Tiempo a escuelas (500m) por radio censal y grupo",
    x = "Radio censal",
    y = "Tiempo (minutos)",
    fill = "Grupo"
  ) +
  theme_minimal()


df_cm <- conteo_escuelas_cm %>%
  mutate(grupo = case_when(
    barrios_nom == "Almagro" ~ "Clase media baja",
    barrios_nom == "Caballito" ~ "Clase media",
    barrios_nom == "Palermo" ~ "Clase media alta"
  ))

df_vul <- conteo_escuelasvul %>%
  mutate(grupo = "Vulnerable")

conteo_comparado <- bind_rows(df_cm, df_vul)
tabla_comparativa <- table(conteo_comparado$grupo, conteo_comparado$buffer)
addmargins(tabla_comparativa)




  library(ggplot2)

  ggplot(data = conteo_unificado, aes(x = buffer, y = total, fill = grupo)) +
    geom_col(position = "dodge") +
    labs(
      title = "Cantidad de escuelas por buffer y grupo socioeconómico",
      x = "Buffer",
      y = "Total de escuelas",
      fill = "Grupo socioeconómico"
    ) +
    theme_minimal()
