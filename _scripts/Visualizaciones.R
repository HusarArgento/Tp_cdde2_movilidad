#Visualizaciones

ggplot(radios)

ggplot(radios) +
  geom_sf(aes(fill = is.na(barrios_j))) +
  labs(title = "Radiocensales?",
       fill = "¿Tiene comuna?") +
  theme_minimal()

ggplot(baires_q) +
  geom_sf(aes (fill = cluster))
theme_minimal()

ggplot(baires_q) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_manual(
    values = c(
      "Clase media baja" = "#fddbc7",
      "Clase media" = "#f4a582",
      "Clase media alta" = "#d6604d",
      "Clase alta" = "#67001f"
    )
  ) +
  theme_minimal() +
  labs(title = "Mapa socioeconómico por cuartiles de ingreso", fill = "Clase social")


ggplot(barrios_j2) +
  geom_sf(aes(fill = is.na(comuna))) +
  labs(title = "Barrios populares: ¿tienen comuna asignada?",
       fill = "¿Tiene comuna?") +
  theme_minimal()


ggplot() +
  geom_sf(data = comunas, fill = "grey90", color = "black") +
  geom_sf(data = barrios_j, aes(fill = comuna), color = NA, alpha = 0.8) +
  labs(title = "Ubicación de barrios populares dentro de CABA",
       fill = "Comuna") +
  theme_minimal()


ggplot() +
  geom_sf(data = radios, fill = "grey90", color = "black", size = 0.1) +
  geom_sf(data = barrios_j, fill = "red", color = "darkred", alpha = 0.) +
  labs(title = "Ubicación de barrios populares  de CABA - Radiocensales",
       fill = "Comuna") +
  theme_minimal()


ggplot() +
  geom_sf(data = radios, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = barrios_j, fill = "red", color = "darkred", alpha = 0.7) +
  theme_void() +
  labs(title = "Radios Censales y Barrios Carenciados")


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = comunas,
              fillColor = "transparent",
              color = "black",
              weight = 1,
              label = ~comuna)%>%
  addPolygons(data = barrios_j,
              fillColor = "red",
              color = "darkred",
              weight = 1,
              fillOpacity = 0.6,
              label = ~NOMBRE)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = comunas,
              fillColor = "transparent",
              color = "black",
              weight = 1,
              label = ~comuna)%>%
  addPolygons(data = com_ingreso,
              fillColor = "red",
              color = "darkred",
              weight = 1,
              fillOpacity = 0.6,
              label = ~barrios)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = comunas,
              fillColor = "transparent",
              color = "black",
              weight = 1,
              label = ~comuna)%>%
  addCircleMarkers(data = barrios_pcomp, #reemplazar addpolygons por addcirclemarkers porque son gm de puntos
                   fillColor = "red",
                   color = "darkred",
                   weight = 1,
                   fillOpacity = 0.6,
                   label = ~barrios) %>%
  addCircleMarkers(data = barrios_cmyaed2,
                   fillColor = "blue",
                   color = "darkblue",
                   weight = 1,
                   fillOpacity = 0.6,
                   label = ~barrios)
#Este no se ve claramente. Probamos otro poniendo los barrios multipoligons

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = comunas,
              fillColor = "transparent",
              color = "black",
              weight = 1,
              label = ~comuna)%>%
  addPolygons (data = barrios_selec,
               fillColor = "red",
               color = "darkred",
               weight = 1,
               fillOpacity = 0.6,
               label = ~barrios) %>%
  addPolygons(data = barrios_cmym,
              fillColor = "blue",
              color = "darkblue",
              weight = 1,
              fillOpacity = 0.6,
              label = ~barrios) %>%
  addCircleMarkers(data = escuelas_cercanas,
                   radius = 6,
                   color = "darkorange",
                   fillColor = "orange",
                   fillOpacity = 0.8,
                   label = ~escuelas_cercanas,
                   popup = ~paste0("<strong>Escuela:</strong> ", escuelas_cercanas)) %>%
  addCircleMarkers(data = escuelas_cercanas2,
                   radius = 6,
                   color = "darkgreen",
                   fillColor = "green",
                   fillOpacity = 0.8,
                   label = ~escuelas_cercanas,
                   popup = ~paste0("<strong>Escuela:</strong> ", escuelas_cercanas))



leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = radios,
              fillColor = "transparent",
              color = "black",
              weight = 1,
              label = ~comuna)%>%
  addPolygons(data = barrios_j,
              fillColor = "red",
              color = "darkred",
              weight = 1,
              fillOpacity = 0.6,
              label = ~NOMBRE)


ggplot(barrios_q) +
  geom_sf(data = radio_bsas, fill = "grey95", color = "black") +
  geom_sf(aes(fill = cluster), color = "white", size = 0.1) +
  geom_sf_text(data = comunas_caba, aes(label = barrios),
               color = "black", size = 3, fontface = "bold")+
  scale_fill_manual(
    values = c(
      "Clase baja" = "#f2dede",
      "Clase media" = "#f9bfbf",
      "Clase media alta" = "#f08080",
      "Clase alta" = "#c0392b"
    )
  ) +
  theme_minimal() +
  labs(title = "Distribución de clases sociales por radiocensal en CABA",
       fill = "Clase Social")

pal <- colorFactor(palette = "Set1", domain = baires_q$cluster)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = baires_q,
              fillColor =  ~pal(cluster),
              color = "darkred",
              weight = 1,
              fillOpacity = 0.6,
              label = ~ Remuneracion_media)


ggplot(conteo_escuelasvulpun, aes(x = buffer, y = total, fill = buffer)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = total), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cantidad de escuelas dentro de cada buffer",
    x = "Buffer de distancia",
    y = "Número de escuelas"
  ) +
  theme_minimal()


library(ggplot2)

# Pasar la tabla a formato data frame
df_heatmap <- as.data.frame(tabla_resumen)

# Crear heatmap
ggplot(df_heatmap, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Escuelas por barrio y buffer",
    x = "Buffer",
    y = "Barrio",
    fill = "Total escuelas"
  ) +
  theme_minimal()


#LEAFLEET para puntos
buffers_cm_bind_wgs <- st_transform(buffers_cm_bind, 4326)
escuelascm_filtradas_wgs <- st_transform(escuelascm_filtradas, 4326)
barrioscm_fil_wgs <- st_transform(barrioscm_fil, 4326)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Buffers (colores por distancia)
  addPolygons(data = buffers_cm_bind_wgs,
              fillColor = ~case_when(
                buffer == "500m" ~ "blue",
                buffer == "1000m" ~ "skyblue",
                buffer == "2000m" ~ "red"
              ),
              fillOpacity = 0.1,
              color = "black",
              weight = 1,
              group = "Buffers") %>%

  # Escuelas (colores por gestión)
  addCircleMarkers(data = escuelascm_filtradas_wgs,
                   radius = 4,
                   color = ~case_when(
                     tipo_gestion == "Estatal" ~ "green",
                     tipo_gestion == "Privada" ~ "purple",
                     TRUE ~ "gray"
                   ),
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   popup = ~paste0("<b>Escuela: </b>", escuela,
                                   "<br><b>Gestión: </b>", tipo_gestion),
                   group = "Escuelas") %>%

  # Centroides (opcional)
  addCircleMarkers(data = barrioscm_fil_wgs,
                   radius = 5,
                   color = "red",
                   fill = TRUE,
                   fillOpacity = 1,
                   stroke = TRUE,
                   weight = 1,
                   popup = ~paste0("<b>Barrio: </b>", barrios_nom),
                   group = "Centroides") %>%
  addLegend(
    position = "bottomright",
    colors = c("red", "orange", "blue"),
    labels = c("0-500m", "500-1000m", "1000-2000m"),
    title = "Rango de distancia"
  )%>%

  # Control de capas
  addLayersControl(
    overlayGroups = c("Buffers", "Escuelas", "Centroides"),
    options = layersControlOptions(collapsed = FALSE)
  )
ggplot(df_tiemposcm_larga_500, aes(x = radio_id, y = tiempo_min)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6) +
  labs(
    title = "Distribución de tiempos a pie (500m)",
    x = "Radio censal",
    y = "Tiempo de viaje (minutos)"
  ) +
  theme_minimal()
