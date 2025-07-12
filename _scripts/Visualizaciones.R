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
