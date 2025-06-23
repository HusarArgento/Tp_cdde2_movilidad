
```{r Librerías}
library(tidyverse)
library(here)
library(sf)
library(nngeo)

```

```{r Urls}
url_barrios_pop <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/barrios-populares/barrios_populares_badata_WGS84.geojson"

url_barrios2 <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/barrios-populares"

url_comunas <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.geojson"

```


```{r Carga de datos 1}

barrios_pop <- st_read(url_barrios_pop)
st_crs(barrios_pop)
sum(!st_is_valid(barrios_pop))

#391 geometrías invalidas
#Corrijo
barrios_pop <- st_make_valid(barrios_pop)
sum(!st_is_valid(barrios_pop))

#Preguntar a Pablo porque me sigue dando mal

```


```{r Posibilidad 2: Shape}
td <- tempdir()
download.file(url = paste(url_barrios2,
                          "barrios-populares-badata.zip",
                          sep = "/"),
              destfile = fs::path(td, "barrios-populares-badata.zip") )
unzip(zipfile = fs::path(td, "barrios-populares-badata.zip"),
      exdir = td)
barrios3 <- read_sf(fs::path(td, "barrios_vulnerables.shp"))
rm(td)
st_crs(barrios2)
sum(!st_is_valid(barrios2))


```
#Barrios populares
Capa que contiene el registro de los denominados barrios poulares de la Ciudad Autonoma de Buenos Aires. Dichos barrios son considerados las zonas precarias y vulnerables.
#TIPO_ASENT
Tipo de registro: Villa, Asentamiento precario, Barrio municipal, Barrio urbanizado, Conjunto habitacional, Núcleo habitacional transitorio. Son categorías definidas según el Ministerio de Desarrollo y Hábitat en Junio de 2020.


```{r Carga de datos 2}

comunas <- st_read(url_comunas)
st_crs(comunas)
sum(!st_is_valid(comunas))


```
#Comunas
Este conjunto de datos proporciona información sobre la ubicación, perímetro y área de las comunas de la Ciudad de Buenos Aires, establecidas a partir de la Ley Orgánica de Comunas (Ley Nº 1777/2005). La información permite analizar la distribución territorial y la organización administrativa de la ciudad, facilitando estudios de planificación urbana y gestión pública.








```{r Carga de datos}

url_bsasdata <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas"
url_premetro <- "https://cdn.buenosaires.gob.ar//datosabiertos/datasets/sbase/premetro"

#Comunas
comunas_caba <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.geojson")

#Premetro
lineas_pre <- st_read(paste(url_premetro, "lineas_de_premetro.geojson",
                            sep = "/") )
est_pre <- st_read(paste(url_premetro, "estaciones-premetro.csv", sep = "/"),
                   options = "GEOM_POSSIBLE_NAMES=geometry",
                   crs = 4326)

reco_pre <- st_read(paste(url_premetro, "recorrido-premetro.csv", sep = "/"),
                    options = "GEOM_POSSIBLE_NAMES=WKT",
                    crs = 4326)
#Subte
frec_sub <- read.csv2 (paste(url_bsasdata,
                             "frecuencia-subte","frecuencia_subte.csv",
                             sep = "/" ) )

est_sub <-  st_read(dsn = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/estaciones-de-subte.geojson")

bocas_sub <- st_read(dsn = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/bocas-subte/bocas-de-subte.geojson") |>
  select(-matches("lineas_de_"))
```

