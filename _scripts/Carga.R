
#Urls
url_barrios_pop <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/barrios-populares/barrios_populares_badata_WGS84.geojson"
url_barrios2 <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/barrios-populares"
url_comunas <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.geojson"
url_bsasdata <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas"
url_premetro <- "https://cdn.buenosaires.gob.ar//datosabiertos/datasets/sbase/premetro"


#Carga
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

#Shapefiles
#Rar
td <- tempdir()
rar_path <- path(td, "Shapes.rar")
download.file(url = url_radiosamba, destfile = rar_path, mode = "wb")
system(paste('winrar x', shQuote(rar_path), shQuote(td)))
list.files(td, pattern = "\\.shp$", recursive = TRUE)
radios2 <- read_sf(path(td, "Shapes", "Shape AMBA.shp"))
rm(td)

td <- tempdir()
rar_path <- path(td, "Shapes.rar")
download.file(url = url_radiosamba, destfile = rar_path, mode = "wb")
system(paste('"C:/Program Files/WinRAR/WinRAR.exe" x', shQuote(rar_path), shQuote(td)))
list.files(td, pattern = "\\.shp$", recursive = TRUE)
radios2 <- read_sf(path(td, "Shapes", "Shape AMBA.shp"))
rm(td)
st_crs(radios2)
sum(!st_is_valid(radios2))
list.files(td, recursive = TRUE)
#D:\PROGRAMS-HDD\Winrar


#Zip
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

