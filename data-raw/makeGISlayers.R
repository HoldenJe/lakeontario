library(rgdal)
library(devtools)
use_data_raw()
#use_data()
LOshore<-readOGR(dsn=file.path(getwd(), 'data-raw'), layer='lo_lake_ESRI_100km')
LObath100m<-readOGR(dsn=file.path(getwd(), 'data-raw'), layer='LO_100m')
#plot(LOshore)
#plot(LObath100m, add=T, lty=2)

use_data(LOshore, overwrite = T)
use_data(LObath100m, overwrite = T)

LOschaner<-read.csv('data-raw/LOshore2.csv')
use_data(LOschaner, overwrite = T)


USGS_Species <- readr::read_csv("data-raw/SPECIES.csv")
use_data(USGS_Species, overwrite = T)

library(sf)
library(ggplot2)
shape_ontarioshore <- sf::st_read("data-raw/lo_lake_ESRI_100km")
base_ontarioshore <- ggplot() +
  geom_sf(data = shape_ontarioshore) +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

usethis::use_data(base_ontarioshore, overwrite = TRUE)
usethis::use_data(shape_ontarioshore, overwrite = TRUE)
