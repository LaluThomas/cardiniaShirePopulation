#Instruction
#https://medium.com/analytics-vidhya/mapping-australia-in-r-6ce092c48b49
#ABS shape files
#https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument

#Australia shape files
#https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files

#instruction population density
#https://www.youtube.com/watch?v=zgFXVhmKNbU&ab_channel=SpencerSchien

#census data packs
#https://www.abs.gov.au/census/find-census-data/datapacks?release=2021&product=GCP&geography=LGA&header=S

library(sf)
library(tidyverse)
library(rgdal)
library(broom)

# load kontur data
data <- st_read("data/kontur_population_AU_20220630.gpkg")


my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/data/") , 
  layer="STE_2021_AUST_GDA2020",
  verbose=FALSE
)

spdf_fortified <- tidy(my_spdf, region = "NAME")


shapefile <- "data/STE_2021_AUST_GDA2020"

shapefile_data <- readOGR(dsn = shapefile, layer = basename(shapefile))

shapefile_data <- readOGR(dsn = shapefile, layer = basename(shapefile))

shapefile %>% 
  ggplot()+
  geom_sf()
#load the states

#load spatial packages

library(rgdal)
library(tmap)
library(ggmap)
library(sf)
library(ggspatial)
library(rlang)
library(broom)
library(tidyverse)
library(readxl)
library(RStoolbox)
library(magick)
#library(raustats) #not available
library(purrr)
library("Census2016")

#Read in the SA2 shapefile downloaded from the ABS
aus_sa2_shp <- 
  read_sf("C:/Users/thomaslal/OneDrive - cardinia.vic.gov.au/Desktop/Desktop folders/RPractices/Cardinia_Shire_Population_Contur_io_data/data/SA2_2016_AUST.shp")

#filter the Australian SA2 shapefile for only Victoria
aus_sa2_vic_shp <- aus_sa2_shp %>%
  filter(STE_CODE16 == 2)

#plot a map of Australia (grey) and a map of Victoria (green)

my_plot <- ggplot() +
  geom_sf(data=aus_sa2_shp)+
  geom_sf(data=aus_sa2_vic_shp,fill = "#A5D46A")+
  ggtitle("") +
  xlab("") +
  xlim(110,155)+
  ylab("") + 
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
     
  )

ggsave("C:/Users/thomaslal/OneDrive - cardinia.vic.gov.au/Desktop/Desktop folders/RPractices/Cardinia_Shire_Population_Contur_io_data/my_plot.png", my_plot, width = 8, height = 6, dpi = 300)

#import a shapefile of state boundaries
AUS_STATE_shp <- read_sf("./data/STE_2021_AUST_GDA2020.shp")

#import a shapefile of Australian electorates
AUS_CED_shp <- read_sf("./data/CED_2021_AUST_GDA2020.shp")

#check the imports worked

head(AUS_STATE_shp)
head(AUS_CED_shp)

#filter the shapefile for Victoria only
AUS_STATE_VIC_shp <- AUS_STATE_shp %>%
  filter(STE_CODE21==2)


victoria <- ggplot() +
  geom_sf(data=AUS_STATE_VIC_shp)+
  ggtitle("Victoria") +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme_bw()


#run a spatial intersection for the state of Victoria and all electorates
VIC_CED <- st_intersection(AUS_STATE_VIC_shp, AUS_CED_shp)
ggplot() +
  geom_sf(data=VIC_CED)+
  ggtitle("Commonwealth Electoral Divisions in Victoria") +
  xlab("Longitude") +
  ylab("Latitude") + 
  theme_bw()
