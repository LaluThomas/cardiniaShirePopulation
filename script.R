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
library(scales)
library(viridis)

#Read in the SA2 shapefile downloaded from the ABS
aus_sa2_shp <- 
  read_sf("./data/SA2_2016_AUST.shp")

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

#my_plot

#ggsave("C:/Users/thomaslal/OneDrive - cardinia.vic.gov.au/Desktop/Desktop folders/RPractices/Cardinia_Shire_Population_Contur_io_data/my_plot.png", my_plot, width = 8, height = 6, dpi = 300)

#import a shapefile of state boundaries
AUS_STATE_shp <- read_sf("./data/STE_2021_AUST_GDA2020.shp")

Aus_States <- ggplot()+
  geom_sf(data=AUS_STATE_shp)+
  ggtitle("Australia")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

Aus_States

#make a new dataset of cities in Australia (google the locations)

AUS_Cities <- tibble(
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide",
           "Gold Coast", "Canberra", "Hobart", "Darwin", "Alice Springs"),
  lat = c(-27.4698, -33.8688, -37.8136, -31.9522, -34.9289,
          -28.0167, -35.2931, -42.8806, -12.4381, -23.7000),
  long = c(153.0251, 151.2093, 144.9631, 115.8589, 138.6011,
           153.4000, 149.1269, 147.3250, 130.8411, 133.8667)
)


#convert those Lat Long columns to geometry column with the st_as_sf() 
#function. Google Maps uses the coordinate reference system 4326 (the GPS system).


AUS_Cities_Geometry <- st_as_sf(
  AUS_Cities, 
  coords = c("long", "lat"), 
  crs = 4326
)

ggplot() +
  geom_sf(data = AUS_STATE_shp, aes(fill="city"), color="black") +
  geom_sf(data = AUS_Cities_Geometry, aes(color="city"), size=3) +
  geom_sf_label(data = AUS_Cities_Geometry, aes(label = city), 
                size = 3, color = "black", 
                fun.geometry = st_centroid) + # Change this line
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_fill_viridis_d() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()


#import a shapefile of Australian electorates
AUS_CED_shp <- read_sf("./data/CED_2021_AUST_GDA2020.shp")

#check the imports worked

head(AUS_STATE_shp)
head(AUS_CED_shp)

#filter the shapefile for Victoria only


AUS_LGA <- read_sf("./data/LGA_2022_AUST_GDA2020.shp")

AUS_LGA_Victoria <- AUS_LGA %>% 
  filter(STE_NAME21 == "Victoria")


# VIC_LGA <- st_intersection(AUS_STATE_VIC_shp,AUS_LGA_Victoria) 


AUS_LGA_Victoria <- AUS_LGA_Victoria %>% 
  filter(!LGA_CODE22=="29799" , !LGA_CODE22=="29499" )

lga_total_population <- read_csv("./data/lga_population.csv")

lga_total_population <- lga_total_population %>% 
  filter(!LGA_CODE_2021=="LGA29799" , !LGA_CODE_2021=="LGA29499" )

lga_total_population$LGA_Code <- substr(lga_total_population$LGA_CODE_2021,4,(nchar(lga_total_population$LGA_CODE_2021)))

lga_total_population <- lga_total_population %>% 
  select(
    c(
      Tot_P_M,
      Tot_P_F,
      Tot_P_P,
      LGA_Code
    )
  )

AUS_LGA_Victoria <- AUS_LGA_Victoria %>% 
  mutate(
    LGA_Code=LGA_CODE22
  )

AUS_LGA_Victoria <- left_join(AUS_LGA_Victoria,lga_total_population,by="LGA_Code")

options(scipen=999)

VIC_LGA_Population <- ggplot() +
  geom_sf(data = AUS_LGA_Victoria,
          aes(fill = Tot_P_P)) +
  scale_fill_viridis_c(name = "Population",
                       labels = comma) +
  labs(title = "2021 Census - Victorian LGA's population",
       subtitle = "Data Source: Australian Bureau of Statistics",
       x = "Longitude",
       y = "Latitude") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("VIC_LGA_Population.png", VIC_LGA_Population, width = 8, height = 6, dpi = 300)
