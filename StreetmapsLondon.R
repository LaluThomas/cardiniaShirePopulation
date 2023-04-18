
#Instruction

#https://www.youtube.com/watch?v=VpKmKRNFH34&ab_channel=StatisticsGuideswithDrPaulChristiansen


install.packages("osmdata")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("sf")

library(tidyverse)
library(osmdata)
library(ggplot2)
library(sf)

available_features()

available_tags("highway")

roads <- getbb("Liverpool UK")%>%
  opq(timeout = 3500)%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary", "residential",
                            "living_street", "unclassified")) %>%
  osmdata_sf()

getbb("Liverpool UK")

#       min      max
#x -3.15168 -2.83168
#y 53.24720 53.56720

liverpoolmap1<-ggplot() +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "black",)+
  coord_sf(xlim = c(-3.15168, -2.83168), 
           ylim = c(53.24720, 53.56720),
           expand = FALSE) 
liverpoolmap1

available_features()
available_tags("natural")

coast <- getbb("Liverpool UK")%>%
  opq(timeout = 3500)%>%
  add_osm_feature(key = "natural", 
                  value = c("coastline")) %>%
  osmdata_sf()

liverpoolmap2<-ggplot() +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "black",)+
  geom_sf(data = coast$osm_lines,
          inherit.aes = FALSE,
          color = "black",)+
  coord_sf(xlim = c(-3.15168, -2.83168), 
           ylim = c(53.24720, 53.56720),
           expand = FALSE) 
liverpoolmap2

#


liverpoolmap3<-ggplot() +
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "black",)+
  geom_sf(data = coast$osm_lines,
          inherit.aes = FALSE,
          color = "black",)+
  coord_sf(xlim = c(-3.15168, -2.83168), 
           ylim = c(53.24720, 53.56720),
           expand = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_line(colour = "transparent"))+
  theme(
    plot.background = element_rect(fill = "gray11")
  )
liverpoolmap3

#






