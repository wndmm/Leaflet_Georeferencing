# VISUALISASI HASIL SENTIMENT
install.packages("maps")
install.packages("mapproj")
install.packages("tmaptools")
install.packages("sf")
install.packages("openintro")
install.packages("palmerpenguins")
install.packages("ggmap")
install.packages("tidycensus")
install.packages("leaflet.extras")


library(tidycensus)
library(tidyverse)
library(maps)
library(mapproj)
library(ggplot2)
library(sf)
library(tmaptools)
library(leaflet)
library(dplyr)
library(googlesheets4)
library(lubridate)
library(openintro)
library(palmerpenguins)
library(ggthemes)
library(ggmap)
library(leaflet.extras)


# DATA LOAD
travelokabdgmap <- read.csv(file.choose(), header = T) %>%
select(27:34, landmarknear)

# LOAD MAP
bandungmapI <- get_stamenmap(bbox = c(left = 107.5060 , bottom = -7.0426, right = 107.7546 , top = -6.7976),
                            maptype = "terrain",
                            zoom = 12)
                            ggmap(bandungmapI)

# FILL THE MAP
ggmap(bandungmapI) + geom_point(data = travelokabdgmap,
                     aes(x = latitude, y = longitude), size = .8, color = "dodgerblue") + theme_map()


# ZOOMING THE MAP karena ga ke cover
kawahputihadd <- get_stamenmap(bbox = c(left = 107.2351 , bottom = -7.386, right = 107.9695 , top = -6.8437),
                            maptype = "terrain",
                            zoom = 10)
                            ggmap(kawahputihadd)

# FILL THE MAP
ggmap(kawahputihadd) + geom_point(data = travelokabdgmap,
                                aes(x = latitude, y = longitude), size = .8, color = "dodgerblue") + theme_map()


# ---------------------------- #
# GEOREFERENCING

travelokabdgmapS <- read.csv(file.choose(), header = T) #%>%
select(15,16, 27:35)
#str(travelokabdgmapS)

# PLOT CIRCLES
travelokabdgmapS %>%
  leaflet(width = "100%") %>%
  addProviderTiles("Esri.WorldImagery") %>% addCircles(data = travelokabdgmapS, lat = ~longitude, lng = ~latitude, color = "dodgerblue
                    ")

# PLOT MARKERS
travelokabdgmapS %>%
  leaflet(width = "100%") %>%
  addProviderTiles("Esri.WorldImagery") %>% setView(107.622966, -6.899925, zoom = 10) %>% 
  addMarkers(lat = ~longitude,
             lng = ~latitude,
             popup = travelokabdgmapS$score)

# AGGREGATING DIFFERENT PLOT MARKERS
numunik <- travelokabdgmapS$score %>% unique() %>% length() 
nameunik <- travelokabdgmapS$score %>% unique() 

# SENTIMENT MARKER
Colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
# #edf8fb','#ccece6','#99d8c9','#66c2a4','#2ca25f','#006d2c
pal <- colorFactor(Colors, domain = nameunik)
m <- leaflet() %>% addProviderTiles("OpenStreetMap.Mapnik") %>% addCircleMarkers(data = travelokabdgmapS, lat = ~longitude, 
                                            lng = ~latitude, color = ~pal(score), 
                                            fillOpacity = 0.20, popup = ~score, label = ~propertyinfo, 
                                            group = "Sentiment Maps") %>% addLegend(data = travelokabdgmapS, "bottomright", pal = pal,
                                            values = ~score,  title = "Sentimen Score Maps", opacity = 1, group = "Leyenda")

m
# HEATMAP
travelokabdgmapS %>%
  leaflet() %>%
  addProviderTiles("Stamen.Toner") %>%
addHeatmap(lng = ~latitude, lat = ~longitude, blur = 10, max = 10, radius = 30)

bringtheheat = function(intensity = 2, max = 4, blur = 15, radius = 30){
  travelokabdgmapS %>%
    leaflet() %>%
    addProviderTiles("Stamen.Toner") %>%
    addHeatmap(lng = ~latitude, lat = ~longitude, blur = blur, 
               max = max, radius = radius, intensity = intensity,)
}
bringtheheat()
bringtheheat(intensity = 8, max = 15, blur = 10, radius = 8)


