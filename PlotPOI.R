library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tidyverse)
library(shinyjs)
library(shiny)


library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(htmltools)
library(osrm)
library(stplanr)



# Load Data ---------------------------------------------------------------



england_counties <- st_read("./Boundary Data/England_ct_2011/england_ct_2011.shp")
england_imd2019 <- st_read("./Boundary Data/IMD_2019/IMD_2019.shp")
england_msoa <- st_read("./Boundary Data/England_msoa_2011/england_msoa_2011.shp")
FEATOutlets <- read_csv("./Point Data/FEAT_Food_Outlets.csv")
HW_Postcode_Units <- read_csv(
  "./Point Data/Holmewood_Postcode_Units.csv", 
  col_types = cols(
    osnrth1m = col_double()
  )
)





# Data processing ---------------------------------------------------------


#Filters Food outlets by West Yorkshire County
FEATOutlets_WY <- FEATOutlets %>%
  filter(geographic_county == "West Yorkshire")

#Filters down on English counties and stores Bradford as a variable
Bradford_county <- england_counties %>%
  filter(name=="Bradford")

Bradford_IMD_2019 <- england_imd2019 %>%
  filter(LADnm=="Bradford")



#Sets BNGCRS as the coordinate system (British National Grid - OSGB36)
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

#Converts the FEAT outlets to an sf object and assigns coordinate references
plot_locations_FEAT <-st_as_sf(FEATOutlets_WY, 
                               coords= c("feature_easting",
                                         "feature_northing"), 
                               crs= ukgrid)

plot_locations_postcodeunit <-st_as_sf(HW_Postcode_Units,
                                       coords = c("oseast1m",
                                                  "osnrth1m"),
                                       crs = ukgrid)




           




#Converts point geographic features from BNG Easting and Northing to WGS LngLat

FEAT_Leaflet <- FEATOutlets_WY
FEAT_Leaflet$FEAT_ID <-1:nrow(FEAT_Leaflet)
FEAT_Leaflet_SF <-st_as_sf(FEAT_Leaflet, 
                           coords= c("feature_easting",
                                     "feature_northing"), 
                           crs= ukgrid)
FEAT_Leaflet_SF_LL <- st_transform(FEAT_Leaflet_SF, crs = latlong)

holmewood_msoa_leaflet <-st_transform(holmewood_msoa, crs = latlong)


# Mapping -----------------------------------------------------------------

#Creates a map variable for Bradford and adds the county polygon.
map_bradford <- tm_shape(Bradford_county) + tm_polygons() + tm_shape(OD_Lines) + tm_lines()
class(map_bradford)
map_bradford

IMD_legend <-expression("Deprevation Decile")

#creates map object for IMD classification
map_bradford_IMD_2019 <- map_bradford +
  tm_shape(Bradford_IMD_2019, unit = "mi") + 
  tm_polygons(col = "IMD_Decile", 
          title = IMD_legend,
          style = "cat",
          palette = "-GnBu") +
  tm_compass(position = c("left","top")) +
  tm_scale_bar(position = c("left","bottom")) +
  tm_layout(legend.position = c("right","top"), inner.margins = 0.1)

class(map_bradford_IMD_2019)
map_bradford_IMD_2019

#Create map object for postcode units
map_holmewood <-tm_shape(holmewood_msoa) + 
  tm_polygons(border.col = "Red") + 
  tm_shape(plot_locations_postcodeunit) + 
  tm_dots()
class(map_holmewood)
map_holmewood

#Create visualisation of Desire Lines



# Interactive Leaflet Map -------------------------------------------------


m = leaflet() %>%
  addTiles() %>% #Add default OpenStreetMap map tiles
  addCircles(data = HW_Postcode_Units, 
             label = htmlEscape(HW_Postcode_Units$pcd)) %>%
  addPolygons(data = holmewood_msoa_leaflet,
              color = "Red", weight = 2, fillOpacity = 0)
  #addCircles(data = FEAT_Leaflet_SF_LL, label = htmlEscape(paste("Name: ",FEAT_Leaflet_SF_LL$name," Type: ",FEAT_Leaflet_SF_LL$FEATClass)))
m
