library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tidyverse)
library(shinyjs)
library(shiny)


library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(htmltools)



# Load Data ---------------------------------------------------------------


holmewood_msoa <- st_read("Holmewood_MSOA.shp")
BD_Postcode_Units <- read_csv("ONSPD_NOV_2020_UK_BD.csv")
HW_Access_Metrics <- read_csv("HW_Access_Metrics.csv")


# Data processing ---------------------------------------------------------

HW_Postcode_Units <- BD_Postcode_Units %>%
  filter(msoa01 == "E02002234")


#Converts point geographic features from BNG Easting and Northing to WGS LngLat
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

# FEAT_Leaflet <- FEATOutlets_WY
# FEAT_Leaflet$FEAT_ID <-1:nrow(FEAT_Leaflet)
# FEAT_Leaflet_SF <-st_as_sf(FEAT_Leaflet, 
#                            coords= c("feature_easting",
#                                      "feature_northing"), 
#                            crs= ukgrid)
# FEAT_Leaflet_SF_LL <- st_transform(FEAT_Leaflet_SF, crs = latlong)

holmewood_msoa_leaflet <-st_transform(holmewood_msoa, crs = latlong)



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "postcode", label = "Enter your postcode"),
      verbatimTextOutput(outputId = "postcodeinfo"),
      ),
    mainPanel(
      leafletOutput("mymap")
      )
  )
)




server <- function(input,output,session) {
  
 filtered_postcode_df <- reactive({
   
   req(input$postcode)
   filtered_HW_postcodes <- HW_Access_Metrics %>% 
     filter(pcd == input$postcode)
 })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% #Add default OpenStreetMap map tiles
      addCircles(data = HW_Access_Metrics, 
                 label = htmlEscape(HW_Access_Metrics$pcd)) %>%
      addPolygons(data = holmewood_msoa_leaflet,
                  color = "Red", weight = 2, fillOpacity = 0)
    #addCircles(data = FEAT_Leaflet_SF_LL, label = htmlEscape(paste("Name: ",FEAT_Leaflet_SF_LL$name," Type: ",FEAT_Leaflet_SF_LL$FEATClass)))
  })
  
  output$postcodeinfo <- renderText({
    paste(paste("Info about ", 
                input$postcode),
          paste("Distance to Closest Supermarket (km): ", 
                round(filtered_postcode_df()$closest_Supermarket,digits = 2)),
          paste("Mean Distance of Closest 3 Supermarkets (km): ", 
                round(filtered_postcode_df()$mean_closest_3_Supermarket,digits = 2)),
          paste("Mean Distance of Closest 5 Supermarkets (km): ", 
                round(filtered_postcode_df()$mean_closest_5_Supermarket,digits = 2)),
          paste("Number of Supermarkets within 400m: ", 
                round(filtered_postcode_df()$Supermarket_count400m,digits = 2)),
          paste("Number of Supermarkets within 800m: ", 
                round(filtered_postcode_df()$Supermarket_count800m,digits = 2)),
          paste("Number of Supermarkets within 1km: ",
                round(filtered_postcode_df()$Supermarket_count1km,digits = 2)),
          paste("Number of Supermarkets within 1.6km: ",
                round(filtered_postcode_df()$Supermarket_count1.6km,digits = 2)),
          paste("Number of Supermarkets within 2km: ",
                round(filtered_postcode_df()$Supermarket_count2km,digits = 2)),
          paste("Distance to Closest Takeaway (km): ", 
                round(filtered_postcode_df()$closest_Takeaways,digits = 2)),
          paste("Mean Distance of Closest 3 Takeaways (km): ", 
                round(filtered_postcode_df()$mean_closest_3_Takeaways,digits = 2)),
          paste("Mean Distance of Closest 5 Takeaways (km): ", 
                round(filtered_postcode_df()$mean_closest_5_Takeaways,digits = 2)),
          paste("Number of Takeaways within 400m: ", 
                round(filtered_postcode_df()$Takeaways_count400m,digits = 2)),
          paste("Number of Takeaways within 800m: ", 
                round(filtered_postcode_df()$Takeaways_count800m,digits = 2)),
          paste("Number of Takeaways within 1km: ",
                round(filtered_postcode_df()$Takeaways_count1km,digits = 2)),
          paste("Number of Takeaways within 1.6km: ",
                round(filtered_postcode_df()$Takeaways_count1.6km,digits = 2)),
          paste("Number of Takeaways within 2km: ",
                round(filtered_postcode_df()$Takeaways_count2km,digits = 2)),
          sep = "\n")
  })
  
  observeEvent(input$postcode,{
    leafletProxy("mymap", data = filtered_postcode_df()) %>%
      clearShapes() %>%
      addCircles(data = filtered_postcode_df(),
                 color = "Red",
                 fillColor = "red",
                 fillOpacity = 0.5)
  })

}

shinyApp(ui,server)