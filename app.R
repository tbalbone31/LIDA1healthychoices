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
BD_Postcode_Units <- read_csv("Bradford_Postcode_Units.csv")
HW_Access_Metrics <- read_csv("HW_Access_Metrics.csv")
ahah_efood_combined_Bradford <- st_read("ahah_efood_combined_Bradford.shp")

pcd <- HW_Access_Metrics$pcd

HW_LSOA <- c("Bradford 052A",
             "Bradford 052B",
             "Bradford 052C",
             "Bradford 052D",
             "Bradford 052E")

HW_MSOA <- "Bradford 052"

overlaychoices <- c("None",
                    "AHAH v2 Retail Domain",
                    "E-Food Deserts Index",
                    "Childhood Obesity")





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

holmewood_msoa_leaflet <- st_transform(holmewood_msoa, crs = latlong)
ahah_efood_combined_Bradford_leaflet <- st_transform(ahah_efood_combined_Bradford, crs = latlong)

HW_ahah_efood_combined <- ahah_efood_combined_Bradford_leaflet %>%
  filter(LSOA11N %in% HW_LSOA)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "overlay",
                  label = "Choose an overlay to display",
                  choices = overlaychoices,
                  selected = "None"),
      htmlOutput(outputId = "overlayinfo"),
      
      selectInput(inputId = "postcode", 
                  label = "Enter your postcode (type or scroll)",
                  choices = c("",
                              HW_Postcode_Units$pcd),
                  selected = NULL
                  ),
      conditionalPanel(
        condition = "output.panelStatus",
        radioButtons(inputId = "pcdmetcht",
                   label = "Select your Access Metrics",
                   choices = c("Closest Outlet" = "closest",
                               "Average of Nearest 3 Outlets" = "mean3",
                               "Average of Nearest 5 Outlets" = "mean5",
                               "Number of Outlets within 500m" = "count500m",
                               "Number of Outlets within 1km" = "count1km",
                               "Number ot Outlets within 1.6m (Approx 1 mile" = "count1.6km",
                               "Number of Outlets within 2km" = "count2km")
                   ),
      plotOutput(outputId = "postcodemetrics")
      )),
    mainPanel(
      leafletOutput("mymap", height = "80vh")
      )
  )
)




server <- function(input,output,session) {

  
 filtered_postcode_df <- reactive({
   
   req(input$postcode)
   filtered_HW_postcodes <- HW_Access_Metrics %>% 
     filter(pcd == input$postcode)
  
   
 })
 
 pcd_met_chart_df <-reactive({
   
  pcd_met_chart <- switch(input$pcdmetcht,
          "closest" = data.frame(
                        id = seq(1,2,1),
                        name = c("Closest Supermarket",
                                 "Closest Takeaway") ,
                        
                        value = c(filtered_postcode_df()$closest_Supermarket,
                                  filtered_postcode_df()$closest_Takeaways)
                                ),
          "mean3" = data.frame(
                        id = seq(1,2,1),
                        name = c("Average of Nearest 3 Supermarkets",
                                 "Average of Nearest 3 Takeaways") ,
                        
                        value = c(filtered_postcode_df()$mean_closest_3_Supermarket,
                                  filtered_postcode_df()$mean_closest_3_Takeaways)
          ) ,
          "mean5"= data.frame(
            id = seq(1,2,1),
            name = c("Average of Nearest 5 Supermarket",
                     "Average of Nearest 5 Takeaways") ,
            
            value = c(filtered_postcode_df()$mean_closest_5_Supermarket,
                      filtered_postcode_df()$mean_closest_5_Takeaways)
          ),
          "count500m"= data.frame(
            id = seq(1,2,1),
            name = c("Number of Supermarkets within 500m",
                     "Number of Takeaways within 500m") ,
            
            value = c(filtered_postcode_df()$Supermarket_count400m,
                      filtered_postcode_df()$Takeaways_count400m)
          ),
          "count1km"= data.frame(
            id = seq(1,2,1),
            name = c("Number of Supermarkets within 1km",
                     "Number of Takeaways within 1km") ,
            
            value = c(filtered_postcode_df()$Supermarket_count1km,
                      filtered_postcode_df()$Takeaways_count1km)
          ),
          "count1.6km" = data.frame(
            id = seq(1,2,1),
            name = c("Number of Supermarkets within 1.6km",
                     "Number of Takeaways within 1.6km") ,
            
            value = c(filtered_postcode_df()$Supermarket_count1.6km,
                      filtered_postcode_df()$Takeaways_count1.6km)
          ),
          "count2km" = data.frame(
            id = seq(1,2,1),
            name = c("Number of Supermarkets within 2km",
                     "Number of Takeaways within 2km") ,
            
            value = c(filtered_postcode_df()$Supermarket_count2km,
                      filtered_postcode_df()$Takeaways_count2km)
          ))
   
 })
 
 output$panelStatus <- reactive({
    input$postcode != ""
 })
 
 outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
 
 Currentpostcode <- reactive ({
   pcdID <- input$mymap_marker_click
   pcdID <- pcdID$id
 })
 
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% #Add default OpenStreetMap map tiles
      addPolygons(data = holmewood_msoa_leaflet,
                  color = "Red", 
                  weight = 2, 
                  fillOpacity = 0,
                  group = "Holme Wood Boundary") %>%
      addCircleMarkers(data = HW_Access_Metrics,
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                       zoomToBoundsOnClick = TRUE),
                 popup = pcd,
                 label = pcd,
                 layerId = pcd,
                 group = "Holme Wood Postcodes") %>%
      addLayersControl(
        overlayGroups = "Holme Wood Postcodes",
        options = layersControlOptions(collapsed = FALSE)
      )
    #addCircles(data = FEAT_Leaflet_SF_LL, label = htmlEscape(paste("Name: ",FEAT_Leaflet_SF_LL$name," Type: ",FEAT_Leaflet_SF_LL$FEATClass)))
  })
  
  # Creates a barchart of the postcode metrics that updates when a unit is 
  # selected or typed into the search function.
  output$postcodemetrics <- renderPlot({
  
    

  ggplot(pcd_met_chart_df(), aes(x = id, y= value)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_manual(values = c("Green", "Blue", "Orange", "Red")) +
    scale_x_continuous(breaks = pcd_met_chart_df()$id, labels = pcd_met_chart_df()$name)
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
  
  output$overlayinfo <- renderText({
    
    case_when(
      input$overlay == "None" ~ paste(
        sep = "</br></br>",
        "<b>No overlay</b>",
        "There are no overlays showing.",
        
        "The red boundary line shows an administrative boundary known as a 
        Middle Layer Super Output Area (MSOA).  The research team have defined 
        this as the boundary to Holme Wood."
      ), 
      input$overlay == "AHAH v2 Retail Domain" ~ paste(
        sep = "</br></br>",
        "<b>Access to Healthy Assets and Hazards (AHAH) v2</b>",
        
        "The red boundary line shows an administrative boundary known as a 
        Middle Layer Super Output Area (MSOA).  The research team have defined 
        this as the boundary to Holme Wood.",
        
        "The blue boundaries show administrative boundaries known as Lower Layer
        Super Output Areas (LSOA).  There are 5 of these within Holme Wood.",
        
        "Mapped to these are AHAH v2 Retail Domain shows access to retail outlets 
        (fast food outlets, pubs, off-licences, tobacconists, gambling outlets).",
        
        "Selecting a LSOA will tell you it's designation and which 
        Retail Domain Decile it falls under.  Deciles split the ranked data in 
        10% intervals with decile 1 representing the best performing and 10 
        the worst."
        
      ), 
      input$overlay == "E-Food Deserts Index" ~ paste(
        sep = "</br></br>",
        "<b>E-Food Deserts Index</b>",
        
        "The red boundary line shows an administrative boundary known as a 
        Middle Layer Super Output Area (MSOA).  The research team have defined 
        this as the boundary to Holme Wood.",
        
        "The blue boundaries show administrative boundaries known as Lower Layer
        Super Output Areas (LSOA).  There are 5 of these within Holme Wood.",
        
        "Mapped to these are E-Food Desert Deciles which measures the extent to 
        which neighbourhoods exhibit characteristics associated with food deserts.",
        
        "Selecting a LSOA will tell you it's designation and which 
        E-Food Desert Decile it falls under.  Deciles split the ranked data in 
        10% intervals with decile 1 representing the worst performing and 10 
        the best"
        
      )
    )
  })
  
  
  observeEvent(input$overlay, {
    req(input$overlay, input$overlay == "None")
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = holmewood_msoa_leaflet,
                  color = "Red", weight = 2, fillOpacity = 0)
    
  })
  
  observeEvent(input$overlay, {
    req(input$overlay, input$overlay == "AHAH v2 Retail Domain")
    leafletProxy("mymap") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = holmewood_msoa_leaflet,
                  color = "Red", weight = 2, fillOpacity = 0, group = "Polygons") %>%
      addPolygons(data = HW_ahah_efood_combined,
                  group = "Polygons",
                  weight = 2,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 1.5,
                                                      bringToFront = TRUE),
                  popup = ~paste(sep = "<br/>",
                                paste("<b>",LSOA11N,"</b>"),
                                paste(sep = "",
                                      "Retail Domain Decile: ", r_dec)
                             )
                                            
                  ) %>%
    addLayersControl(
      baseGroups = "Polygons",
      overlayGroups = "Holme Wood Postcodes",
      options = layersControlOptions(collapsed = FALSE)
      )
    
      })

  
  observeEvent(input$overlay, {
    req(input$overlay, input$overlay == "E-Food Deserts Index")
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = holmewood_msoa_leaflet,
                  color = "Red", weight = 2, fillOpacity = 0) %>%
      addPolygons(data = HW_ahah_efood_combined,
                  group = "E-Food Deserts Index",
                  weight = 2,
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 1.5,
                                                      bringToFront = TRUE),
                  popup = ~paste(sep = "<br/>",
                                 paste("<b>",LSOA11N,"</b>"),
                                 paste(sep = "",
                                       "E-Food Deserts Decile: ", efd_dcl)
                                 
                  )
                  
      )
    
  })
  
  observe({
    
    updateTextInput(session, "postcode", value = Currentpostcode())
    
  })
  

  # observeEvent(input$postcode,{
  #   leafletProxy("mymap", data = filtered_postcode_df()) %>%
  #     clearShapes() %>%
  #     addCircles(data = filtered_postcode_df(),
  #                color = "Red",
  #                fillColor = "red",
  #                fillOpacity = 0.5)
  # })

}

shinyApp(ui,server)