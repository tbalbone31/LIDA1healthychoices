library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tidyverse)
library(shinyjs)
library(shiny)
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(htmltools)
library(htmlwidgets)
library(fontawesome)



# Load Data ---------------------------------------------------------------

#MSOA shapefile clipped to Bradford Local Authority
BD_msoa <- st_read("Bradford_MSOA.shp")

#All postcode units within the Bradford Local Authority.
BD_PCD <- read_csv("Bradford_Postcode_Units.csv")

#Access measures calculated for Bradford postcodes in separate script.
BD_Acc <- read_csv("BD_Access_Metrics.csv")

#Combined AHAH and E-Food deserts index
BD_ahah_efood <- st_read("ahah_efood_combined_Bradford.shp")

#postcode unit characters from dataframe
pcd <- BD_Acc$pcd

#Vector of LSOA contained within the HolmeWood study area
HW_LSOA <- c("Bradford 052A",
             "Bradford 052B",
             "Bradford 052C",
             "Bradford 052D",
             "Bradford 052E")

#character representing the Holme wood MSOA
HW_MSOA <- "Bradford 052"

#Vector containing the overlay choices for selectinput feature.
overlaychoices <- c("None",
                    "AHAH v2 Retail Domain",
                    "E-Food Deserts Index",
                    "Childhood Obesity",
                    "Food Insecurity Risk")

#Converts point geographic features from BNG Easting and Northing to WGS LngLat
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"


# Data processing ---------------------------------------------------------

#Transforms shapefules into correct CRS for rendering in leaflet.
BD_MSOA_lf <- st_transform(BD_msoa, crs = latlong)
BD_ahah_efood_lf <- st_transform(BD_ahah_efood, crs = latlong)

#Create HolmeWood subsets from Bradford files
HW_ahah_efood_lf <- BD_ahah_efood_lf %>%
  filter(LSOA11N %in% HW_LSOA)

HW_MSOA_lf <- BD_MSOA_lf %>%
  filter(code == "E02002234")

HW_msoa <- BD_msoa %>%
  filter(code == "E02002234")

HW_PCD <- BD_PCD %>%
  filter(msoa01 == "E02002234")

HW_Acc <- BD_Acc %>%
  filter(msoa01 == "E02002234")

HWpcd <- HW_Acc$pcd



#R Shiny Server function
function(input,output,session) {
  
  #Sends the choice of postcode unit to the selectinput feature in UI
  updateSelectInput(session, "postcode", choices = c("",HW_PCD$pcd))
  
  #Sends the choice of overlay to the selectinput feature in UI
  updateSelectInput(session, "overlay", choices = overlaychoices)
  
  #Creates a dataframe containing a single postcode in order to plot correct
  #access measures
  filtered_postcode_df <- reactive({
    
    req(input$postcode)
    filtered_HW_postcodes <- HW_Acc %>% 
      filter(pcd == input$postcode)
    
    
  })
  
  
  #Creates data frame containing only relevant access measures for a single
  #postcode
  pcd_met_chart_df <-reactive({
    
    name <- c("Supermarkets",
              "Speciality Outlets",
              "Convenience Stores",
              "Takeaways")
    
    #Switch statement that combines the relevant measures for the four outlet types
    #for a single measure.
    pcd_met_chart <- switch(input$pcdmetcht,
                            "closest" = data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$cls_Sup,
                                        filtered_postcode_df()$cls_Spec,
                                        filtered_postcode_df()$cls_ConSt,
                                        filtered_postcode_df()$cls_Tway)
                            ),
                            "mean3" = data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$mean3_Sup,
                                        filtered_postcode_df()$mean3_Spec,
                                        filtered_postcode_df()$mean3_ConSt,
                                        filtered_postcode_df()$mean3_Tway)
                            ),
                            "mean5"= data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$mean5_Sup,
                                        filtered_postcode_df()$mean5_Spec,
                                        filtered_postcode_df()$mean5_ConSt,
                                        filtered_postcode_df()$mean5_Tway)
                            ),
                            "count500m"= data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$ct500_Sup,
                                        filtered_postcode_df()$ct500_Spec,
                                        filtered_postcode_df()$ct500_ConSt,
                                        filtered_postcode_df()$ct500_Tway)
                            ),
                            "count1km"= data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$ct1000_Sup,
                                        filtered_postcode_df()$ct1000_Spec,
                                        filtered_postcode_df()$ct1000_ConSt,
                                        filtered_postcode_df()$ct1000_Tway)
                            ),
                            "count1.6km" = data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$ct1600_Sup,
                                        filtered_postcode_df()$ct1600_Spec,
                                        filtered_postcode_df()$ct1600_ConSt,
                                        filtered_postcode_df()$ct1600_Tway)
                            ),
                            "count2km" = data.frame(
                              id = seq(1,4,1),
                              name = name,
                              value = c(filtered_postcode_df()$ct2000_Sup,
                                        filtered_postcode_df()$ct2000_Spec,
                                        filtered_postcode_df()$ct2000_ConSt,
                                        filtered_postcode_df()$ct2000_Tway)
                            )
    ) 
    
  })
  
  output$panelStatus <- reactive({
    input$postcode != ""
  })
  
  
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  
  #Changes the postcode in the selectinput to match a point clicked in the map.
  Currentpostcode <- reactive ({
    pcdID <- input$mymap_marker_click
    pcdID <- pcdID$id
  })
  
  
  #Renders a dyanmic leaflet map.
  output$mymap <- renderLeaflet({
      

      leaflet() %>%
        addTiles() %>% #Add default OpenStreetMap map tiles
      
        #Adds MSOA polygon to the map as a default layer
        addPolygons(data = HW_MSOA_lf,
                    color = "Red", 
                    weight = 2, 
                    fillOpacity = 0,
                    group = "Holme Wood Boundary",
                    layerId = "holmewoodmsoa") %>%
      
      #Adds postcode units as circle markers and clusters
        addCircleMarkers(data = HW_Acc,
                         clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                               zoomToBoundsOnClick = TRUE),
                         popup = HWpcd,
                         label = HWpcd,
                         layerId = HWpcd,
                         group = "Holme Wood Postcodes") %>%
      #Adds a layer control to the map
        addLayersControl(
          overlayGroups = "Holme Wood Postcodes","overlay",
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
      
      #Adds an easybutton to allow fast changing of map scale
      addEasyButton(easyButton(
        
        states = list(
          easyButtonState(
                stateName = 'zoom-to-bradford',
                icon = 'fa-city',
                title = 'Zoom to Bradford Local Authority',
                onClick = JS("
                function(btn, map) {
                  map.setView([53.841995,-1.837950],10.5);
                  btn.state('zoom-to-holmewood');
                }")
          ),
          easyButtonState(
                stateName = 'zoom-to-holmewood',
                icon = 'fa-home',
                title = 'Zoom to Holme Wood',
                onClick = JS("
                function(btn,map) {
                  map.setView([53.773370,-1.709840],15);
                  btn.state('zoom-to-bradford');
                }")
          )
          )
      )
      )
  })
                  

  
  # Creates a barchart of the postcode metrics that updates when a unit is 
  # selected or typed into the search function.
  output$postcodemetrics <- renderPlot({
    
    #switch statement to dynamically change label
    ylabel <- switch(input$pcdmetcht,
                     "Closest Outlet" = "Distance (km)",
                     "Average of Nearest 3 Outlets" = "Distance (km)",
                     "Average of Nearest 5 Outlets" = "Distance (km)",
                     "Number of Outlets within 500m" = "Count within 500m",
                     "Number of Outlets within 1km" = "Count within 1km",
                     "Number of Outlets within 1.6km (Approx 1 mile" = "Count within 1.6km",
                     "Number of Outlets within 2km" = "Count within 2km")
    
    
    
    ggplot(pcd_met_chart_df(), aes(x = id, y= value, fill=as.factor(value))) +
      geom_bar(stat = "identity",width = 0.5) +
      scale_fill_manual(values = c("green","blue","orange","red")) +
      scale_x_continuous(breaks = pcd_met_chart_df()$id, labels = pcd_met_chart_df()$name) +
      theme(legend.position="none") +
      xlab("Food Outlet Category") + ylab(~ylabel)
  })
  
  

  #Dynamic overlay information text that changes whenever an overlay is selected.
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
        the worst.",
        
        "See the Data tab for further info and sources"
        
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
        the best.",
        
        "See the Data tab for further info and sources"
        
      ),
      input$overlay == "Childhood Obesity" ~ paste(
        sep = "</br></br>",
        "<b>Childhood Obesity (National Child Measurement Programme)</b>",
        
        "The red boundary line shows an administrative boundary known as a 
        Middle Layer Super Output Area (MSOA).  The research team have defined 
        this as the boundary to Holme Wood.",
        
        "Also mapped to these MSOAs are Year 6 childhood obesity statistics from the National Child Measurement Programme.",
        
        "Selecting an MSOA will show you the rates of obesity for that area from the 2017/18 - 19/20 time period of data collection.",
        
        "See the Data tab for further info and sources"
    ),
    input$overlay == "Food Insecurity Risk" ~ paste(
      sep = "</br></br>",
      "<b>Food Insecurity Risk</b>",
      
      "The red boundary line shows an administrative boundary known as a 
        Middle Layer Super Output Area (MSOA).  The research team have defined 
        this as the boundary to Holme Wood.",
      
      "Also mapped to these MSOAs are data for the total population at higher risk of food insecurity (as percentages).",
      
      "Selecting an MSOA will show you the rates of food insecurity risk for that area.",
      
      "See the Data tab for further info and sources"
    )
    )
  })

# Redraw map objects based on user selection ------------------------------

  #Sets a leaflet proxy for changing map elements without redrawing entire map.
  proxy <- leafletProxy("mymap")
  
   #Redraw map objects based on user selecting 'None' for overlay
  observeEvent(input$overlay, {
    req(input$overlay, input$overlay == "None")
    proxy %>%
      clearGroup("overlay") %>%
      clearControls() %>%
      addLayersControl(
        overlayGroups = "Holme Wood Postcodes",
        options = layersControlOptions(collapsed = FALSE))

  })

  

  # Redraw map objects based on user selection
  observeEvent(input$overlay, {
    req(input$overlay, input$overlay != "None")
  
  #Creates dynamic labels for the overlay polygons
  labels <- switch(input$overlay,
          "AHAH v2 Retail Domain" = sprintf(
            "<strong> LSOA Name: %s </strong> <br/> Decile: %g",
            BD_ahah_efood_lf$LSOA11N, BD_ahah_efood_lf$r_dec)
          %>% lapply(htmltools::HTML),

          "E-Food Deserts Index" = sprintf(
            "<strong> LSOA Name: %s </strong> <br/> Decile: %g",
            BD_ahah_efood_lf$LSOA11N, BD_ahah_efood_lf$efd_dcl)
          %>% lapply(htmltools::HTML),
          
          "Childhood Obesity" = sprintf(
            "<strong> MSOA Name: %s </strong> <br/> Obesity Prevalance: %g",
            BD_MSOA_lf$name, BD_MSOA_lf$chldbs_v)
          %>% lapply(htmltools::HTML),
          
          "Food Insecurity Risk" = sprintf(
            "<strong> MSOA Name: %s </strong> <br/> Food Insecurity Risk: %g",
            BD_MSOA_lf$name, BD_MSOA_lf$fdns_vl)
          %>% lapply(htmltools::HTML)
          

    )
  
  #Creates a dynamic colour palette for the overlay polygons
  pal <-  switch(input$overlay,
             "AHAH v2 Retail Domain" = colorFactor(
               palette = "RdYlBu",
               domain = BD_ahah_efood_lf$r_dec,
               reverse = TRUE),
             
             "E-Food Deserts Index" = colorFactor(
               palette = "RdYlBu",
               domain = BD_ahah_efood_lf$efd_dcl,
               reverse = TRUE),
             
             "Childhood Obesity" = colorBin(
               palette = "Blues",
               domain = BD_MSOA_lf$chldbs_v,
               5,
               pretty = FALSE),
             
             "Food Insecurity Risk" = colorBin(
               palette = "Blues",
               domain = BD_MSOA_lf$fdns_vl,
               5,
               pretty = FALSE)
  )
  
  #Loads a column vector based on user selection
  overlayvalues <- switch(input$overlay,
                     "AHAH v2 Retail Domain" = BD_ahah_efood_lf$r_dec,
                     "E-Food Deserts Index" = BD_ahah_efood_lf$efd_dcl,
                     "Childhood Obesity" = BD_MSOA_lf$chldbs_v,
                     "Food Insecurity Risk" = BD_MSOA_lf$fdns_vl)
    
             
  #Loads a shapefile based on user selection
  shp <- switch(input$overlay,
                "AHAH v2 Retail Domain" = BD_ahah_efood_lf,
                "E-Food Deserts Index" = BD_ahah_efood_lf,
                "Childhood Obesity" = BD_MSOA_lf,
                "Food Insecurity Risk" = BD_MSOA_lf)
  
  #Loads a legend variable to set a dyanmic legend.
  legend <- switch(input$overlay,
                   "AHAH v2 Retail Domain" = "AHAH v2 Retail Domain Deciles",
                   "E-Food Deserts Index" = "E-Food Deserts Index Deciles",
                   "Childhood Obesity" = "Child Obesity Prevalance (%)",
                   "Food Insecurity Risk" = "Food Insecurity Risk 2020 - Total higher risk individuals (%)")
    
  

  
    #uses the leaflet proxy to change the map features and rebuild layers in
  #correct order without reconstructing entire map.
      proxy %>%
        clearGroup("overlay") %>%
        clearGroup("Holme Wood Postcodes") %>%
        clearControls() %>%
        addPolygons(data = shp,
                    color = ~pal(overlayvalues),
                    weight = 2,
                    fillOpacity = 0.5,
                    group = "overlay",
                    highlightOptions = highlightOptions(color = "#666",
                                                        weight = 1.5,
                                                        bringToFront = FALSE,
                                                        fillOpacity = 0.75),
                    label = labels) %>%
        addCircleMarkers(data = HW_Acc,
                         clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                               zoomToBoundsOnClick = TRUE),
                         popup = HWpcd,
                         label = HWpcd,
                         layerId = HWpcd,
                         group = "Holme Wood Postcodes") %>%
        addLegend(pal = pal,
                  title = legend,
                  values = overlayvalues,
                  opacity = 0.75,
                  position = "bottomright")

  })
  
  
  observe({
    
    updateSelectInput(session, "postcode", selected = Currentpostcode())
    
  })
  