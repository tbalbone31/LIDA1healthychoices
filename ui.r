library(leaflet) # for interactive maps

fluidPage(
  
  titlePanel("Holme Wood Community Toolkit"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "overlay",
                  label = "Choose an overlay to display",
                  choices = NULL,
                  selected = "None"),
      htmlOutput(outputId = "overlayinfo"),
      
      br(),
      
      selectInput(inputId = "postcode", 
                  label = "Enter your postcode (type or scroll)",
                  choices = NULL,
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
                                 "Number of Outlets within 1.6m (Approx 1 mile" = "count1.6km",
                                 "Number of Outlets within 2km" = "count2km")
        ),
        plotOutput(outputId = "postcodemetrics")
      )),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",
                           leafletOutput("mymap", height = "80vh")),
                  tabPanel("User Guide",
                           h3("Community Toolkit User Guide"),
                           
                           p("This Community Toolkit has been developed for policymakers and local residents of the Holme Wood locality of Bradford.  It is a proof of concept."),
                           
                           p("The toolkit itself was designed to balance out providing users with a range of information with being as intuitive as possible.  This user guide explains the basic features of the toolkit."),
                           
                           br(),
                           
                           h4("Navigating the Map"),
                           p("When the toolkit launches, you will be faced with a map of Holme Wood with the relevant postcode units showing as an interactive layer and the boundary of the study area fixed onto the basemap.
                             The postcode units can be toggled on and off using the 'Holme Wood Postcodes' checkbox in the top right corner of the map panel. The study boundary cannot be removed."),
                           
                           p("The map can be navigated by clicking the left mouse button and dragging in the opposite direction to which you wish to travel.  The map is oriented with North at the top, so if you want to move North of Holme Wood then click, hold and drag the mouse down."),
                           
                            p("In the top left corner of the map are the zoom controls.  There is a standard '+/-' for increasing or decreasing the zoom level and a bookmark for two fixed views: Bradford Local Authority and Holme Wood.
                             Initially the icon is set to Bradford LA.  When clicked, it will change the view to display less detail but a larger area and the icon will change.  If clicked again, it will return to Holme Wood and display more detail.
                             Please note that the icon will only change when clicked, it will not respond to the user manually navigating around the map."),
                           
                           p("There are several other features of the map, such as overlays and postcode units, that offer interactivity, however these will be explained in the relevant sections below"),
                           
                           br(),
                           
                           h4("Changing the Overlay"),
                           
                           p("The map has several overlays that can be loaded in with information relating to Holme Wood and the wider Bradford Local Authority.  By default this is set to not display any overlay."),
                             p("As this is a toolkit designed to help investigate the local food environment, all the information is relevant to this topic."),
                             p("To change overlay:"), 
                              tags$ol(
                                tags$li("Click the drop down menu on the left sidebar panel and choose an option.  For example, selecting 'AHAH v2 Retail Domain' will load in the Access to Healthy Assessts and Hazards Retail Domain Decile."),
                                tags$li("These data will appear on the map as individual shapes that represent UK administrative or census boundaries.  Hovering over these units will highlight them, clicking on one will reveal the unit code and any relevant information about it."),
                              ),
                                p("The legend in the bottom right corner will also give an idea of how different areas compare with each other."),
                             p("The specifics of each data source and relevant geographical boundaries are not covered here, however an explanation will load into the main screen when the overlay is changed.  More technical information about the data can be found in the data tab."),
                           
                           br(),
                           
                           h4("Access Measures"),
                          
                           p("The other main feature of the toolkit is to provide information about accessibility of the local food environment for each postcode unit.
                             There are two main ways this can be done:"),
                           tags$ol(
                             tags$li("The first is to search for the postcode in the drop down list in the left sidebar panel.  Although this appears as a simple drop down, the postcode can be typed into the box as well."),
                             tags$li("The second method is to click on the relevant postcode unit on the map.")
                             ),
                             p("Either of these two options will display a chart with accessibility measures relating to four different categories of food outlet: Supermarkets, Speciality Stores, Convenience Stores and Takeaways.
                             The selection box that appears above can be used to change the type of measure being displayed.  The measures are explained below:")
                             ),
                  
                  tabPanel("Data",
                           h3("Community Toolkit Data Guide"),
                          
                           p("This data guide outlines the data sources used in the toolkit and where they can be downloaded from if they are open data"),
                           
                           br(),
                           
                           h4("Access to Health Assets and Hazards (AHAH) Index"),
                           
                           p("The AHAH was developed by the CDRC and can be download from",
                             a(href="https://data.cdrc.ac.uk/dataset/access-healthy-assets-hazards-ahah","the CDRC website"),
                             "along with technical documentation and research papers."),
                           
                           p("The data visualised in this toolkit is the Retail Environment Domain which is included as a seperate variable within the csv datasets"),
                           
                           br(),
                           
                           h4("E-Food Deserts Index"),
                           
                           p("The E-Food Deserts Index was developed by the University of Leeds and can be downloaded from",
                             a(href="https://data.cdrc.ac.uk/dataset/e-food-desert-index","the CDRC website"),
                               "along with technical documentation."),
                           
                           p("The data visualised in this toolkit is the overall index generated after combining several indicators and domains.  These domains can each be visualised seperately as seperate variables within the csv dataset."),
                           
                           br(),
                           
                           h4("Childhood Obesity (National Child Measurement Programme)"),
                           
                           p("Childhood Obesity data is taken from the National Child Measurement Programme (NCMP) showing child obesity and excess weight for children in Reception and Year 6.  It can be downloaded from",
                             a(href="https://www.gov.uk/government/statistics/child-obesity-and-excess-weight-small-area-level-data","GOV.UK"),
                             "along with technical documentation and datasets for different geographies."),
                           
                           p("The data visualised in this toolkit is the Year 6 Obesity data which was captured for the time period covering 2017/18 - 19/20."),
                           
                           br(),
                           
                           h4("Food Insecurity Risk - Total Population"),
                           
                           p("Household Food Insecurity risk data was developed by Dianna Smith and a team at the University of Southampton.  It can be downloaded from",
                             a(href="https://mylocalmap.org.uk/iaahealth/","My Local Map"),
                             "and was created by calculating two seperate risk domains: Household Composition and Benefits.  The highest risk MSOAs overall are those in the top quintile (20%) for Household Composition and the highest risk group based on Benefits data."),
                           
                           p("The data visualised in this toolkit is the combined total population deemed to be most at risk of food insecurity.  These are shown as percentages.")
                           
                           
                           )
                  )
      
      )
    
  )
)