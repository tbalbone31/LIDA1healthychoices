library(tidyverse)


# Create Bradford FEAT Points ---------------------------------------------


Bradford_poi <- read_csv("POI_Bradford_3km_Buffered.csv")

case_pointxclass <- function(pointx_class,brand) {
  case_when(
    pointx_class == 1020013 ~ "Cafes",
    pointx_class == 1020025 ~ "Cafes",
    pointx_class == 9470798 ~ "Cafes",
    pointx_class == 9470819 ~ "Supermarkets",
    pointx_class == 9470699 & (brand == "Sainsbury's Local" |
                                 brand == "Tesco Express" |
                                 brand == "Morrisons Daily"
    ) ~ "Supermarkets",
    pointx_class == 9470699 & (brand != "Sainsbury's Local" |
                                 brand != "Tesco Express" |
                                 brand != "Morrisons Daily"
    ) ~ "Convenience Stores",
    pointx_class == 1020043 ~ "Restaurants",
    pointx_class == 1020034 ~ "Restaurants",
    pointx_class == 9470662 ~ "Speciality Outlets",
    pointx_class == 9470665 ~ "Speciality Outlets",
    pointx_class == 9470666 ~ "Speciality Outlets",
    pointx_class == 9470667 ~ "Speciality Outlets",
    pointx_class == 9470668 ~ "Speciality Outlets",
    pointx_class == 9470669 ~ "Speciality Outlets",
    pointx_class == 9470670 ~ "Speciality Outlets",
    pointx_class == 9470672 ~ "Speciality Outlets",
    pointx_class == 9470705 ~ "Speciality Outlets",
    pointx_class == 7400524 ~ "Speciality Outlets",
    pointx_class == 9470663 ~ "Speciality Outlets",
    pointx_class == 1020018 ~ "Takeaways",
    pointx_class == 1020019 ~ "Takeaways",
    pointx_class == 1020020 ~ "Takeaways",
    pointx_class == 9470661 ~ "Takeaways"
  )
}

Bradford_FEATPoints <- Bradford_poi %>%
  # Filter the dataframe by relevant classification codes
  filter(pointx_class ==1020013| pointx_class ==1020025| 
           pointx_class ==9470798| pointx_class ==9470699| 
           pointx_class ==1020043| pointx_class ==1020034|
           pointx_class ==9470662| pointx_class ==9470665| 
           pointx_class ==9470666| pointx_class ==9470667| 
           pointx_class ==9470668| pointx_class ==9470669|
           pointx_class ==9470670| pointx_class ==9470672| 
           pointx_class ==9470705| pointx_class ==7400524| 
           pointx_class ==9470663| pointx_class ==9470699|
           pointx_class ==9470819| pointx_class ==1020018| 
           pointx_class ==1020019| pointx_class ==1020020| 
           pointx_class ==9470661) %>%
  #Select relevant columns
  select(name,pointx_class,feature_easting,feature_northing,street_name,
         locality,geographic_county,postcode,admin_boundary,brand) %>%
  #Create new column and populate with FEAT classification
  mutate(FEATClass = case_pointxclass(pointx_class,brand))


#Writes the POI out to a csv
write_csv(Bradford_FEATPoints,"Bradford_FEAT_Food_Outlets.csv")
