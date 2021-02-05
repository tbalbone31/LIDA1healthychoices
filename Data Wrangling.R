library(tidyverse)

# Create FEAT Outlets dataset from POI ------------------------------------


poi <- read_csv("poi-extract-2020_09.csv", 
                header = TRUE, 
                sep = "|") 

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


FEATPoints <- poi %>%
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
write_csv(poi,"FEAT_Food_Outlets.csv")



# Create Holmewood Postcode Units from Bradford Postcode Units ------------

BD_Postcode_Units <- read_csv("ONSPD_NOV_2020_UK_BD.csv")

HW_Postcode_Units <- BD_Postcode_Units %>%
  select("pcd","oseast1m","osnrth1m","msoa01","lat","long") %>%
  filter(msoa01 == "E02002234") %>%
  add_column(PU_ID = 1:nrow(HW_Postcode_Units),.before = "pcd")

write_csv(HW_Postcode_Units,"Holmewood_Postcode_Units.csv")


# Create Holmewood MSOA polygon from England MSOA polygon -----------------

england_msoa <- st_read("england_msoa_2011.shp")

holmewood_msoa <- england_msoa %>%
  filter(code == "E02002234")

st_write(obj = holmewood_msoa, dsn = "Holmewood_MSOA.shp")
