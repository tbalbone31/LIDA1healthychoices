

# Load libraries and data files -------------------------------------------



library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tidyverse)
library(stplanr)

FEATOutlets <- read_csv("FEAT_Food_Outlets.csv")
BD_Postcode_Units <- read_csv("ONSPD_NOV_2020_UK_BD.csv",
                              col_types = cols(
                                osnrth1m = col_double()
                                )
)



source("FEATAccessMetricFunction.R")



# Create Bradford Postcode Units ----------------


#Selects relevant columns and adds an ID variable
BD_Postcode_Units <- BD_Postcode_Units %>%
  select("pcd","oseast1m","osnrth1m","msoa01","lat","long") %>%
  drop_na("oseast1m":"osnrth1m")
  
BD_Postcode_Units <- BD_Postcode_Units %>%
  add_column(PU_ID = 1:nrow(BD_Postcode_Units),
             .before = "pcd")

#Writes DF out to a CSV file
write_csv(BD_Postcode_Units,"Bradford_Postcode_Units.csv")

# Create Holmewood Postcode Units ------------

#Selects relevant columns and adds an ID variable
HW_Postcode_Units <- BD_Postcode_Units %>%
  select("pcd","oseast1m","osnrth1m","msoa01","lat","long") %>%
  filter(msoa01 == "E02002234")

HW_Postcode_Units <- HW_Postcode_Units %>%
  add_column(PU_ID = 1:nrow(HW_Postcode_Units),
                                .before = "pcd")

#Writes DF out to a CSV file
write_csv(HW_Postcode_Units,"Holmewood_Postcode_Units.csv")







# Holmewood Access Metrics ------------------------------------------------

#Create Supermarket Origin Destination Desire lines to be use for distance metrics
HW_Supermarket_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                           FEATSubset = "Supermarkets",
                                           Postcode_Units = HW_Postcode_Units)


#Creates Access Metrics DF (see documentation for list)
HW_SupermarketAccessMetrics <- Calc_FEAT_Access_Met(ODLines = HW_Supermarket_OD_Lines,
                                                 FeatType = "Supermarket",
                                                 PostcodeDataset = HW_Postcode_Units)

#Removes desire lines from GE
rm(HW_Supermarket_OD_Lines)


#Writes Access Metrics out as CSV file
write_csv(HW_SupermarketAccessMetrics,"HW_Supermarket_Access_Metrics.csv")


#Create Takeaways Origin Destination Desire lines to be use for distance metrics
HW_Takeaways_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                              FEATSubset = "Supermarkets",
                                              Postcode_Units = HW_Postcode_Units)


#Creates Takeaway Access Metrics DF (see documentation for list)
HW_TakeawayAccessMetrics <- Calc_FEAT_Access_Met(ODLines = HW_Takeaways_OD_Lines,
                                              FeatType = "Takeaways",
                                              PostcodeDataset = HW_Postcode_Units)
rm(HW_Takeaways_OD_Lines)

#Writes Takeaways Access Metrics out as CSV file.
write_csv(HW_TakeawayAccessMetrics,"HW_Takeaway_Access_Metrics.csv")



#Combine supermarkets and takeaways datasets and write to csv file
HW_AccessMetrics <- HW_SupermarketAccessMetrics %>%
  left_join(select(HW_TakeawayAccessMetrics,
                   "pcd","closest_Takeaways":"Takeaways_count2km")
            , by = "pcd")

write_csv(HW_AccessMetrics,"HW_Access_Metrics.csv")

rm(HW_SupermarketAccessMetrics,HW_TakeawayAccessMetrics,(HW_AccessMetrics))










#Bradford City Metrics --------------------------------------------

#Create Supermarket Origin Destination Desire lines to be use for distance metrics
BD_Supermarket_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                              FEATSubset = "Supermarkets",
                                              Postcode_Units = BD_Postcode_Units)


#Creates Access Metrics DF (see documentation for list)
BD_SupermarketAccessMetrics <- Calc_FEAT_Access_Met(ODLines = BD_Supermarket_OD_Lines,
                                                    FeatType = "Supermarket",
                                                    PostcodeDataset = BD_Postcode_Units)
#Removes desire lines from GE
rm(BD_Supermarket_OD_Lines)

#Writes bradford supermarket access metrics to csv file.
write_csv(BD_SupermarketAccessMetrics,"BD_Supermarket_Access_Metrics.csv")



#Create Takeaway Origin Destination Desire lines to be use for distance metrics
BD_Takeaway_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                           FEATSubset = "Takeaways",
                                           Postcode_Units = BD_Postcode_Units)

#Creates Access Metrics DF (see documentation for list)
BD_TakeawayAccessMetrics <- Calc_FEAT_Access_Met(ODLines = BD_Takeaway_OD_Lines,
                                              FeatType = "Takeaways",
                                              PostcodeDataset = BD_Postcode_Units)

#remove desire lines from GE
rm(BD_Takeaway_OD_Lines)

#Write takeaways access metrics to csv file
write_csv(BD_TakeawayAccessMetrics,"BD_Takeaway_Access_Metrics.csv")





#combine supermarkets and takeaways datasets and write to file.
BD_AccessMetrics <- BD_SupermarketAccessMetrics %>%
  left_join(select(BD_TakeawayAccessMetrics,
                   "pcd","closest_Takeaways":"Takeaways_count2km")
            , by = "pcd")

write_csv(AccessMetrics,"BD_Access_Metrics.csv")





#ggplot(data = AccessMetrics) + geom_histogram(mapping = aes(x = closest_Takeaways), bins = 10)
