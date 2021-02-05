library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tidyverse)
library(stplanr)

FEATOutlets <- read_csv("FEAT_Food_Outlets.csv")
HW_Postcode_Units <- read_csv("Holmewood_Postcode_Units.csv", 
                              col_types = cols(
                                osnrth1m = col_double()
                          )
)

source("FEATAccessMetricFunction.R")

#Filters Food outlets by West Yorkshire County
#FEATOutlets_WY <- FEATOutlets %>%
  #filter(geographic_county == "West Yorkshire")



# Create Supermarket Access Metrics ----------------------------------------------

#Creates a Supermarket subset for testing the routing process
FEAT_Supermarkets_for_OD <- FEATOutlets %>%
  filter(FEATClass == "Supermarkets") %>%
  select("feature_easting",
         "feature_northing")
  
FEAT_Supermarkets_for_OD <- mutate(FEAT_Supermarkets_for_OD,
                                   Supermarket_ID = 1:nrow(FEAT_Supermarkets_for_OD),
                                  .before = "feature_easting")

#Creates an origin destination dataset containing a Supermarket ID and joins to Supermarket dataset
Supermarket_OD_Data <- HW_Postcode_Units %>% 
  slice(rep(1:n(), 
            each = nrow(FEAT_Supermarkets_for_OD))) %>%
  mutate(Supermarket_ID = rep(1:nrow(FEAT_Supermarkets_for_OD),
                              nrow(HW_Postcode_Units)))
  
Supermarket_OD_Data <- Supermarket_OD_Data %>%
  mutate(Trip_ID = 1:nrow(Supermarket_OD_Data),
         .before = "PU_ID") %>%
  left_join(FEAT_Supermarkets_for_OD,
            by = "Supermarket_ID") %>%
  relocate("oseast1m",
           "osnrth1m",
           "feature_easting",
           "feature_northing",
           .before = "Trip_ID")

rm(FEAT_Supermarkets_for_OD)

#Selects only numeric columns to be processed as desire lines
Supermarket_OD_Data_for_Lines <- select(Supermarket_OD_Data,
                                        "oseast1m",
                                        "osnrth1m",
                                        "feature_easting",
                                        "feature_northing",
                                        "Trip_ID",
                                        "PU_ID",
                                        "Supermarket_ID")

rm(Supermarket_OD_Data)

#Converts OD into desire lines and adds a distance calculation
Supermarket_OD_Lines <- od_coords2line(Supermarket_OD_Data_for_Lines, 
                                       crs = 27700, 
                                       remove_duplicates = FALSE) #converts the OD points to "desire lines"

rm(Supermarket_OD_Data_for_Lines)

Supermarket_OD_Lines$distancekm = as.numeric(st_length(Supermarket_OD_Lines)/1000)

Supermarket_OD_Lines <- filter(Supermarket_OD_Lines,distancekm <= 2)


SupermarketAccessMetrics <- Calc_FEAT_Access_Met(ODLines = Supermarket_OD_Lines,
                                                 FeatType = "Supermarket",
                                                 PostcodeDataset = HW_Postcode_Units)

rm(Supermarket_OD_Lines)

write_csv(SupermarketAccessMetrics,"Supermarket_Access_Metrics.csv")

# Create Takeaway Access Metrics-------------------------------------------------

#Creates a Takeaway subset for testing the routing process
FEAT_Takeaways_for_OD <- FEATOutlets %>%
  filter(FEATClass == "Takeaways") %>%
  select("feature_easting",
         "feature_northing")

FEAT_Takeaways_for_OD <- mutate(FEAT_Takeaways_for_OD,
                                Takeaway_ID = 1:nrow(FEAT_Takeaways_for_OD),
                                   .before = "feature_easting")

#Creates an origin destination dataset containing a Supermarket ID and joins to Supermarket dataset
Takeaways_OD_Data <- HW_Postcode_Units %>% 
  slice(rep(1:n(), 
            each = nrow(FEAT_Takeaways_for_OD))) %>%
  mutate(Takeaway_ID = rep(1:nrow(FEAT_Takeaways_for_OD),
                           nrow(HW_Postcode_Units)))



Takeaways_OD_Data <- Takeaways_OD_Data %>%
  mutate(Trip_ID = 1:nrow(Takeaways_OD_Data),
         .before = "PU_ID") %>%
  left_join(FEAT_Takeaways_for_OD,
            by = "Takeaway_ID") %>%
  relocate("oseast1m",
           "osnrth1m",
           "feature_easting",
           "feature_northing",
           .before = "Trip_ID")

rm(FEAT_Takeaways_for_OD)

#Selects only numeric columns to be processed as desire lines
Takeaways_OD_Data_for_Lines <- select(Takeaways_OD_Data,
                                      "oseast1m",
                                      "osnrth1m",
                                      "feature_easting",
                                      "feature_northing",
                                      "Trip_ID",
                                      "PU_ID",
                                      "Takeaway_ID")

rm(Takeaways_OD_Data)

#Converts OD into desire lines and adds a distance calculation
Takeaways_OD_Lines <- od_coords2line(Takeaways_OD_Data_for_Lines, 
                                     crs = 27700, 
                                     remove_duplicates = FALSE)

rm(Takeaways_OD_Data_for_Lines)

Takeaways_OD_Lines$distancekm = as.numeric(st_length(Takeaways_OD_Lines)/1000)

Takeaways_OD_Lines <- filter(Takeaways_OD_Lines,distancekm <= 2)


TakeawayAccessMetrics <- Calc_FEAT_Access_Met(ODLines = Takeaways_OD_Lines,
                                              FeatType = "Takeaways",
                                              PostcodeDataset = HW_Postcode_Units)

write_csv(TakeawayAccessMetrics,"Takeaway_Access_Metrics.csv")



#combine supermarkets and takeaways datasets and write to file.
AccessMetrics <- SupermarketAccessMetrics %>%
  left_join(select(TakeawayAccessMetrics,
                   "pcd","closest_Takeaways":"Takeaways_count2km")
            , by = "pcd")

write_csv(AccessMetrics,"HW_Access_Metrics.csv")

#ggplot(data = AccessMetrics) + geom_histogram(mapping = aes(x = closest_Takeaways), bins = 10)
