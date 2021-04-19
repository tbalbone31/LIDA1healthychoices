

# Load libraries and data files -------------------------------------------



library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(stplanr)
library(tidyverse)
library(hrbrthemes)

FEATOutlets <- read_csv("Bradford_FEAT_Food_Outlets.csv")
BD_Postcode_Units <- read_csv("ONSPD_NOV_2020_UK_BD.csv",
                              col_types = cols(
                                osnrth1m = col_double()
                                )
)



source("FEATAccessMetricFunction.R")



# Create Bradford Postcode Units ----------------


#Selects relevant columns and adds an ID variable
BD_Postcode_Units <- BD_Postcode_Units %>%
  select("pcd","oseast1m","osnrth1m","oslaua","msoa01","lat","long") %>%
  drop_na("oseast1m":"osnrth1m") %>%
  filter(oslaua == "E08000032")
  
BD_Postcode_Units <- BD_Postcode_Units %>%
  add_column(PU_ID = 1:nrow(BD_Postcode_Units),
             .before = "pcd")

#Writes DF out to a CSV file
write_csv(BD_Postcode_Units,"Bradford_Postcode_Units.csv")

# Create Holmewood Postcode Units ------------

#Selects relevant columns and adds an ID variable
HW_Postcode_Units <- BD_Postcode_Units %>%
  select("pcd","oseast1m","osnrth1m","oslaua","msoa01","lat","long") %>%
  filter(msoa01 == "E02002234")

HW_Postcode_Units <- HW_Postcode_Units %>%
  add_column(PU_ID = 1:nrow(HW_Postcode_Units),
                                .before = "pcd")

#Writes DF out to a CSV file
write_csv(HW_Postcode_Units,"Holmewood_Postcode_Units.csv")







# Holmewood Access Metrics ------------------------------------------------

#Create Supermarket Origin Destination Desire lines to be use for distance metrics
HW_Sup_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                           FEATSubset = "Supermarkets",
                                           Postcode_Units = HW_Postcode_Units)





#Creates Access Metrics DF (see documentation for list)
HW_Sup_Acc <- Calc_FEAT_Access_Met(ODLines = HW_Sup_OD_Lines,
                                                 FeatType = "Sup",
                                                 PostcodeDataset = HW_Postcode_Units)

#Removes desire lines from GE
rm(HW_Sup_OD_Lines)


# #Writes Access Metrics out as CSV file
# write_csv(HW_SupermarketAccessMetrics,"HW_Supermarket_Access_Metrics.csv")


#Create Takeaways Origin Destination Desire lines to be use for distance metrics
HW_Tway_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                              FEATSubset = "Takeaways",
                                              Postcode_Units = HW_Postcode_Units)


#Creates Takeaway Access Metrics DF (see documentation for list)
HW_Tway_Acc <- Calc_FEAT_Access_Met(ODLines = HW_Tway_OD_Lines,
                                              FeatType = "Tway",
                                              PostcodeDataset = HW_Postcode_Units)
rm(HW_Tway_OD_Lines)

#Writes Takeaways Access Metrics out as CSV file.
#write_csv(HW_TakeawayAccessMetrics,"HW_Takeaway_Access_Metrics.csv")




HW_Con_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                            FEATSubset = "Convenience Stores",
                                            Postcode_Units = HW_Postcode_Units)


#Creates Convenience Store Access Metrics DF (see documentation for list)
HW_Con_Acc <- Calc_FEAT_Access_Met(ODLines = HW_Con_OD_Lines,
                                                 FeatType = "ConSt",
                                                 PostcodeDataset = HW_Postcode_Units)
rm(HW_Con_OD_Lines)



HW_Spec_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                            FEATSubset = "Speciality Outlets",
                                            Postcode_Units = HW_Postcode_Units)


#Creates Takeaway Access Metrics DF (see documentation for list)
HW_Spec_Acc <- Calc_FEAT_Access_Met(ODLines = HW_Spec_OD_Lines,
                                                 FeatType = "Spec",
                                                 PostcodeDataset = HW_Postcode_Units)
rm(HW_Spec_OD_Lines)



#Combine supermarkets and takeaways datasets and write to csv file
HW_Acc <- HW_Sup_Acc %>%
  left_join(select(HW_Tway_Acc,
                   "pcd",c(8:14))
            , by = "pcd") %>%
  left_join(select(HW_Con_Acc,
                   "pcd",c(8:14))
            , by = "pcd") %>%
  left_join(select(HW_Spec_Acc,
                   "pcd",c(8:14))
            , by = "pcd")

HW_Acc <- HW_Acc %>%
  relocate(all_of(c("cls_Tway","cls_ConSt","cls_Spec")),
           .after = "cls_Sup") %>%
  relocate(all_of(c("mean3_Tway","mean3_ConSt","mean3_Spec")),
           .after = "mean3_Sup") %>%
  relocate(all_of(c("mean5_Tway","mean5_ConSt","mean5_Spec")),
           .after = "mean5_Sup") %>%
  relocate(all_of(c("ct500_Tway","ct500_ConSt","ct500_Spec")),
           .after = "ct500_Sup") %>%
  relocate(all_of(c("ct1000_Tway","ct1000_ConSt","ct1000_Spec")),
           .after = "ct1000_Sup") %>%
  relocate(all_of(c("ct1600_Tway","ct1600_ConSt","ct1600_Spec")),
           .after = "ct1600_Sup") %>%
  relocate(all_of(c("ct2000_Tway","ct2000_ConSt","ct2000_Spec")),
           .after = "ct2000_Sup")

write_csv(HW_Acc,"HW_Access_Metrics.csv")








#Bradford City Metrics --------------------------------------------

#Create Supermarket Origin Destination Desire lines to be use for distance metrics
BD_Sup_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                      FEATSubset = "Supermarkets",
                                      Postcode_Units = BD_Postcode_Units)





#Creates Access Metrics DF (see documentation for list)
BD_Sup_Acc <- Calc_FEAT_Access_Met(ODLines = BD_Sup_OD_Lines,
                                   FeatType = "Sup",
                                   PostcodeDataset = BD_Postcode_Units)

#Removes desire lines from GE
rm(BD_Sup_OD_Lines)


# #Writes Access Metrics out as CSV file
# write_csv(HW_SupermarketAccessMetrics,"HW_Supermarket_Access_Metrics.csv")


#Create Takeaways Origin Destination Desire lines to be use for distance metrics
BD_Tway_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                       FEATSubset = "Takeaways",
                                       Postcode_Units = BD_Postcode_Units)


#Creates Takeaway Access Metrics DF (see documentation for list)
BD_Tway_Acc <- Calc_FEAT_Access_Met(ODLines = BD_Tway_OD_Lines,
                                    FeatType = "Tway",
                                    PostcodeDataset = BD_Postcode_Units)
rm(BD_Tway_OD_Lines)

#Writes Takeaways Access Metrics out as CSV file.
#write_csv(HW_TakeawayAccessMetrics,"HW_Takeaway_Access_Metrics.csv")




BD_Con_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                      FEATSubset = "Convenience Stores",
                                      Postcode_Units = BD_Postcode_Units)


#Creates Convenience Store Access Metrics DF (see documentation for list)
BD_Con_Acc <- Calc_FEAT_Access_Met(ODLines = BD_Con_OD_Lines,
                                   FeatType = "ConSt",
                                   PostcodeDataset = BD_Postcode_Units)
rm(BD_Con_OD_Lines)



BD_Spec_OD_Lines <- Create_FEAT_Subset(FEATOutlets = FEATOutlets,
                                       FEATSubset = "Speciality Outlets",
                                       Postcode_Units = BD_Postcode_Units)


#Creates Takeaway Access Metrics DF (see documentation for list)
BD_Spec_Acc <- Calc_FEAT_Access_Met(ODLines = BD_Spec_OD_Lines,
                                    FeatType = "Spec",
                                    PostcodeDataset = BD_Postcode_Units)
rm(BD_Spec_OD_Lines)



#Combine supermarkets and takeaways datasets and write to csv file
BD_Acc <- BD_Sup_Acc %>%
  left_join(select(BD_Tway_Acc,
                   "pcd",c(8:14))
            , by = "pcd") %>%
  left_join(select(BD_Con_Acc,
                   "pcd",c(8:14))
            , by = "pcd") %>%
  left_join(select(BD_Spec_Acc,
                   "pcd",c(8:14))
            , by = "pcd")

BD_Acc <- BD_Acc %>%
  relocate(all_of(c("cls_Tway","cls_ConSt","cls_Spec")),
           .after = "cls_Sup") %>%
  relocate(all_of(c("mean3_Tway","mean3_ConSt","mean3_Spec")),
           .after = "mean3_Sup") %>%
  relocate(all_of(c("mean5_Tway","mean5_ConSt","mean5_Spec")),
           .after = "mean5_Sup") %>%
  relocate(all_of(c("ct500_Tway","ct500_ConSt","ct500_Spec")),
           .after = "ct500_Sup") %>%
  relocate(all_of(c("ct1000_Tway","ct1000_ConSt","ct1000_Spec")),
           .after = "ct1000_Sup") %>%
  relocate(all_of(c("ct1600_Tway","ct1600_ConSt","ct1600_Spec")),
           .after = "ct1600_Sup") %>%
  relocate(all_of(c("ct2000_Tway","ct2000_ConSt","ct2000_Spec")),
           .after = "ct2000_Sup")

write_csv(BD_Acc,"BD_Access_Metrics.csv")