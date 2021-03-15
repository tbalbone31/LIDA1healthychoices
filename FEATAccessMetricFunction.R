


# Food Outlet Subset and Desire Lines --------


Create_FEAT_Subset <-function(FEATOutlets, FEATSubset, Postcode_Units) {
  
  
  #Creates a Subset subset for testing the routing process
  FEAT_Subset_for_OD <- FEATOutlets %>%
    filter(FEATClass == FEATSubset) %>%
    select("feature_easting",
           "feature_northing")
  
  FEAT_Subset_for_OD <- mutate(FEAT_Subset_for_OD,
                               !!paste(FEATSubset,"_ID",sep = "") := 1:nrow(FEAT_Subset_for_OD),
                               .before = "feature_easting")
  
  #Creates an origin destination dataset containing a Subset ID and joins to Subset dataset
  Subset_OD_Data <- Postcode_Units %>% 
    slice(rep(1:n(), 
              each = nrow(FEAT_Subset_for_OD))) %>%
    mutate(!!paste(FEATSubset,"_ID",sep = "") := rep(1:nrow(FEAT_Subset_for_OD),
                                                     nrow(Postcode_Units)))
  
  Subset_OD_Data <- Subset_OD_Data %>%
    mutate(Trip_ID = 1:nrow(Subset_OD_Data),
           .before = "PU_ID") %>%
    left_join(FEAT_Subset_for_OD,
              by = paste(FEATSubset,"_ID", sep = "")) %>%
    relocate("oseast1m",
             "osnrth1m",
             "feature_easting",
             "feature_northing",
             .before = "Trip_ID")
  
  rm(FEAT_Subset_for_OD)
  
  #Selects only numeric columns to be processed as desire lines
  Subset_OD_Data_for_Lines <- select(Subset_OD_Data,
                                     "oseast1m",
                                     "osnrth1m",
                                     "feature_easting",
                                     "feature_northing",
                                     "Trip_ID",
                                     "PU_ID",
                                     paste(FEATSubset,"_ID", sep = ""))
  
  rm(Subset_OD_Data)
  
  #Converts OD into desire lines and adds a distance calculation
  Subset_OD_Lines <- od_coords2line(Subset_OD_Data_for_Lines, 
                                    crs = 27700, 
                                    remove_duplicates = FALSE) #converts the OD points to "desire lines"
  
  rm(Subset_OD_Data_for_Lines)
  
  Subset_OD_Lines$distancekm = as.numeric(st_length(Subset_OD_Lines)/1000)
  
  Subset_OD_Lines <- filter(Subset_OD_Lines,distancekm <= 2)
  
  return(Subset_OD_Lines)
  
  
}



# Food Outlet Access Metrics ----------------------------------------------



Calc_FEAT_Access_Met <- function(ODLines,FeatType,PostcodeDataset) {
  
  NearestFEAT <- ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste("cls_",FeatType, sep = "") := min(distancekm))
  
  MeanofNearest3FEAT <- ODLines %>%
    group_by(PU_ID) %>%
    top_n(n=-3,distancekm) %>%
    summarise(!!paste("mean3_",FeatType, sep = "") := mean(distancekm))
  
  MeanofNearest5FEAT  <- ODLines %>%
    group_by(PU_ID) %>%
    top_n(n=-5,distancekm) %>%
    summarise(!!paste("mean5_",FeatType, sep = "") := mean(distancekm))
  
  FEATcount400m <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_ct400", sep = "") := sum(distancekm <= 0.4))
  
  FEATcount800m <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_ct800", sep = "") := sum(distancekm <= 0.8))
  
  FEATcount1km <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_ct1000", sep = "") := sum(distancekm <= 1))
  
  FEATcount1.6km <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_ct1600", sep = "") := sum(distancekm <= 1.6))
  
  FEATcount2km <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_ct2000", sep = "") := sum(distancekm <= 2))
  

  
  FEATAccessMetrics <- PostcodeDataset %>%
    left_join(NearestFEAT,
              by = "PU_ID") %>%
    left_join(MeanofNearest3FEAT,
              by = "PU_ID") %>%
    left_join(MeanofNearest5FEAT,
              by = "PU_ID") %>%
    left_join(FEATcount400m,
              by = "PU_ID") %>%
    left_join(FEATcount800m,
              by = "PU_ID") %>%
    left_join(FEATcount1km,
              by = "PU_ID") %>%
    left_join(FEATcount1.6km,
              by = "PU_ID") %>%
    left_join(FEATcount2km,
              by = "PU_ID") %>%
    select("PU_ID",
           "pcd",
           "oseast1m",
           "osnrth1m",
           "msoa01",
           "lat",
           "long",
           paste("cls_",FeatType, sep = ""),
           paste("mean3_",FeatType, sep = ""),
           paste("mean5_",FeatType, sep = ""),
           paste(FeatType,"_ct400", sep = ""),
           paste(FeatType,"_ct800", sep = ""),
           paste(FeatType,"_ct1000", sep = ""),
           paste(FeatType,"_ct1600", sep = ""),
           paste(FeatType,"_ct2000", sep = "")
    )
  
  
  return(FEATAccessMetrics)
  
}