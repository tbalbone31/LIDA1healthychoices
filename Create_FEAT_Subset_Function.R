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