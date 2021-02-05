Calc_FEAT_Access_Met <- function(ODLines,FeatType,PostcodeDataset) {
  
  NearestFEAT <- ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste("closest_",FeatType, sep = "") := min(distancekm))
  
  MeanofNearest3FEAT <- ODLines %>%
    group_by(PU_ID) %>%
    top_n(n=-3,distancekm) %>%
    summarise(!!paste("mean_closest_3_",FeatType, sep = "") := mean(distancekm))
  
  MeanofNearest5FEAT  <- ODLines %>%
    group_by(PU_ID) %>%
    top_n(n=-5,distancekm) %>%
    summarise(!!paste("mean_closest_5_",FeatType, sep = "") := mean(distancekm))
  
  FEATcount400m <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_count400m", sep = "") := sum(distancekm <= 0.4))
  
  FEATcount800m <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_count800m", sep = "") := sum(distancekm <= 0.8))
  
  FEATcount1km <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_count1km", sep = "") := sum(distancekm <= 1))
  
  FEATcount1.6km <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_count1.6km", sep = "") := sum(distancekm <= 1.6))
  
  FEATcount2km <-ODLines %>%
    group_by(PU_ID) %>%
    summarise(!!paste(FeatType,"_count2km", sep = "") := sum(distancekm <= 2))
  

  
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
           paste("closest_",FeatType, sep = ""),
           paste("mean_closest_3_",FeatType, sep = ""),
           paste("mean_closest_5_",FeatType, sep = ""),
           paste(FeatType,"_count400m", sep = ""),
           paste(FeatType,"_count800m", sep = ""),
           paste(FeatType,"_count1km", sep = ""),
           paste(FeatType,"_count1.6km", sep = ""),
           paste(FeatType,"_count2km", sep = "")
    )
  
  
  return(FEATAccessMetrics)
  
}