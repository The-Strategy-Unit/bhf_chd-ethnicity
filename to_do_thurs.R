library(tidyverse)
library(targets)


# Look at this data
cvdp003 <- tar_read(cvdp003) |> as_tibble()
# cvdp006, cvdp007,cvdp008,cvdp009

ncdes_data <- tar_read(ncdes_data) |> as_tibble()

#check each metric for format / value to use etc


############################################################################



final_data_full_cats_percent_over45_5_clusters <- tar_read(final_data_full_cats_percent_over45_5_clusters) |> as_tibble()
gp_lsoa_with_eth_sum <- tar_read(gp_lsoa_with_eth_sum) |> as_tibble()
clustered_gp_and_metrics <- tar_read(clustered_gp_and_metrics)|> as_tibble()
activity_by_type_clusters_stg1 <- tar_read(activity_by_type_clusters_stg1)|> as_tibble()


93088
92588

  Metric <- fingertips_data(IndicatorID = 273,AreaTypeID = "All") 
  
  #Get Fingertips data
  get_my_fingertips_gp_data <- function(ind,year){
    Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
      filter(AreaType=="GPs", Timeperiod==year) 
  }
  
 metric_ft <- get_my_fingertips_gp_data(241,"2022/23")
  
  Metric <- Metric |>
    filter(AreaType=="GPs") 
  
  unique(Metric$AreaType)
