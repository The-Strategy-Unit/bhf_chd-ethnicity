#process metrics ready to calc
library(tidyverse)
library(fingertipsR)
library(readxl)

clustered_gp_and_metrics <- targets::tar_read(clustered_gp_and_metrics) |> dplyr::as_tibble()

metric12 <- targets::tar_read(metric12) |> dplyr::as_tibble()

#Get Fingertips data count
get_my_fingertips_gp_data_count <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year)
}

metric_33 <- get_my_fingertips_gp_data_count(91262,"2022/23")

#metric1 - ok
#metric6 - check which is denom - list or prev - think its list - ok
#metric7 - check which is denom - list or prev - think its list - ok
#metric8 - check which is denom - list or prev - think its list - ok
#metric9 - ok - DQ with practice F85002?

#metric 11 - need to create a numerator and a denominator - currently a %
#done

#metric 13 - keep 13b and remove 13
# should be a % then as a prop of list size

#metric 14 - ok
#metric 15 ok



metric13_all <- fingertips_data(IndicatorID = 273,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod=="2022/23") 

qof_chd_2223 <- tar_read(qof_chd_2223) |> as_tibble()

# check the metric 13b columns