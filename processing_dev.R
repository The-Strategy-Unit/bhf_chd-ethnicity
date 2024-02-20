#process metrics ready to calc
library(tidyverse)
library(fingertipsR)
library(readxl)

clustered_gp_and_metrics <- targets::tar_read(clustered_gp_and_metrics) |> dplyr::as_tibble()

metric32_updated <- targets::tar_read(metric32_updated) |> dplyr::as_tibble()

#Get Fingertips data count
get_my_fingertips_gp_data_count <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year)
}
temp_17 <- get_my_fingertips_gp_data_count(91000,"2020/21")

get_data_via_server <- function(){
  StrategicWorking_DB <- dbConnect(odbc(), 
                                   driver = "SQL Server",
                                   server="MLCSU-BI-SQL",
                                   database="StrategicWorking")
  server_metric<- dbFetch(dbSendQuery(StrategicWorking_DB,"SELECT *
FROM [StrategicWorking].[adhoc].[QOF_ACHIEVEMENT_2021_v2]
where
indicator_code = 'CHD007'
and
measure = 'PCAS'
order by PRACTICE_CODE")) |>
    clean_names()
  #colnames(server_metric)[colnames(server_metric) == 'VALUE'] <- 'Metric32'
}

temp32 <-get_data_via_server()



data_path16 <- tar_read(data_path16)

qof <- read_qof_excel_file(data_path16) |>
  clean_names() 

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