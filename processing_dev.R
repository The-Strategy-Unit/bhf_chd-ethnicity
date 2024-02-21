#process metrics ready to calc
library(tidyverse)
library(fingertipsR)
library(readxl)

clustered_gp_and_metrics <- targets::tar_read(clustered_gp_and_metrics) |> dplyr::as_tibble()
activity_by_type_clusters_stg1<- targets::tar_read(activity_by_type_clusters_stg1) |> dplyr::as_tibble()
activity_by_type_clusters_stg2<- targets::tar_read(activity_by_type_clusters_stg2) |> dplyr::as_tibble()
activity_by_type_clusters_stg3<- targets::tar_read(activity_by_type_clusters_stg3) |> dplyr::as_tibble()
activity_by_type_clusters_stg4<- targets::tar_read(activity_by_type_clusters_stg4) |> dplyr::as_tibble()
activity_by_type_clusters_stg5<- targets::tar_read(activity_by_type_clusters_stg5) |> dplyr::as_tibble()
activity_by_type_clusters_stg6<- targets::tar_read(activity_by_type_clusters_stg6) |> dplyr::as_tibble()
ncdes_data <- tar_read(ncdes_data) |> as_tibble()


activity_by_type_clusters_stg7 <- activity_by_type_clusters_stg6 |>
  select(cluster2,list_size_total,ends_with("rel_iod"))|>
         #metric28_rel_iod,metric29_rel_iod,metric31_rel_iod,metric32_rel_iod,metric33_rel_iod)|>
  pivot_longer(cols=starts_with("metric"),
               names_to="metric")|>
  filter(cluster2==1)|>
  select(-cluster2,-list_size_total)


activity_by_type_clusters_stg7 |>
  mutate(metric=fct_rev(factor(metric,levels=c("metric2_rel_iod","metric5_rel_iod","metric6_rel_iod","metric7_rel_iod","metric8_rel_iod",
                                   "metric9_rel_iod","metric34_rel_iod","metric38_rel_iod","metric11_rel_iod","metric12_rel_iod","metric13_rel_iod","metric33_rel_iod","metric14_rel_iod",
                                   "metric15_rel_iod","metric16_rel_iod","metric31_rel_iod", "metric17_rel_iod","metric32_rel_iod", 
                                   "metric39_rel_iod","metric40_rel_iod","metric18_rel_iod",
                                   "metric19_rel_iod","metric20_rel_iod","metric21_rel_iod", "metric22_rel_iod","metric23_rel_iod",
                                   "metric25_rel_iod","metric27_rel_iod","metric28_rel_iod", "metric29_rel_iod")))) |>
  mutate(metric_name=case_when(metric=="metric2_rel_iod"~"Smoking prev est",
                               metric=="metric5_rel_iod"~"Smoking register",
                               metric=="metric6_rel_iod"~"Obesity register",
                               metric=="metric7_rel_iod"~"Diabetes register",
                               metric=="metric8_rel_iod"~"Depression register",
                               metric=="metric9_rel_iod"~"CVD risk register",
                               metric=="metric34_rel_iod"~"CVD pat treated with lipid lowering therapy",
                               metric=="metric38_rel_iod"~"Pat at risk treated with lipid lowering therapy",
                               metric=="metric11_rel_iod"~"Smoking cessation support offered",
                               .default = metric))|>
ggplot(aes(x=metric, y=value)) +
  geom_segment( aes(x=metric, xend=metric, y=0, yend=value), color="grey") +
  geom_point( color="orange", size=4) +
  coord_flip()+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Relative Index of Disparity (%)")+
  labs(title = paste("Index of Disparity along CHD pathway"))





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