#Read an excel data file
read_excel_file <- function(file){
  excel_data <- read_excel(file) 
}

#Read a csv data file
read_csv_file <- function(file){
  csv_data <- read_csv(file) |>
    clean_names()
}


#Get Fingertips data
get_my_fingertips_gp_data <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year) |>
    select(AreaCode,Value)
}

#Get QOF via Excel file
read_qof_excel_file <- function(file){
  excel_qof_data <- read_xlsx(file,skip=11,.name_repair = "unique_quiet") 
}

#Connect to Server and get data
get_data_via_server <- function(){
StrategicWorking_DB <- dbConnect(odbc(), 
                                 driver = "SQL Server",
                                 server="MLCSU-BI-SQL",
                                 database="StrategicWorking")
server_metric<- dbFetch(dbSendQuery(StrategicWorking_DB,"SELECT [PRACTICE_CODE]
,[VALUE]
FROM [StrategicWorking].[adhoc].[QOF_ACHIEVEMENT_2021_v2]
where
indicator_code = 'CHD007'
and
measure = 'PCAS'
order by PRACTICE_CODE")) |>
  clean_names()
#colnames(server_metric)[colnames(server_metric) == 'VALUE'] <- 'Metric32'
}

# Process Census 2021 ethnicity data to remove wales and organise ethnicities
process_census21data <- function(data){
  data |>
    filter(str_detect(lsoa_code, '^E'))|> 
    group_by(lsoa_code) |> 
    summarise(sum_all = sum(total_all_usual_residents),
              all_asian = sum(asian_asian_british_or_asian_welsh),
              bangladeshi = sum(asian_asian_british_or_asian_welsh_bangladeshi),
              chinese = sum(asian_asian_british_or_asian_welsh_chinese),
              indian =  sum(asian_asian_british_or_asian_welsh_indian),
              pakistani =  sum(asian_asian_british_or_asian_welsh_pakistani),
              other_asian =  sum(asian_asian_british_or_asian_welsh_other_asian),
              all_black = sum(black_black_british_black_welsh_caribbean_or_african),
              blk_african = sum(black_black_british_black_welsh_caribbean_or_african_african),
              blk_caribbean = sum(black_black_british_black_welsh_caribbean_or_african_caribbean),
              other_blk = sum(black_black_british_black_welsh_caribbean_or_african_other_black),
              all_mixed = sum(mixed_or_multiple_ethnic_groups),
              mixed_other = sum(mixed_or_multiple_ethnic_groups_other_mixed_or_multiple_ethnic_groups),
              mixed_white_asian = sum(mixed_or_multiple_ethnic_groups_white_and_asian),
              mixed_white_blk_african = sum(mixed_or_multiple_ethnic_groups_white_and_black_african),
              mixed_white_blk_caribbean = sum(mixed_or_multiple_ethnic_groups_white_and_black_caribbean),
              all_white = sum(white),
              white_british = sum(white_english_welsh_scottish_northern_irish_or_british),
              white_irish = sum(white_irish),
              white_gypsy_or_irish_traveller = sum(white_gypsy_or_irish_traveller),
              white_roma = sum(white_roma),
              white_other_white = sum(white_other_white),
              all_other = sum(other_ethnic_group),
              other_arab = sum(other_ethnic_group_arab),
              other_other= sum(other_ethnic_group_any_other_ethnic_group)
    ) |>
    mutate(white_other = white_gypsy_or_irish_traveller+white_roma+white_other_white)
}

#Process
process_gpdata <- function(data){
data |>
  filter(str_detect(lsoa_code, '^E'))
}


# Join census ethnicity data to gp lists by lsoa
join_gp_and_eth <- function(eth,gp){
  gp |> 
    left_join(eth) |>
    mutate(est_all = (sum_all/sum_all)*number_of_patients,
           est_all_asian = (all_asian/sum_all)*number_of_patients,
           est_bangladeshi = (bangladeshi/sum_all)*number_of_patients,
           est_chinese = (chinese/sum_all)*number_of_patients,          
           est_indian = (indian/sum_all)*number_of_patients,   
           est_pakistani = (pakistani/sum_all)*number_of_patients,   
           est_other_asian = (other_asian/sum_all)*number_of_patients,              
           est_all_black = (all_black/sum_all)*number_of_patients, 
           est_blk_african = (blk_african/sum_all)*number_of_patients, 
           est_blk_caribbean = (blk_caribbean/sum_all)*number_of_patients, 
           est_other_blk = (other_blk/sum_all)*number_of_patients,              
           est_all_mixed = (all_mixed/sum_all)*number_of_patients, 
           est_mixed_other = (mixed_other/sum_all)*number_of_patients, 
           est_mixed_white_asian = (mixed_white_asian/sum_all)*number_of_patients, 
           est_mixed_white_blk_african = (mixed_white_blk_african/sum_all)*number_of_patients,              
           est_mixed_white_blk_caribbean = (mixed_white_blk_caribbean/sum_all)*number_of_patients, 
           est_all_white = (all_white/sum_all)*number_of_patients, 
           est_white_british = (white_british/sum_all)*number_of_patients, 
           est_white_irish = (white_irish/sum_all)*number_of_patients,            
           est_white_gypsy_or_irish_traveller = (white_gypsy_or_irish_traveller/sum_all)*number_of_patients,   
           est_white_roma = (white_roma/sum_all)*number_of_patients,
           est_white_other_white = (white_other_white/sum_all)*number_of_patients,
           est_all_other = (all_other/sum_all)*number_of_patients,
           est_other_arab = (other_arab/sum_all)*number_of_patients,
           est_other_other = (other_other/sum_all)*number_of_patients,
           est_white_other = (white_other/sum_all)*number_of_patients
           ) 
}

sum_eth_by_lsoa11 <- function(lookup,eth){
  #join the lookup to the ethnicity data

  lookup |> 
  left_join(eth, join_by(lsoa21cd==lsoa_code)) |>
  #sum the ethnicity in the dataframe with 2011 lsoa group by
  group_by(f_lsoa11cd) |> 
  summarise(sum_all = sum(sum_all),
            all_asian = sum(all_asian),
            bangladeshi = sum(bangladeshi),
            chinese = sum(chinese),
            indian = sum(indian),
            pakistani = sum(pakistani),
            other_asian = sum(other_asian),
            all_black = sum(all_black),
            blk_african = sum(blk_african),
            blk_caribbean = sum(blk_caribbean),
            other_blk = sum(other_blk),
            all_mixed = sum(all_mixed),
            mixed_other = sum(mixed_other),
            mixed_white_asian = sum(mixed_white_asian),
            mixed_white_blk_african = sum(mixed_white_blk_african),
            mixed_white_blk_caribbean = sum(mixed_white_blk_caribbean),
            all_white = sum(all_white),
            white_british = sum(white_british),
            white_irish = sum(white_irish),
            white_gypsy_or_irish_traveller = sum(white_gypsy_or_irish_traveller),
            white_roma = sum(white_roma),
            white_other_white = sum(white_other_white),
            all_other = sum(all_other),
            other_arab = sum(other_arab),
            other_other = sum(other_other),
            white_other = sum(white_other)
  ) |>  # have tested this works!
    rename(lsoa_code=f_lsoa11cd)

}

#######

sum_ethnicities <- function(data){

# add up and check the total matches the number of patients

  data |>
  group_by(practice_code) |> 
  summarise(gp_sum_est_all_asian = sum(est_all_asian),
            gp_sum_est_bangladeshi = sum(est_bangladeshi),
            gp_sum_est_chinese = sum(est_chinese),
            gp_sum_est_indian = sum(est_indian),
            gp_sum_est_pakistani = sum(est_pakistani),
            gp_sum_est_other_asian = sum(est_other_asian),
            gp_sum_est_all_black = sum(est_all_black),
            gp_sum_est_blk_african = sum(est_blk_african),
            gp_sum_est_blk_caribbean = sum(est_blk_caribbean),
            gp_sum_est_other_blk = sum(est_other_blk),
            gp_sum_est_all_mixed = sum(est_all_mixed),
            gp_sum_est_mixed_other = sum(est_mixed_other),
            gp_sum_est_mixed_white_asian = sum(est_mixed_white_asian),
            gp_sum_est_mixed_white_blk_african = sum(est_mixed_white_blk_african),
            gp_sum_est_mixed_white_blk_caribbean = sum(est_mixed_white_blk_caribbean),
            gp_sum_est_all_white = sum(est_all_white),
            gp_sum_est_white_british = sum(est_white_british),
            gp_sum_est_white_irish = sum(est_white_irish),
            gp_sum_est_white_gypsy_or_irish_traveller = sum(est_white_gypsy_or_irish_traveller),
            gp_sum_est_white_roma = sum(est_white_roma),
            gp_sum_est_white_other_white = sum(est_white_other_white),
            gp_sum_est_all_other = sum(est_all_other),
            gp_sum_est_other_arab = sum(est_other_arab),
            gp_sum_est_other_other = sum(est_other_other),
            gp_sum_est_white_other = sum(est_white_other)
  ) |> 
  arrange()  |>
  mutate(gp_sum_total=gp_sum_est_bangladeshi+gp_sum_est_chinese+gp_sum_est_indian+
           gp_sum_est_pakistani+gp_sum_est_other_asian+gp_sum_est_blk_african+gp_sum_est_blk_caribbean+
           gp_sum_est_other_blk+gp_sum_est_all_mixed+gp_sum_est_white_british+gp_sum_est_white_irish+
           gp_sum_est_white_other+gp_sum_est_other_arab+gp_sum_est_other_other
  ) |>
  mutate(gp_sum_total2=gp_sum_est_all_asian+gp_sum_est_all_black+gp_sum_est_all_mixed+
           gp_sum_est_all_white+gp_sum_est_all_other
  )
}
