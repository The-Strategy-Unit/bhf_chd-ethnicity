#Read an excel data file
read_excel_file <- function(file){
  excel_data <- read_excel(file) 
}

#Read a csv data file
read_csv_file <- function(file){
  csv_data <- read_csv(file) |>
    clean_names()
}


#Get Fingertips data Value
get_my_fingertips_gp_data <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year) |>
    select(AreaCode,Value)
}

#Get Fingertips data count
get_my_fingertips_gp_data_count <- function(ind,year){
  Metric <- fingertips_data(IndicatorID = ind,AreaTypeID = "All") |>
    filter(AreaType=="GPs", Timeperiod==year) |>
    select(AreaCode,Count)
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

#join the lookup to the ethnicity data
sum_eth_by_lsoa11 <- function(lookup,eth){
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


# add up and check the total matches the number of patients
sum_ethnicities <- function(data){
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

#get selected fields from gp_history
get_gp_history_short <- function(gp_history){
  gp_history_short <- gp_history |> 
    select(org_code, name, postcode,open_date,close_date,join_date,left_date)
  return(gp_history_short)
}

#summarise the gp list size file to just a simple list of all practices
get_gp_list_summary <- function(gp_reg_pat_prac_lsoa,gp_history_short){
  gp_list_summary <- gp_reg_pat_prac_lsoa |> 
    filter(sex=="ALL") |>
    group_by(practice_code,practice_name) |>
    summarise(number_of_patients=sum(number_of_patients)) |>
    rename(org_code=practice_code)|> 
    left_join(gp_history_short)
  return(gp_list_summary)
}


#join the chd prev to gp history
get_joined_gp_history_and_chd_prev <- function (metric1,gp_history_short){
    orig <- metric1 |> select(AreaCode,AreaName, Value, IsCurrent) |>
    rename(org_code=AreaCode) |>
    left_join(gp_history_short)
  return(orig)
}

# joins gp list to the prevalence and geocodes all practices
get_geocoded_data <- function(gp_list_summary,orig,joined_gp_history_and_chd_prev){
  
  joined <- gp_list_summary |>
    left_join(joined_gp_history_and_chd_prev) |>
    #where there is a matching value from orig in the gp list summary then flag these 
    mutate(has_orig_prev=case_when(Value>=0 ~ 1, .default =  0))
  
  orig_with_geocode <- joined |>
    select(org_code,practice_name,postcode,has_orig_prev) |>
    mutate(id = row_number()) |>
    geocode(postalcode = postcode) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326,na.fail=FALSE)

  return(orig_with_geocode)
}


# splits practices into those with and those without prev, finds neighbours
# of those missing prev and populates with mean of neighbours
get_missing_chd_prevalence <- function(metric1,gp_history_short,joined_gp_history_and_chd_prev,gp_list_summary,gp_geocoded){

  joined <- gp_list_summary |>
    left_join(joined_gp_history_and_chd_prev) |>
    #where there is no matching value from orig in the gp list summary then flag these 
    mutate(has_orig_prev=case_when(Value>=0 ~ 1, .default =  0))
  
  orig_with_geocode_and_id <- gp_geocoded |>
    select(org_code,practice_name,postcode,has_orig_prev,geometry) |>
    mutate(id=row_number())

  no_prev <- orig_with_geocode_and_id |>
    filter(has_orig_prev == 0)
  
  with_prev <- orig_with_geocode_and_id |>
    filter(has_orig_prev == 1)
  
  no_prev |>
    select(no_prev_postcode = postcode) |>
    st_join(with_prev, st_nearest_feature)
  
  no_prev_with_neighbours <-  st_sf(no_prev) |>
    select(no_prev_postcode = postcode,no_prev_org_code=org_code) |>
    # finds all the gp's in 1.5 km
    st_join(st_sf(with_prev), st_is_within_distance, 1500)
  
  no_prev_with_neighbours_and_neighbours_prev <- no_prev_with_neighbours |>
    left_join(joined_gp_history_and_chd_prev, join_by(org_code)) |>
    group_by(no_prev_org_code) |>
    summarise(prev_est=mean(Value)) |>
    st_drop_geometry(select(no_prev_org_code,prev_est))

  metric1_updated <- joined |>
    left_join(no_prev_with_neighbours_and_neighbours_prev,join_by(org_code==no_prev_org_code)) |>
    mutate(chd_prev_to_use = case_when(Value>=0 ~ Value,
                                       .default = prev_est)
    ) |>
    select(org_code,chd_prev_to_use)
  return(metric1_updated)
}


get_over45_perc <- function(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)
{gp_over45_perc <- gp_reg_pat_prac_sing_age_male |>
    rbind(gp_reg_pat_prac_sing_age_female)|>
    filter(age != "ALL") |>
    group_by(org_code,age) |>
    summarise(number_of_patients=sum(number_of_patients)) |>
    dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                           .default = as.numeric(age))
    )) |>
    mutate(over_45=case_when(age>= 45 ~1,
                             .default = 0)) |>
    group_by(org_code,over_45)|>
    summarise(number_of_patients=sum(number_of_patients))|>
    group_by(org_code) |>
    mutate(total_patients=sum(number_of_patients))|>
    ungroup() |>
    mutate(perc=(number_of_patients/total_patients)*100)|>
    filter(over_45==1) |>
    select(org_code,perc)|>
    rename(practice_code=org_code,perc_over45=perc)


return(gp_over45_perc)

}

get_gp_16andover_pop <- function(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)
{gp_16andover_pop <- gp_reg_pat_prac_sing_age_male |>
  rbind(gp_reg_pat_prac_sing_age_female)|>
  filter(age != "ALL") |>
  group_by(org_code,age) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                         .default = as.numeric(age))
  )) |>
  filter(as.numeric(age) >= 16) |>
  group_by(org_code) |>
  summarise(number_of_patients16andover=sum(number_of_patients)) |>
  ungroup() |>
  select(org_code,number_of_patients16andover)|>
  rename(practice_code=org_code)

return(gp_16andover_pop)

}


join_over45_to_gp_lsoa_with_eth_sum <- function(gp_lsoa_with_eth_sum,gp_over45_perc){
  gp_lsoa_with_eth_sum_over45perc <- gp_lsoa_with_eth_sum |>
    left_join(gp_over45_perc)
  return(gp_lsoa_with_eth_sum_over45perc)
}



get_full_cats_percents_over45 <- function(gp_lsoa_with_eth_sum_over45perc)
{
  full_cats_percents_over45 <- gp_lsoa_with_eth_sum_over45perc |>
    select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
           gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
           gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
           gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total,perc_over45) |>
    mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/gp_sum_total)*100,
           gp_perc_est_chinese=(gp_sum_est_chinese/gp_sum_total)*100,
           gp_perc_est_indian=(gp_sum_est_indian/gp_sum_total)*100,
           gp_perc_est_pakistani=(gp_sum_est_pakistani/gp_sum_total)*100,
           gp_perc_est_other_asian=(gp_sum_est_other_asian/gp_sum_total)*100,
           gp_perc_est_blk_african=(gp_sum_est_blk_african/gp_sum_total)*100,
           gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/gp_sum_total)*100,
           gp_perc_est_other_blk=(gp_sum_est_other_blk/gp_sum_total)*100,
           gp_perc_est_all_mixed=(gp_sum_est_all_mixed/gp_sum_total)*100,
           gp_perc_est_white_british=(gp_sum_est_white_british/gp_sum_total)*100,
           gp_perc_est_white_irish=(gp_sum_est_white_irish/gp_sum_total)*100,
           gp_perc_est_white_other=(gp_sum_est_white_other/gp_sum_total)*100,
           gp_perc_est_other_arab=(gp_sum_est_other_arab/gp_sum_total)*100,
           gp_perc_est_other_other=(gp_sum_est_other_other/gp_sum_total)*100
    ) |>
    select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
           -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
           -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
           -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total)
  return(full_cats_percents_over45)
}


get_scale_full_cats_percents_over45 <- function(full_cats_percents_over45){
  scale_full_cats_percents_over45 <- full_cats_percents_over45 |>
    remove_rownames() |>
    column_to_rownames(var = 'practice_code') |>
    na.omit() |>
    scale()
  
  return(scale_full_cats_percents_over45)
  
}

get_clusters <- function(scale_full_cats_percents_over45,full_cats_percents_over45){
  #FULL CATS WITH 15 CLUSTERS PERCENT BASED
  #make this example reproducible
  set.seed(10)
  #perform k-medoids clustering with k = 15 clusters
  pams_full_cats_percents_over45 <- pam(scale_full_cats_percents_over45, 5, metric = 'euclidean', stand = FALSE)
  
  final_data_full_cats_percent_over45_5_clusters <- 
    cbind(full_cats_percents_over45, cluster = pams_full_cats_percents_over45$cluster)|>
    rename(gp_practice_code=practice_code)
  
  return(final_data_full_cats_percent_over45_5_clusters)
}


get_full_cats_percents <- function(gp_lsoa_with_eth_sum)
{
 
  full_cats_percents <- gp_lsoa_with_eth_sum |>
    select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
           gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
           gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
           gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total) |>
    mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/gp_sum_total)*100,
           gp_perc_est_chinese=(gp_sum_est_chinese/gp_sum_total)*100,
           gp_perc_est_indian=(gp_sum_est_indian/gp_sum_total)*100,
           gp_perc_est_pakistani=(gp_sum_est_pakistani/gp_sum_total)*100,
           gp_perc_est_other_asian=(gp_sum_est_other_asian/gp_sum_total)*100,
           gp_perc_est_blk_african=(gp_sum_est_blk_african/gp_sum_total)*100,
           gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/gp_sum_total)*100,
           gp_perc_est_other_blk=(gp_sum_est_other_blk/gp_sum_total)*100,
           gp_perc_est_all_mixed=(gp_sum_est_all_mixed/gp_sum_total)*100,
           gp_perc_est_white_british=(gp_sum_est_white_british/gp_sum_total)*100,
           gp_perc_est_white_irish=(gp_sum_est_white_irish/gp_sum_total)*100,
           gp_perc_est_white_other=(gp_sum_est_white_other/gp_sum_total)*100,
           gp_perc_est_other_arab=(gp_sum_est_other_arab/gp_sum_total)*100,
           gp_perc_est_other_other=(gp_sum_est_other_other/gp_sum_total)*100
    ) |>
    select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
           -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
           -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
           -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total)
  
  
  return(full_cats_percents)
}


get_scale_full_cats_percents<- function(full_cats_percents){
  scale_full_cats_percents <- full_cats_percents |>
    remove_rownames() |>
    column_to_rownames(var = 'practice_code') |>
    na.omit() |>
    scale()
  
  return(scale_full_cats_percents)
}

get_clusters <- function(pams_full_cats_percents,full_cats_percents){
  
  
  final_data <- 
    cbind(full_cats_percents, cluster = pams_full_cats_percents$cluster)|>
    rename(gp_practice_code=practice_code)|>
    mutate(cluster_renum=case_when(cluster==2~1,
                                    cluster==1~2,
                                    cluster==3~3,
                                    cluster==5~4,
                                    cluster==4~5))|>
    select(-cluster)|>
    rename(cluster=cluster_renum)
  
  return(final_data)
}

get_pams <-  function(scale_full_cats_percents){
  set.seed(99)
#perform k-medoids clustering with k = 5 clusters
pams_full_cats_percents <- pam(scale_full_cats_percents, 5, metric = 'euclidean', stand = FALSE)
return(pams_full_cats_percents)
}

get_elbow_plot <- function(scaled_data){
  
elbow_plot <- fviz_nbclust(scaled_data, pam, method = "wss")
return(elbow_plot)
}

get_cluster_plot <- function(pams_data){
  #plot results of final k-medoids model
  cluster_plot <- fviz_cluster(pams_data, data = full_cats)
  return(cluster_plot)
}


#proccess metric 23 to work out whether los of cabg and pci is below trimpoint
metric23_below_trimpoint <- function(metric23,metric23trimpoints){
  #Join the PCI and CABG spells to the trim point data by HRG, flag those with a spell duration less than relevant trim point
  #(take average spell duration for the HRG where spell duration is missing and remove data where still null spell duration
  #eg no average available)
  
  metric23_updated <- metric23 |>
  left_join(select(metric23trimpoints,hrg_code,ordinary_elective_long_stay_trim_point_days), by = c("pb_r_spell_id" = "hrg_code")) |>
  filter(!is.na(adjusted_spel_dur))|> 
  filter(!is.na(ordinary_elective_long_stay_trim_point_days))|>
  mutate (below_trimpoint = adjusted_spel_dur-ordinary_elective_long_stay_trim_point_days) |>
  filter(below_trimpoint<0) |>
  group_by (gp_practice_code ) |> 
  summarise(numbelow_trimpoint = sum(number_pcicabg)) |> 
  arrange() |>
    rename(metric23 = numbelow_trimpoint)

return(metric23_updated)
}




add_all_metrics <- function(final_data_full_cats_percent_over45_5_clusters,final_data_full_cats_percent_5_clusters,
                            gp_16andover_pop,
                            metric1_updated,metric6,metric7,metric8,metric9,metric11,metric13,metric13b,
                            metric14,metric15,metric16,metric16b,metric17,metric18,metric19,metric20,
                            metric21,metric22,metric23_updated,metric25b,metric27,metric28,
                            metric29,metric31,metric32,metric34,metric38,
                            metric39,metric40){
  clustered_gp_and_metrics <-
    final_data_full_cats_percent_over45_5_clusters |> rename(cluster1=cluster)|>
    left_join(final_data_full_cats_percent_5_clusters|>select(gp_practice_code,cluster2=cluster)) |>
    left_join(gp_16andover_pop|>select(gp_practice_code=practice_code,list_size=number_of_patients16andover)) |>
    left_join(metric1_updated)|>
    
    #2 - to do
    #3 - deleted
    #4 - deleted
    #5 - to do
    
    left_join(metric6)|>
    mutate(metric6=(metric6/100)*list_size)|>
    left_join(metric7)|>
    mutate(metric7=(metric7/100)*list_size)|>
    left_join(metric8)|>
    mutate(metric8=(metric8/100)*list_size)|>
    left_join(metric9)|>    
    mutate(metric9=(metric9/100)*list_size)|>
    #10 - might not be available
    
    left_join(metric11)|>
    #12 - to do
    left_join(metric13)|>
    mutate(metric13=(metric13/100)*list_size)|>
    left_join(metric13b)|>
    mutate(metric13b=(metric13b/100)*list_size)|>
    left_join(metric14)|>
    left_join(metric15)|>
    left_join(metric16)|>
    left_join(metric16b)|>
    left_join(metric17)|>
    left_join(metric18)|>
    left_join(metric19)|>
    left_join(metric20)|>
    left_join(metric21)|>
    left_join(metric22)|>
    left_join(metric23_updated)|>
    #24b - NACR
    #24c - NACR
  left_join(metric25b)|>
    #26 - to do
  left_join(metric27)|>
  left_join(metric28)|>
  left_join(metric29)|>
    #30 - might not be available
  left_join(metric31)|>
  left_join(metric32)|>
    #33 - to do
  left_join(metric34)|>
 #35-37 removed not needed
  left_join(metric38)|>
  left_join(metric39)|>
  left_join(metric40)
    
  return(clustered_gp_and_metrics)
}


process_metrics <-function(clustered_gp_and_metrics){

activity_by_type_clusters_stg1<-clustered_gp_and_metrics |>
  filter(metric1 != "NA") |>
  mutate(list_size_total = replace_na(list_size, 0)) |>
  mutate(metric1_total = replace_na(metric1, 0)) |>
  #mutate(metric2_total = replace_na(metric2, 0)) |>
  #  mutate(metric3_total = replace_na(metric3, 0)) |>
  #  mutate(metric4_total = replace_na(metric4, 0)) |>
  #mutate(metric5_total = replace_na(metric5, 0)) |>
  mutate(metric6_total = replace_na(metric6, 0)) |>
  mutate(metric7_total = replace_na(metric7, 0)) |>
  mutate(metric8_total = replace_na(metric8, 0)) |>
  mutate(metric9_total = replace_na(metric9, 0)) |>
  #mutate(metric10_total = replace_na(metric10, 0)) |>
  #mutate(metric30_total = replace_na(metric30, 0)) |> 
  mutate(metric11_total = replace_na(metric11, 0)) |> 
  #mutate(metric12_total = replace_na(metric12, 0)) |> 
  mutate(metric13_total = replace_na(metric13, 0)) |>
  mutate(metric13b_total = replace_na(metric13b, 0)) |>
  mutate(metric14_total = replace_na(metric14, 0)) |> 
  mutate(metric15_total = replace_na(metric15, 0)) |> 
  mutate(metric16_total = replace_na(metric16, 0)) |>
  mutate(metric16b_total = replace_na(metric16b, 0)) |>
  mutate(metric17_total = replace_na(metric17, 0)) |>
  mutate(metric18_total = replace_na(metric18, 0)) |> 
  mutate(metric19_total = replace_na(metric19, 0)) |>
  mutate(metric20_total = replace_na(metric20, 0)) |>
  mutate(metric21_total = replace_na(metric21, 0)) |> 
  mutate(metric22_total = replace_na(metric22, 0)) |> 
  mutate(metric23_total = replace_na(metric23, 0)) |>
  #24
  mutate(metric25b_total = replace_na(metric25b, 0)) |>
  #mutate(Metric26_total = replace_na(`Metric26`, 0)) |>
  mutate(metric27_total = replace_na(metric27, 0)) |>
  mutate(metric28_total = replace_na(metric28, 0)) |>
  mutate(metric29_total = replace_na(metric29, 0)) |>
  #mutate(metric30_total = replace_na(metric30, 0)) |>  
  mutate(metric31_total = replace_na(metric31, 0)) |>
  mutate(metric32_total = replace_na(metric32, 0)) |>
  #33
  mutate(metric34_total = replace_na(metric34, 0)) |>
  mutate(metric38_total = replace_na(metric38, 0)) |>
  mutate(metric39_total = replace_na(metric39, 0)) |>
  mutate(metric40_total = replace_na(metric40, 0)) |>
  
  group_by (cluster2)|>
  
  summarise(list_size_total = sum(list_size_total) ,
            #metric2_total = sum(metric2_total) ,
            #metric5_total = sum(metric5_total) , 
            metric6_total = sum(metric6_total) , 
            metric7_total = sum(metric7_total) , 
            metric8_total = sum(metric8_total) , 
            metric9_total = sum(metric9_total) , 
            #10
            metric11_total = sum(metric11_total) , 
            #12
            metric13_total = sum(metric13_total) , 
            metric13b_total = sum(metric13b_total) , 
            metric14_total = sum(metric14_total) , 
            metric15_total = sum(metric15_total) , 
            metric16_total = sum(metric16_total) , 
            metric16b_total = sum(metric16b_total) , 
            metric17_total = sum(metric17_total) , 
            metric18_total = sum(metric18_total) , 
            metric19_total = sum(metric19_total) , 
            metric20_total = sum(metric20_total) , 
            metric21_total = sum(metric21_total) , 
            metric22_total = sum(metric22_total) , 
            metric23_total = sum(metric23_total) , 
            #24
            metric25b_total = sum(metric25b_total) , 
            #26
            metric27_total = sum(metric27_total) ,            
            metric28_total = sum(metric28_total) ,   
            metric29_total = sum(metric29_total) ,   
            #30
            metric31_total = sum(metric31_total) ,            
            metric32_total = sum(metric32_total) ,  
            #33
            metric34_total = sum(metric34_total) ,  
            metric38_total = sum(metric38_total) ,
            metric39_total = sum(metric39_total) ,            
            metric40_total = sum(metric40_total) ,  
            metric1_total = sum(metric1_total) )|> 
  arrange()

return(activity_by_type_clusters_stg1)
}

get_cluster2_map <- function(clustered_gp_and_metrics,gp_geocoded){
  
  # Map the clusters
  # get domain of numeric data
  domain <- range(clustered_gp_and_metrics$cluster2)
  # make a colour palette
  pal <- colorNumeric(palette = brewer.pal(5, "Set1"), domain = domain)
  
  map_data <- clustered_gp_and_metrics |>
    rownames_to_column(var = 'practice_code') |>
    select(gp_practice_code,cluster2) |>
    
    left_join(gp_geocoded |>
                mutate(id = row_number()), join_by(gp_practice_code==org_code)) |>
    filter(postcode != "NA") |>
    mutate(lon = sf::st_coordinates(geometry)[,1],
           lat = sf::st_coordinates(geometry)[,2])
  
  map_plot2 <- leaflet(map_data) |>
    addTiles() |>
    addCircleMarkers(color = ~pal(cluster2),radius=3.5,
                     label = ~as.character(paste0(gp_practice_code,"- Cluster: ",cluster2)),
                     stroke = FALSE, fillOpacity = 0.75
    )
  
  return(map_plot2)
  
}

get_cluster1_map <- function(clustered_gp_and_metrics,gp_geocoded){
  
  # Map the clusters
  # get domain of numeric data
  domain <- range(clustered_gp_and_metrics$cluster1)
  # make a colour palette
  pal <- colorNumeric(palette = brewer.pal(5, "Set1"), domain = domain)
  
  map_data <- clustered_gp_and_metrics |>
    rownames_to_column(var = 'practice_code') |>
    select(gp_practice_code,cluster1) |>
    
    left_join(gp_geocoded |>
                mutate(id = row_number()), join_by(gp_practice_code==org_code)) |>
    filter(postcode != "NA") |>
    mutate(lon = sf::st_coordinates(geometry)[,1],
           lat = sf::st_coordinates(geometry)[,2])
  
  map_plot1 <- leaflet(map_data) |>
    addTiles() |>
    addCircleMarkers(color = ~pal(cluster1),radius=3.5,
                     label = ~as.character(paste0(gp_practice_code,"- Cluster: ",cluster1)),
                     stroke = FALSE, fillOpacity = 0.75
    )
  
  return(map_plot1)
  
}

######

get_cluster2_chart <- function(clustered_gp_and_metrics){
  
  
  titles <-clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    mutate(title= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster2, " - ", round(med_percent, digits = 0), "% White British")),
    ) |>
    filter(ethnicity =="gp_perc_est_white_british")|>
    select(cluster2,title)
  
  cluster2_chart <- clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    filter(ethnicity !="gp_perc_est_white_british")|>
    left_join(titles)|>
    ggplot(aes(x=fct_rev(factor(ethnicity)) , y=med_percent, fill=factor(ethnicity))) +
    geom_col() +
    coord_flip() +
    ylab("Median Percent of GP Lists") +
    xlab("") +
    facet_wrap(cluster2 ~ title) +
    ylim(0,25) +
    labs(title="Ethnicity Median Percent by Cluster") +
    guides(fill = FALSE) 
  
  return(cluster2_chart)
  
}


get_cluster1_chart <- function(clustered_gp_and_metrics){
  
  
  titles <-clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster1,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    mutate(title= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster1, " - ", round(med_percent, digits = 0), "% White British")),
    ) |>
    filter(ethnicity =="gp_perc_est_white_british")|>
    select(cluster1,title)
  
  cluster1_chart <- clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster1,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    filter(ethnicity !="gp_perc_est_white_british")|>
    left_join(titles)|>
    ggplot(aes(x=fct_rev(factor(ethnicity)) , y=med_percent, fill=factor(ethnicity))) +
    geom_col() +
    coord_flip() +
    ylab("Median Percent of GP Lists") +
    xlab("") +
    facet_wrap(cluster1 ~ title) +
    ylim(0,25) +
    labs(title="Ethnicity Median Percent by Cluster") +
    guides(fill = FALSE) 
  
  return(cluster1_chart)
  
}

