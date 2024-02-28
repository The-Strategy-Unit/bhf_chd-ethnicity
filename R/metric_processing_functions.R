################################################################################

# Functions to pre-process metrics

################################################################################

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



#process metric 23 to work out whether los of cabg and pci is below trimpoint
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

combine_metrics17_and_32 <- function(metric32,metric17){
  metric32_updated <- metric17 |>
    left_join(metric32)|>
    select(-metric17_num)|>
    rename(metric32_denom=metric17_denom)
  
  return(metric32_updated)
}

add_all_metrics <- function(final_data_full_cats_percent_over45_5_clusters,final_data_full_cats_percent_5_clusters,
                            gp_16andover_pop,gp_list_45over,
                            metric1_updated,metric2,metric5,metric6,metric7,metric8,metric9,metric11,metric12,metric13,metric13b,
                            metric14,metric15,metric16,metric16b,metric17,metric18,metric19,metric20,
                            metric21,metric22,metric23_updated,metric25b,metric26,metric27,metric28,metric28b,
                            metric29,metric29b,metric31,metric32_updated,metric33,metric34,metric38,
                            metric39,metric40){
  clustered_gp_and_metrics <-
    final_data_full_cats_percent_over45_5_clusters |> rename(cluster1=cluster)|>
    left_join(final_data_full_cats_percent_5_clusters|>select(gp_practice_code,cluster2=cluster)) |>
    left_join(gp_16andover_pop|>select(gp_practice_code=practice_code,list_size=number_of_patients16andover)) |>
    left_join(gp_list_45over|>select(gp_practice_code=practice_code,list_size_45=number_45over)) |>
    left_join(metric1_updated)|>
    mutate(metric1=(metric1/100)*list_size) |>
    left_join(metric2)|>
    
    #3 - deleted
    #4 - deleted
    
    left_join(metric5)|>
    left_join(metric6)|>
    mutate(metric6=(metric6/100)*list_size)|>
    left_join(metric7)|>
    mutate(metric7=(metric7/100)*list_size)|>
    left_join(metric8)|>
    mutate(metric8=(metric8/100)*list_size)|>
    left_join(metric9)|>    
    mutate(metric9=(metric9/100)*list_size)|>
    
    #10 - not available
    
    left_join(metric11)|>
    left_join(metric12)|>
    left_join(metric13)|>
    left_join(metric13b)|>
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
    left_join(metric26)|>
    left_join(metric27)|>
    left_join(metric28)|>
    left_join(metric28b)|>
    left_join(metric29)|>
    left_join(metric29b)|>
    #30 - not available
    left_join(metric31)|>
    left_join(metric32_updated)|>
    left_join(metric33)|>
    left_join(metric34)|>
    #35-37 removed not needed
    left_join(metric38)|>
    left_join(metric39)|>
    left_join(metric40)
  
  return(clustered_gp_and_metrics)
}




process_metrics <-function(clustered_gp_and_metrics){
  
  activity_by_type_clusters_stg1<-clustered_gp_and_metrics |>
    filter(metric1 != "NA") |> # removes 26 practices (need to check details of these....)
    mutate(list_size_total = replace_na(list_size, 0)) |>
    mutate(list_size_45_total = replace_na(list_size_45, 0)) |>
    mutate(metric1_total = replace_na(metric1, 0)) |>
    mutate(metric2_num_total = replace_na(metric2_num, 0)) |> 
    mutate(metric2_denom_total = replace_na(metric2_denom, 0)) |>
    mutate(metric5_num_total = replace_na(metric5_num, 0)) |> 
    mutate(metric5_denom_total = replace_na(metric5_denom, 0)) |>
    mutate(metric6_total = replace_na(metric6, 0)) |>
    mutate(metric7_total = replace_na(metric7, 0)) |>
    mutate(metric8_total = replace_na(metric8, 0)) |>
    mutate(metric9_total = replace_na(metric9, 0)) |>
    mutate(metric11_num_total = replace_na(metric11_num, 0)) |> 
    mutate(metric11_denom_total = replace_na(metric11_denom, 0)) |>
    mutate(metric12_num_total = replace_na(metric12_num, 0)) |> 
    mutate(metric12_denom_total = replace_na(metric12_denom, 0)) |>
    mutate(metric13_num_total = replace_na(metric13_num, 0)) |> 
    mutate(metric13_denom_total = replace_na(metric13_denom, 0)) |>
    mutate(metric13b_total = replace_na(metric13b, 0)) |>
    mutate(metric14_total = replace_na(metric14, 0)) |> 
    mutate(metric15_total = replace_na(metric15, 0)) |> 
    mutate(metric16_num_total = replace_na(metric16_num, 0)) |> 
    mutate(metric16_denom_total = replace_na(metric16_denom, 0)) |>
    mutate(metric16b_total = replace_na(metric16b, 0)) |>
    mutate(metric17_num_total = replace_na(metric17_num, 0)) |> 
    mutate(metric17_denom_total = replace_na(metric17_denom, 0)) |>
    mutate(metric18_total = replace_na(metric18, 0)) |> 
    mutate(metric19_total = replace_na(metric19, 0)) |>
    mutate(metric20_total = replace_na(metric20, 0)) |>
    mutate(metric21_total = replace_na(metric21, 0)) |> 
    mutate(metric22_total = replace_na(metric22, 0)) |> 
    mutate(metric23_total = replace_na(metric23, 0)) |>
    #24
    mutate(metric25_num_total = replace_na(metric25_num, 0)) |> 
    mutate(metric25_denom_total = replace_na(metric25_denom, 0)) |>
    mutate(metric26_total = replace_na(`metric26`, 0)) |>
    mutate(metric27_total = replace_na(metric27, 0)) |>
    mutate(metric28_total = replace_na(metric28, 0)) |>
    mutate(metric28b_total = replace_na(metric28b, 0)) |>
    mutate(metric29_total = replace_na(metric29, 0)) |>
    mutate(metric29b_total = replace_na(metric29b, 0)) |>
    #mutate(metric30_total = replace_na(metric30, 0)) |>  
    mutate(metric31_num_total = replace_na(metric31_num, 0)) |> 
    mutate(metric31_denom_total = replace_na(metric31_denom, 0)) |>
    mutate(metric32_num_total = replace_na(metric32_num, 0)) |> 
    mutate(metric32_denom_total = replace_na(metric32_denom, 0)) |>
    mutate(metric33_num_total = replace_na(metric33_num, 0)) |> 
    mutate(metric33_denom_total = replace_na(metric33_denom, 0)) |>
    mutate(metric34_num_total = replace_na(metric34_num, 0)) |> 
    mutate(metric34_denom_total = replace_na(metric34_denom, 0)) |>
    mutate(metric38_num_total = replace_na(metric38_num, 0)) |> 
    mutate(metric38_denom_total = replace_na(metric38_denom, 0)) |>
    mutate(metric39_num_total = replace_na(metric39_num, 0)) |> 
    mutate(metric39_denom_total = replace_na(metric39_denom, 0)) |>
    mutate(metric40_num_total = replace_na(metric40_num, 0)) |> 
    mutate(metric40_denom_total = replace_na(metric40_denom, 0)) |>
    
    group_by (cluster2)|>
    
    summarise(list_size_total = sum(list_size_total) ,
              list_size_45_total = sum(list_size_45_total),
              metric2_num_total = sum(metric2_num_total) , 
              metric2_denom_total = sum(metric2_denom_total) , 
              metric5_num_total = sum(metric5_num_total) , 
              metric5_denom_total = sum(metric5_denom_total) , 
              metric6_total = sum(metric6_total) , 
              metric7_total = sum(metric7_total) , 
              metric8_total = sum(metric8_total) , 
              metric9_total = sum(metric9_total) , 
              metric11_num_total = sum(metric11_num_total) , 
              metric11_denom_total = sum(metric11_denom_total) , 
              metric12_num_total = sum(metric12_num_total) , 
              metric12_denom_total = sum(metric12_denom_total) , 
              metric13_num_total = sum(metric13_num_total) , 
              metric13_denom_total = sum(metric13_denom_total) , 
              metric13b_total = sum(metric13b_total) , 
              metric14_total = sum(metric14_total) , 
              metric15_total = sum(metric15_total) , 
              metric16_num_total = sum(metric16_num_total) , 
              metric16_denom_total = sum(metric16_denom_total) , 
              metric16b_total = sum(metric16b_total) , 
              metric17_num_total = sum(metric17_num_total) , 
              metric17_denom_total = sum(metric17_denom_total) , 
              metric18_total = sum(metric18_total) , 
              metric19_total = sum(metric19_total) , 
              metric20_total = sum(metric20_total) , 
              metric21_total = sum(metric21_total) , 
              metric22_total = sum(metric22_total) , 
              metric23_total = sum(metric23_total) , 
              #24
              metric25_num_total = sum(metric25_num_total) , 
              metric25_denom_total = sum(metric25_denom_total) , 
              metric26_total = sum(metric26_total) ,  
              metric27_total = sum(metric27_total) ,            
              metric28_total = sum(metric28_total) , 
              metric28b_total = sum(metric28b_total) ,
              metric29_total = sum(metric29_total) ,
              metric29b_total = sum(metric29b_total) ,
              metric31_num_total = sum(metric31_num_total) , 
              metric31_denom_total = sum(metric31_denom_total) ,           
              metric32_num_total = sum(metric32_num_total) , 
              metric32_denom_total = sum(metric32_denom_total) , 
              metric33_num_total = sum(metric33_num_total) , 
              metric33_denom_total = sum(metric33_denom_total) , 
              metric34_num_total = sum(metric34_num_total) , 
              metric34_denom_total = sum(metric34_denom_total) ,  
              metric38_num_total = sum(metric38_num_total) , 
              metric38_denom_total = sum(metric38_denom_total) ,
              metric39_num_total = sum(metric39_num_total) , 
              metric39_denom_total = sum(metric39_denom_total) ,            
              metric40_num_total = sum(metric40_num_total) , 
              metric40_denom_total = sum(metric40_denom_total) ,  
              metric1_total = sum(metric1_total) )|> 
    arrange()
  
  return(activity_by_type_clusters_stg1)
}

calc_iod_rate <-function(activity_by_type_clusters_stg1){
  activity_by_type_clusters_stg2 <- activity_by_type_clusters_stg1 |>
    mutate(metric2_rate=metric2_num_total/list_size_total,
           metric5_rate=metric5_num_total/list_size_total,
           metric6_rate=metric6_total/list_size_total,
           metric7_rate=metric7_total/list_size_total,
           metric8_rate=metric8_total/list_size_total,
           metric9_rate=metric9_total/list_size_total,
           metric11_rate=metric11_num_total/metric1_total,
           metric12_rate=metric12_num_total/metric1_total,
           metric13_rate=metric13_num_total/metric1_total,
           metric14_rate=metric14_total/metric1_total,
           metric15_rate=metric15_total/metric1_total,
           metric16_rate=metric16_num_total/metric1_total,
           #16b
           metric17_rate=metric17_num_total/metric1_total,
           metric18_rate=metric18_total/metric1_total,
           metric19_rate=metric19_total/metric1_total,
           metric20_rate=metric20_total/metric1_total,
           metric21_rate=metric21_total/metric1_total,
           metric22_rate=metric22_total/metric1_total,
           metric23_rate=metric23_total/metric1_total,
           #24
           metric25_rate=metric25_num_total/metric1_total, 
           metric26_rate=metric26_total/metric1_total,
           metric27_rate=metric27_total/metric1_total,
           metric28_rate=metric28_total/metric1_total,
           metric28b_rate=metric28b_total/metric1_total,
           metric29_rate=metric29_total/metric1_total,
           metric29b_rate=metric29b_total/metric1_total,
           metric31_rate=metric31_num_total/metric1_total,
           metric32_rate=metric32_num_total/metric1_total,
           metric33_rate=metric33_num_total/list_size_45_total,
           metric34_rate=metric34_num_total/metric1_total,
           metric38_rate=metric38_num_total/metric1_total,
           metric39_rate=metric39_num_total/metric1_total,
           metric40_rate=metric40_num_total/metric1_total
           
           
    )
  return(activity_by_type_clusters_stg2)
}

calc_iod_global_rate <-function(activity_by_type_clusters_stg2){
activity_by_type_clusters_stg3 <- activity_by_type_clusters_stg2|>
  mutate(metric2_overall_num_total=sum(metric2_num_total),
         metric2_overall_denom_total=sum(list_size_total),
         metric2_global_rate=metric2_overall_num_total/metric2_overall_denom_total,
         metric2_upper_ci=(1/metric2_overall_denom_total)*((sqrt(metric2_overall_num_total)*1.96)+metric2_overall_num_total),
         metric2_lower_ci=(1/metric2_overall_denom_total)*((sqrt(metric2_overall_num_total)*1.96)-metric2_overall_num_total),
         metric5_overall_num_total=sum(metric5_num_total),
         metric5_overall_denom_total=sum(list_size_total),
         metric5_global_rate=metric5_overall_num_total/metric5_overall_denom_total,
         metric5_upper_ci=(1/metric5_overall_denom_total)*((sqrt(metric5_overall_num_total)*1.96)+metric5_overall_num_total),
         metric5_lower_ci=(1/metric5_overall_denom_total)*((sqrt(metric5_overall_num_total)*1.96)-metric5_overall_num_total),
         metric6_overall_num_total=sum(metric6_total),
         metric6_overall_denom_total=sum(list_size_total),
         metric6_global_rate=metric6_overall_num_total/metric6_overall_denom_total,
         metric6_upper_ci=(1/metric6_overall_denom_total)*((sqrt(metric6_overall_num_total)*1.96)+metric6_overall_num_total),
         metric6_lower_ci=(1/metric6_overall_denom_total)*((sqrt(metric6_overall_num_total)*1.96)-metric6_overall_num_total),
         metric7_overall_num_total=sum(metric7_total),
         metric7_overall_denom_total=sum(list_size_total),
         metric7_global_rate=metric7_overall_num_total/metric7_overall_denom_total,
         metric7_upper_ci=(1/metric7_overall_denom_total)*((sqrt(metric7_overall_num_total)*1.96)+metric7_overall_num_total),
         metric7_lower_ci=(1/metric7_overall_denom_total)*((sqrt(metric7_overall_num_total)*1.96)-metric7_overall_num_total),
         metric8_overall_num_total=sum(metric8_total),
         metric8_overall_denom_total=sum(list_size_total),
         metric8_global_rate=metric8_overall_num_total/metric8_overall_denom_total,
         metric8_upper_ci=(1/metric8_overall_denom_total)*((sqrt(metric8_overall_num_total)*1.96)+metric8_overall_num_total),
         metric8_lower_ci=(1/metric8_overall_denom_total)*((sqrt(metric8_overall_num_total)*1.96)-metric8_overall_num_total),      
         metric9_overall_num_total=sum(metric9_total),
         metric9_overall_denom_total=sum(list_size_total),
         metric9_global_rate=metric9_overall_num_total/metric9_overall_denom_total,
         metric9_upper_ci=(1/metric9_overall_denom_total)*((sqrt(metric9_overall_num_total)*1.96)+metric9_overall_num_total),
         metric9_lower_ci=(1/metric9_overall_denom_total)*((sqrt(metric9_overall_num_total)*1.96)-metric9_overall_num_total),
         metric11_overall_num_total=sum(metric11_num_total),
         metric11_overall_denom_total=sum(metric1_total),
         metric11_global_rate=metric11_overall_num_total/metric11_overall_denom_total,
         metric11_upper_ci=(1/metric11_overall_denom_total)*((sqrt(metric11_overall_num_total)*1.96)+metric11_overall_num_total),
         metric11_lower_ci=(1/metric11_overall_denom_total)*((sqrt(metric11_overall_num_total)*1.96)-metric11_overall_num_total),
         metric12_overall_num_total=sum(metric12_num_total),
         metric12_overall_denom_total=sum(metric1_total),
         metric12_global_rate=metric12_overall_num_total/metric12_overall_denom_total,
         metric12_upper_ci=(1/metric12_overall_denom_total)*((sqrt(metric12_overall_num_total)*1.96)+metric12_overall_num_total),
         metric12_lower_ci=(1/metric12_overall_denom_total)*((sqrt(metric12_overall_num_total)*1.96)-metric12_overall_num_total),     
         metric13_overall_num_total=sum(metric13_num_total),
         metric13_overall_denom_total=sum(metric1_total),
         metric13_global_rate=metric13_overall_num_total/metric13_overall_denom_total,
         metric13_upper_ci=(1/metric13_overall_denom_total)*((sqrt(metric13_overall_num_total)*1.96)+metric13_overall_num_total),
         metric13_lower_ci=(1/metric13_overall_denom_total)*((sqrt(metric13_overall_num_total)*1.96)-metric13_overall_num_total),
         metric14_overall_num_total=sum(metric14_total),
         metric14_overall_denom_total=sum(metric1_total),
         metric14_global_rate=metric14_overall_num_total/metric14_overall_denom_total,
         metric14_upper_ci=(1/metric14_overall_denom_total)*((sqrt(metric14_overall_num_total)*1.96)+metric14_overall_num_total),
         metric14_lower_ci=(1/metric14_overall_denom_total)*((sqrt(metric14_overall_num_total)*1.96)-metric14_overall_num_total),
         metric15_overall_num_total=sum(metric15_total),
         metric15_overall_denom_total=sum(metric1_total),
         metric15_global_rate=metric15_overall_num_total/metric15_overall_denom_total, 
         metric15_upper_ci=(1/metric15_overall_denom_total)*((sqrt(metric15_overall_num_total)*1.96)+metric15_overall_num_total),
         metric15_lower_ci=(1/metric15_overall_denom_total)*((sqrt(metric15_overall_num_total)*1.96)-metric15_overall_num_total),
         metric16_overall_num_total=sum(metric16_num_total),
         metric16_overall_denom_total=sum(metric1_total),
         metric16_global_rate=metric16_overall_num_total/metric16_overall_denom_total,
         metric16_upper_ci=(1/metric16_overall_denom_total)*((sqrt(metric16_overall_num_total)*1.96)+metric16_overall_num_total),
         metric16_lower_ci=(1/metric16_overall_denom_total)*((sqrt(metric16_overall_num_total)*1.96)-metric16_overall_num_total),
         #16b
         metric17_overall_num_total=sum(metric17_num_total),
         metric17_overall_denom_total=sum(metric1_total),
         metric17_global_rate=metric17_overall_num_total/metric17_overall_denom_total,
         metric17_upper_ci=(1/metric17_overall_denom_total)*((sqrt(metric17_overall_num_total)*1.96)+metric17_overall_num_total),
         metric17_lower_ci=(1/metric17_overall_denom_total)*((sqrt(metric17_overall_num_total)*1.96)-metric17_overall_num_total),
         metric18_overall_num_total=sum(metric18_total),
         metric18_overall_denom_total=sum(metric1_total),
         metric18_global_rate=metric18_overall_num_total/metric18_overall_denom_total,
         metric18_upper_ci=(1/metric18_overall_denom_total)*((sqrt(metric18_overall_num_total)*1.96)+metric18_overall_num_total),
         metric18_lower_ci=(1/metric18_overall_denom_total)*((sqrt(metric18_overall_num_total)*1.96)-metric18_overall_num_total),
         metric19_overall_num_total=sum(metric19_total),
         metric19_overall_denom_total=sum(metric1_total),
         metric19_global_rate=metric19_overall_num_total/metric19_overall_denom_total, 
         metric19_upper_ci=(1/metric19_overall_denom_total)*((sqrt(metric19_overall_num_total)*1.96)+metric19_overall_num_total),
         metric19_lower_ci=(1/metric19_overall_denom_total)*((sqrt(metric19_overall_num_total)*1.96)-metric19_overall_num_total),
         metric20_overall_num_total=sum(metric20_total),
         metric20_overall_denom_total=sum(metric1_total),
         metric20_global_rate=metric20_overall_num_total/metric20_overall_denom_total, 
         metric20_upper_ci=(1/metric20_overall_denom_total)*((sqrt(metric20_overall_num_total)*1.96)+metric20_overall_num_total),
         metric20_lower_ci=(1/metric20_overall_denom_total)*((sqrt(metric20_overall_num_total)*1.96)-metric20_overall_num_total),
         metric21_overall_num_total=sum(metric21_total),
         metric21_overall_denom_total=sum(metric1_total),
         metric21_global_rate=metric21_overall_num_total/metric21_overall_denom_total, 
         metric21_upper_ci=(1/metric21_overall_denom_total)*((sqrt(metric21_overall_num_total)*1.96)+metric21_overall_num_total),
         metric21_lower_ci=(1/metric21_overall_denom_total)*((sqrt(metric21_overall_num_total)*1.96)-metric21_overall_num_total),
         metric22_overall_num_total=sum(metric22_total),
         metric22_overall_denom_total=sum(metric1_total),
         metric22_global_rate=metric22_overall_num_total/metric22_overall_denom_total, 
         metric22_upper_ci=(1/metric22_overall_denom_total)*((sqrt(metric22_overall_num_total)*1.96)+metric22_overall_num_total),
         metric22_lower_ci=(1/metric22_overall_denom_total)*((sqrt(metric22_overall_num_total)*1.96)-metric22_overall_num_total),
         metric23_overall_num_total=sum(metric23_total),
         metric23_overall_denom_total=sum(metric1_total),
         metric23_global_rate=metric23_overall_num_total/metric23_overall_denom_total, 
         metric23_upper_ci=(1/metric23_overall_denom_total)*((sqrt(metric23_overall_num_total)*1.96)+metric23_overall_num_total),
         metric23_lower_ci=(1/metric23_overall_denom_total)*((sqrt(metric23_overall_num_total)*1.96)-metric23_overall_num_total),
         #24
         metric25_overall_num_total=sum(metric25_num_total),
         metric25_overall_denom_total=sum(metric1_total),
         metric25_global_rate=metric25_overall_num_total/metric25_overall_denom_total,
         metric25_upper_ci=(1/metric25_overall_denom_total)*((sqrt(metric25_overall_num_total)*1.96)+metric25_overall_num_total),
         metric25_lower_ci=(1/metric25_overall_denom_total)*((sqrt(metric25_overall_num_total)*1.96)-metric25_overall_num_total),
         metric26_overall_num_total=sum(metric26_total),
         metric26_overall_denom_total=sum(metric1_total),
         metric26_global_rate=metric26_overall_num_total/metric26_overall_denom_total,
         metric26_upper_ci=(1/metric26_overall_denom_total)*((sqrt(metric26_overall_num_total)*1.96)+metric26_overall_num_total),
         metric26_lower_ci=(1/metric26_overall_denom_total)*((sqrt(metric26_overall_num_total)*1.96)-metric26_overall_num_total),
         metric27_overall_num_total=sum(metric27_total),
         metric27_overall_denom_total=sum(metric1_total),
         metric27_global_rate=metric27_overall_num_total/metric27_overall_denom_total, 
         metric27_upper_ci=(1/metric27_overall_denom_total)*((sqrt(metric27_overall_num_total)*1.96)+metric27_overall_num_total),
         metric27_lower_ci=(1/metric27_overall_denom_total)*((sqrt(metric27_overall_num_total)*1.96)-metric27_overall_num_total),
         metric28_overall_num_total=sum(metric28_total),
         metric28_overall_denom_total=sum(metric1_total),
         metric28_global_rate=metric28_overall_num_total/metric28_overall_denom_total,
         metric28_upper_ci=(1/metric28_overall_denom_total)*((sqrt(metric28_overall_num_total)*1.96)+metric28_overall_num_total),
         metric28_lower_ci=(1/metric28_overall_denom_total)*((sqrt(metric28_overall_num_total)*1.96)-metric28_overall_num_total),
         metric28b_overall_num_total=sum(metric28b_total),
         metric28b_overall_denom_total=sum(metric1_total),
         metric28b_global_rate=metric28b_overall_num_total/metric28b_overall_denom_total, 
         metric28b_upper_ci=(1/metric28b_overall_denom_total)*((sqrt(metric28b_overall_num_total)*1.96)+metric28b_overall_num_total),
         metric28b_lower_ci=(1/metric28b_overall_denom_total)*((sqrt(metric28b_overall_num_total)*1.96)-metric28b_overall_num_total),
         metric29_overall_num_total=sum(metric29_total),
         metric29_overall_denom_total=sum(metric1_total),
         metric29_global_rate=metric29_overall_num_total/metric29_overall_denom_total, 
         metric29_upper_ci=(1/metric29_overall_denom_total)*((sqrt(metric29_overall_num_total)*1.96)+metric29_overall_num_total),
         metric29_lower_ci=(1/metric29_overall_denom_total)*((sqrt(metric29_overall_num_total)*1.96)-metric29_overall_num_total),
         metric29b_overall_num_total=sum(metric29b_total),
         metric29b_overall_denom_total=sum(metric1_total),
         metric29b_global_rate=metric29b_overall_num_total/metric29b_overall_denom_total, 
         metric29b_upper_ci=(1/metric29b_overall_denom_total)*((sqrt(metric29b_overall_num_total)*1.96)+metric29b_overall_num_total),
         metric29b_lower_ci=(1/metric29b_overall_denom_total)*((sqrt(metric29b_overall_num_total)*1.96)-metric29b_overall_num_total),
         metric31_overall_num_total=sum(metric31_num_total),
         metric31_overall_denom_total=sum(metric1_total),
         metric31_global_rate=metric31_overall_num_total/metric31_overall_denom_total,
         metric31_upper_ci=(1/metric31_overall_denom_total)*((sqrt(metric31_overall_num_total)*1.96)+metric31_overall_num_total),
         metric31_lower_ci=(1/metric31_overall_denom_total)*((sqrt(metric31_overall_num_total)*1.96)-metric31_overall_num_total),
         metric32_overall_num_total=sum(metric32_num_total),
         metric32_overall_denom_total=sum(metric1_total),
         metric32_global_rate=metric32_overall_num_total/metric32_overall_denom_total,
         metric32_upper_ci=(1/metric32_overall_denom_total)*((sqrt(metric32_overall_num_total)*1.96)+metric32_overall_num_total),
         metric32_lower_ci=(1/metric32_overall_denom_total)*((sqrt(metric32_overall_num_total)*1.96)-metric32_overall_num_total),
         metric33_overall_num_total=sum(metric33_num_total),
         metric33_overall_denom_total=sum(list_size_45_total),
         metric33_global_rate=metric33_overall_num_total/metric33_overall_denom_total,
         metric33_upper_ci=(1/metric33_overall_denom_total)*((sqrt(metric33_overall_num_total)*1.96)+metric33_overall_num_total),
         metric33_lower_ci=(1/metric33_overall_denom_total)*((sqrt(metric33_overall_num_total)*1.96)-metric33_overall_num_total),
         metric34_overall_num_total=sum(metric34_num_total),
         metric34_overall_denom_total=sum(metric1_total),
         metric34_global_rate=metric34_overall_num_total/metric34_overall_denom_total,
         metric34_upper_ci=(1/metric34_overall_denom_total)*((sqrt(metric34_overall_num_total)*1.96)+metric34_overall_num_total),
         metric34_lower_ci=(1/metric34_overall_denom_total)*((sqrt(metric34_overall_num_total)*1.96)-metric34_overall_num_total),
         metric38_overall_num_total=sum(metric38_num_total),
         metric38_overall_denom_total=sum(metric1_total),
         metric38_global_rate=metric38_overall_num_total/metric38_overall_denom_total,
         metric38_upper_ci=(1/metric38_overall_denom_total)*((sqrt(metric38_overall_num_total)*1.96)+metric38_overall_num_total),
         metric38_lower_ci=(1/metric38_overall_denom_total)*((sqrt(metric38_overall_num_total)*1.96)-metric38_overall_num_total),
         metric39_overall_num_total=sum(metric39_num_total),
         metric39_overall_denom_total=sum(metric1_total),
         metric39_global_rate=metric39_overall_num_total/metric39_overall_denom_total,
         metric39_upper_ci=(1/metric39_overall_denom_total)*((sqrt(metric39_overall_num_total)*1.96)+metric39_overall_num_total),
         metric39_lower_ci=(1/metric39_overall_denom_total)*((sqrt(metric39_overall_num_total)*1.96)-metric39_overall_num_total),
         metric40_overall_num_total=sum(metric40_num_total),
         metric40_overall_denom_total=sum(metric1_total),
         metric40_global_rate=metric40_overall_num_total/metric40_overall_denom_total,
         metric40_upper_ci=(1/metric40_overall_denom_total)*((sqrt(metric40_overall_num_total)*1.96)+metric40_overall_num_total),
         metric40_lower_ci=(1/metric40_overall_denom_total)*((sqrt(metric40_overall_num_total)*1.96)-metric40_overall_num_total)
         )

  return(activity_by_type_clusters_stg3)
}


calc_iod_diff_rate <-function(activity_by_type_clusters_stg3){
  activity_by_type_clusters_stg4 <- activity_by_type_clusters_stg3|>
    mutate(metric2_diff_rate=metric2_rate-metric2_global_rate,
           metric5_diff_rate=metric5_rate-metric5_global_rate,
           metric6_diff_rate=metric6_rate-metric6_global_rate,
           metric7_diff_rate=metric7_rate-metric7_global_rate,
           metric8_diff_rate=metric8_rate-metric8_global_rate,
           metric9_diff_rate=metric9_rate-metric9_global_rate,         
           metric11_diff_rate=metric11_rate-metric11_global_rate,
           metric12_diff_rate=metric12_rate-metric12_global_rate,
           metric13_diff_rate=metric13_rate-metric13_global_rate,
           metric14_diff_rate=metric14_rate-metric14_global_rate,
           metric15_diff_rate=metric15_rate-metric15_global_rate,
           metric16_diff_rate=metric16_rate-metric16_global_rate,
           #16b
           metric17_diff_rate=metric17_rate-metric17_global_rate,
           metric18_diff_rate=metric18_rate-metric18_global_rate,
           metric19_diff_rate=metric19_rate-metric19_global_rate,          
           metric20_diff_rate=metric20_rate-metric20_global_rate,
           metric21_diff_rate=metric21_rate-metric21_global_rate,
           metric22_diff_rate=metric22_rate-metric22_global_rate,
           metric23_diff_rate=metric23_rate-metric23_global_rate,
           #24
           metric25_diff_rate=metric25_rate-metric25_global_rate,
           metric26_diff_rate=metric26_rate-metric26_global_rate,
           metric27_diff_rate=metric27_rate-metric27_global_rate,
           metric28_diff_rate=metric28_rate-metric28_global_rate,
           metric28b_diff_rate=metric28b_rate-metric28b_global_rate,
           metric29_diff_rate=metric29_rate-metric29_global_rate,
           metric29b_diff_rate=metric29b_rate-metric29b_global_rate,     
           metric31_diff_rate=metric31_rate-metric31_global_rate,
           metric32_diff_rate=metric32_rate-metric32_global_rate,
           metric33_diff_rate=metric33_rate-metric33_global_rate,        
           metric34_diff_rate=metric34_rate-metric34_global_rate, 
           metric38_diff_rate=metric38_rate-metric38_global_rate,
           metric39_diff_rate=metric39_rate-metric39_global_rate,
           metric40_diff_rate=metric40_rate-metric40_global_rate,
           
           metric2_abs_diff_rate=abs(metric2_diff_rate),
           metric5_abs_diff_rate=abs(metric5_diff_rate),
           metric6_abs_diff_rate=abs(metric6_diff_rate),
           metric7_abs_diff_rate=abs(metric7_diff_rate),
           metric8_abs_diff_rate=abs(metric8_diff_rate),
           metric9_abs_diff_rate=abs(metric9_diff_rate),      
           metric11_abs_diff_rate=abs(metric11_diff_rate),
           metric12_abs_diff_rate=abs(metric12_diff_rate),
           metric13_abs_diff_rate=abs(metric13_diff_rate),
           metric14_abs_diff_rate=abs(metric14_diff_rate),
           metric15_abs_diff_rate=abs(metric15_diff_rate),
           metric16_abs_diff_rate=abs(metric16_diff_rate),
           #16
           metric17_abs_diff_rate=abs(metric17_diff_rate),
           metric18_abs_diff_rate=abs(metric18_diff_rate),
           metric19_abs_diff_rate=abs(metric19_diff_rate),         
           metric20_abs_diff_rate=abs(metric20_diff_rate),
           metric21_abs_diff_rate=abs(metric21_diff_rate),
           metric22_abs_diff_rate=abs(metric22_diff_rate),
           metric23_abs_diff_rate=abs(metric23_diff_rate),
           #24
           metric25_abs_diff_rate=abs(metric25_diff_rate),
           metric26_abs_diff_rate=abs(metric26_diff_rate),
           metric27_abs_diff_rate=abs(metric27_diff_rate),
           metric28_abs_diff_rate=abs(metric28_diff_rate),
           metric28b_abs_diff_rate=abs(metric28b_diff_rate),
           metric29_abs_diff_rate=abs(metric29_diff_rate),  
           metric29b_abs_diff_rate=abs(metric29b_diff_rate),     
           metric31_abs_diff_rate=abs(metric31_diff_rate),
           metric32_abs_diff_rate=abs(metric32_diff_rate),
           metric33_abs_diff_rate=abs(metric33_diff_rate),     
           metric34_abs_diff_rate=abs(metric34_diff_rate),
           metric38_abs_diff_rate=abs(metric38_diff_rate),
           metric39_abs_diff_rate=abs(metric39_diff_rate),
           metric40_abs_diff_rate=abs(metric40_diff_rate)

    )
  
  return(activity_by_type_clusters_stg4)
}

calc_iod_diff <-function(activity_by_type_clusters_stg4){
  activity_by_type_clusters_stg5 <- activity_by_type_clusters_stg4|>
    mutate(metric2_diff=metric2_diff_rate*list_size_total,
           metric5_diff=metric5_diff_rate*list_size_total,
           metric6_diff=metric6_diff_rate*list_size_total,
           metric7_diff=metric7_diff_rate*list_size_total,
           metric8_diff=metric8_diff_rate*list_size_total,
           metric9_diff=metric9_diff_rate*list_size_total,         
           metric11_diff=metric11_diff_rate*metric1_total,
           metric12_diff=metric12_diff_rate*metric1_total,
           metric13_diff=metric13_diff_rate*metric1_total,
           metric14_diff=metric14_diff_rate*metric1_total,
           metric15_diff=metric15_diff_rate*metric1_total,
           metric16_diff=metric16_diff_rate*metric1_total,
           #16b
           metric17_diff=metric17_diff_rate*metric1_total,
           metric18_diff=metric18_diff_rate*metric1_total,
           metric19_diff=metric19_diff_rate*metric1_total,
           metric20_diff=metric20_diff_rate*metric1_total,          
           metric21_diff=metric21_diff_rate*metric1_total,  
           metric22_diff=metric22_diff_rate*metric1_total,  
           metric23_diff=metric23_diff_rate*metric1_total,  
           #24
           metric25_diff=metric25_diff_rate*metric1_total, 
           metric26_diff=metric26_diff_rate*metric1_total,
           metric27_diff=metric27_diff_rate*metric1_total,  
           metric28_diff=metric28_diff_rate*metric1_total, 
           metric28b_diff=metric28b_diff_rate*metric1_total,  
           metric29_diff=metric29_diff_rate*metric1_total,  
           metric29b_diff=metric29b_diff_rate*metric1_total,  
           metric31_diff=metric31_diff_rate*metric1_total,  
           metric32_diff=metric32_diff_rate*metric1_total,  
           metric33_diff=metric33_diff_rate*list_size_45_total,          
           metric34_diff=metric34_diff_rate*metric1_total,
           metric38_diff=metric38_diff_rate*metric1_total,
           metric39_diff=metric39_diff_rate*metric1_total,
           metric40_diff=metric40_diff_rate*metric1_total,
           
           metric2_abs_diff=abs(metric2_diff),
           metric5_abs_diff=abs(metric5_diff),
           metric6_abs_diff=abs(metric6_diff),
           metric7_abs_diff=abs(metric7_diff),
           metric8_abs_diff=abs(metric8_diff),
           metric9_abs_diff=abs(metric9_diff),         
           metric11_abs_diff=abs(metric11_diff),
           metric12_abs_diff=abs(metric12_diff),
           metric13_abs_diff=abs(metric13_diff),
           metric14_abs_diff=abs(metric14_diff),
           metric15_abs_diff=abs(metric15_diff),
           metric16_abs_diff=abs(metric16_diff),
           #16b
           metric17_abs_diff=abs(metric17_diff),
           metric18_abs_diff=abs(metric18_diff),
           metric19_abs_diff=abs(metric19_diff),
           metric20_abs_diff=abs(metric20_diff),          
           metric21_abs_diff=abs(metric21_diff),
           metric22_abs_diff=abs(metric22_diff),  
           metric23_abs_diff=abs(metric23_diff),  
           #24
           metric25_abs_diff=abs(metric25_diff), 
           metric26_abs_diff=abs(metric26_diff),
           metric27_abs_diff=abs(metric27_diff), 
           metric28_abs_diff=abs(metric28_diff), 
           metric28b_abs_diff=abs(metric28b_diff), 
           metric29_abs_diff=abs(metric29_diff),
           metric29b_abs_diff=abs(metric29b_diff),
           metric31_abs_diff=abs(metric31_diff),  
           metric32_abs_diff=abs(metric32_diff), 
           metric33_abs_diff=abs(metric33_diff),        
           metric34_abs_diff=abs(metric34_diff), 
           metric38_abs_diff=abs(metric38_diff), 
           metric39_abs_diff=abs(metric39_diff), 
           metric40_abs_diff=abs(metric40_diff) 
    )
  
  return(activity_by_type_clusters_stg5)
}

calc_abs_iod <- function(activity_by_type_clusters_stg5){
  
  activity_by_type_clusters_stg6 <- activity_by_type_clusters_stg5 |>
    mutate(metric2_abs_iod=sum(metric2_abs_diff)/2,
           metric5_abs_iod=sum(metric5_abs_diff)/2,
           metric6_abs_iod=sum(metric6_abs_diff)/2,
           metric7_abs_iod=sum(metric7_abs_diff)/2,
           metric8_abs_iod=sum(metric8_abs_diff)/2,
           metric9_abs_iod=sum(metric9_abs_diff)/2,
           metric11_abs_iod=sum(metric11_abs_diff)/2,
           metric12_abs_iod=sum(metric12_abs_diff)/2,
           metric13_abs_iod=sum(metric13_abs_diff)/2,
           metric14_abs_iod=sum(metric14_abs_diff)/2,
           metric15_abs_iod=sum(metric15_abs_diff)/2,   
           metric16_abs_iod=sum(metric16_abs_diff)/2, 
           #16b
           metric17_abs_iod=sum(metric17_abs_diff)/2,
           metric18_abs_iod=sum(metric18_abs_diff)/2,
           metric19_abs_iod=sum(metric19_abs_diff)/2,
           metric20_abs_iod=sum(metric20_abs_diff)/2,
           metric21_abs_iod=sum(metric21_abs_diff)/2,
           metric22_abs_iod=sum(metric22_abs_diff)/2,
           metric23_abs_iod=sum(metric23_abs_diff)/2,
           #24
           metric25_abs_iod=sum(metric25_abs_diff)/2,
           metric26_abs_iod=sum(metric26_abs_diff)/2,
           metric27_abs_iod=sum(metric27_abs_diff)/2,
           metric28_abs_iod=sum(metric28_abs_diff)/2,
           metric28b_abs_iod=sum(metric28b_abs_diff)/2,
           metric29_abs_iod=sum(metric29_abs_diff)/2,
           metric29b_abs_iod=sum(metric29b_abs_diff)/2,
           metric31_abs_iod=sum(metric31_abs_diff)/2,
           metric32_abs_iod=sum(metric32_abs_diff)/2,
           metric33_abs_iod=sum(metric33_abs_diff)/2,        
           metric34_abs_iod=sum(metric34_abs_diff)/2,
           metric38_abs_iod=sum(metric38_abs_diff)/2,
           metric39_abs_iod=sum(metric39_abs_diff)/2,
           metric40_abs_iod=sum(metric40_abs_diff)/2,

           metric2_rel_iod=(sum(metric2_abs_diff)/(2*metric2_overall_num_total))*100,
           metric5_rel_iod=(sum(metric5_abs_diff)/(2*metric5_overall_num_total))*100,
           metric6_rel_iod=(sum(metric6_abs_diff)/(2*metric6_overall_num_total))*100,
           metric7_rel_iod=(sum(metric7_abs_diff)/(2*metric7_overall_num_total))*100,
           metric8_rel_iod=(sum(metric8_abs_diff)/(2*metric8_overall_num_total))*100,
           metric9_rel_iod=(sum(metric9_abs_diff)/(2*metric9_overall_num_total))*100,
           metric11_rel_iod=(sum(metric11_abs_diff)/(2*metric11_overall_num_total))*100,
           metric12_rel_iod=(sum(metric12_abs_diff)/(2*metric12_overall_num_total))*100,
           metric13_rel_iod=(sum(metric13_abs_diff)/(2*metric13_overall_num_total))*100,
           metric14_rel_iod=(sum(metric14_abs_diff)/(2*metric14_overall_num_total))*100,
           metric15_rel_iod=(sum(metric15_abs_diff)/(2*metric15_overall_num_total))*100,
           metric16_rel_iod=(sum(metric16_abs_diff)/(2*metric16_overall_num_total))*100,
           #16b
           metric17_rel_iod=(sum(metric17_abs_diff)/(2*metric17_overall_num_total))*100,
           metric18_rel_iod=(sum(metric18_abs_diff)/(2*metric18_overall_num_total))*100,
           metric19_rel_iod=(sum(metric19_abs_diff)/(2*metric19_overall_num_total))*100,
           metric20_rel_iod=(sum(metric20_abs_diff)/(2*metric20_overall_num_total))*100,
           metric21_rel_iod=(sum(metric21_abs_diff)/(2*metric21_overall_num_total))*100,
           metric22_rel_iod=(sum(metric22_abs_diff)/(2*metric22_overall_num_total))*100,
           metric23_rel_iod=(sum(metric23_abs_diff)/(2*metric23_overall_num_total))*100,
           #24
           metric25_rel_iod=(sum(metric25_abs_diff)/(2*metric25_overall_num_total))*100,
           metric26_rel_iod=(sum(metric26_abs_diff)/(2*metric26_overall_num_total))*100,
           metric27_rel_iod=(sum(metric27_abs_diff)/(2*metric27_overall_num_total))*100,
           metric28_rel_iod=(sum(metric28_abs_diff)/(2*metric28_overall_num_total))*100,
           metric28b_rel_iod=(sum(metric28b_abs_diff)/(2*metric28b_overall_num_total))*100,
           metric29_rel_iod=(sum(metric29_abs_diff)/(2*metric29_overall_num_total))*100,
           metric29b_rel_iod=(sum(metric29b_abs_diff)/(2*metric29b_overall_num_total))*100,
           metric31_rel_iod=(sum(metric31_abs_diff)/(2*metric31_overall_num_total))*100,
           metric32_rel_iod=(sum(metric32_abs_diff)/(2*metric32_overall_num_total))*100,
           metric33_rel_iod=(sum(metric33_abs_diff)/(2*metric33_overall_num_total))*100,
           metric34_rel_iod=(sum(metric34_abs_diff)/(2*metric34_overall_num_total))*100,
           metric38_rel_iod=(sum(metric38_abs_diff)/(2*metric38_overall_num_total))*100,
           metric39_rel_iod=(sum(metric39_abs_diff)/(2*metric39_overall_num_total))*100,
           metric40_rel_iod=(sum(metric40_abs_diff)/(2*metric40_overall_num_total))*100
           )
    
  return(activity_by_type_clusters_stg6)
  
}
