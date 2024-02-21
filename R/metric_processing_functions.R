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
                            gp_16andover_pop,
                            metric1_updated,metric2,metric5,metric6,metric7,metric8,metric9,metric11,metric12,metric13,metric13b,
                            metric14,metric15,metric16,metric16b,metric17,metric18,metric19,metric20,
                            metric21,metric22,metric23_updated,metric25b,metric27,metric28,
                            metric29,metric31,metric32_updated,metric33,metric34,metric38,
                            metric39,metric40){
  clustered_gp_and_metrics <-
    final_data_full_cats_percent_over45_5_clusters |> rename(cluster1=cluster)|>
    left_join(final_data_full_cats_percent_5_clusters|>select(gp_practice_code,cluster2=cluster)) |>
    left_join(gp_16andover_pop|>select(gp_practice_code=practice_code,list_size=number_of_patients16andover)) |>
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
    
    #10 - might not be available
    
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
    #26 - to do
    left_join(metric27)|>
    left_join(metric28)|>
    left_join(metric29)|>
    #30 - might not be available
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
    mutate(metric1_total = replace_na(metric1, 0)) |>
    mutate(metric2_num_total = replace_na(metric2_num, 0)) |> 
    mutate(metric2_denom_total = replace_na(metric2_denom, 0)) |>
  #  mutate(metric3_total = replace_na(metric3, 0)) |>
    #  mutate(metric4_total = replace_na(metric4, 0)) |>
    mutate(metric5_num_total = replace_na(metric5_num, 0)) |> 
    mutate(metric5_denom_total = replace_na(metric5_denom, 0)) |>
    mutate(metric6_total = replace_na(metric6, 0)) |>
    mutate(metric7_total = replace_na(metric7, 0)) |>
    mutate(metric8_total = replace_na(metric8, 0)) |>
    mutate(metric9_total = replace_na(metric9, 0)) |>
    #mutate(metric10_total = replace_na(metric10, 0)) |>
    #mutate(metric30_total = replace_na(metric30, 0)) |> 
    mutate(metric11_num_total = replace_na(metric11_num, 0)) |> 
    mutate(metric11_denom_total = replace_na(metric11_denom, 0)) |>
    mutate(metric12_num_total = replace_na(metric12_num, 0)) |> 
    mutate(metric12_denom_total = replace_na(metric12_denom, 0)) |>
    mutate(metric13_total = replace_na(metric13, 0)) |>
    mutate(metric13b_total = replace_na(metric13b, 0)) |>
    mutate(metric14_total = replace_na(metric14, 0)) |> 
    mutate(metric15_total = replace_na(metric15, 0)) |> 
    mutate(metric16_total = replace_na(metric16, 0)) |>
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
    #mutate(Metric26_total = replace_na(`Metric26`, 0)) |>
    mutate(metric27_total = replace_na(metric27, 0)) |>
    mutate(metric28_total = replace_na(metric28, 0)) |>
    mutate(metric29_total = replace_na(metric29, 0)) |>
    #mutate(metric30_total = replace_na(metric30, 0)) |>  
    mutate(metric31_num_total = replace_na(metric31_num, 0)) |> 
    mutate(metric31_denom_total = replace_na(metric31_denom, 0)) |>
    mutate(metric32_num_total = replace_na(metric32_num, 0)) |> 
    mutate(metric32_denom_total = replace_na(metric32_denom, 0)) |>
    mutate(metric33_num_total = replace_na(metric33_num, 0)) |> 
    mutate(metric33_denom_total = replace_na(metric33_denom, 0)) |>
    mutate(metric34_total = replace_na(metric34, 0)) |>
    mutate(metric38_total = replace_na(metric38, 0)) |>
    mutate(metric39_total = replace_na(metric39, 0)) |>
    mutate(metric40_total = replace_na(metric40, 0)) |>
    
    group_by (cluster2)|>
    
    summarise(list_size_total = sum(list_size_total) ,
              metric2_num_total = sum(metric2_num_total) , 
              metric2_denom_total = sum(metric2_denom_total) , 
              metric5_num_total = sum(metric5_num_total) , 
              metric5_denom_total = sum(metric5_denom_total) , 
              metric6_total = sum(metric6_total) , 
              metric7_total = sum(metric7_total) , 
              metric8_total = sum(metric8_total) , 
              metric9_total = sum(metric9_total) , 
              #10
              metric11_num_total = sum(metric11_num_total) , 
              metric11_denom_total = sum(metric11_denom_total) , 
              metric12_num_total = sum(metric12_num_total) , 
              metric12_denom_total = sum(metric12_denom_total) , 
              metric13_total = sum(metric13_total) , 
              metric13b_total = sum(metric13b_total) , 
              metric14_total = sum(metric14_total) , 
              metric15_total = sum(metric15_total) , 
              metric16_total = sum(metric16_total) , 
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
              #26
              metric27_total = sum(metric27_total) ,            
              metric28_total = sum(metric28_total) ,   
              metric29_total = sum(metric29_total) ,   
              #30
              metric31_num_total = sum(metric31_num_total) , 
              metric31_denom_total = sum(metric31_denom_total) ,           
              metric32_num_total = sum(metric32_num_total) , 
              metric32_denom_total = sum(metric32_denom_total) , 
              metric33_num_total = sum(metric33_num_total) , 
              metric33_denom_total = sum(metric33_denom_total) , 
              metric34_total = sum(metric34_total) ,  
              metric38_total = sum(metric38_total) ,
              metric39_total = sum(metric39_total) ,            
              metric40_total = sum(metric40_total) ,  
              metric1_total = sum(metric1_total) )|> 
    arrange()
  
  return(activity_by_type_clusters_stg1)
}

process_metrics_2 <-function(activity_by_type_clusters_stg1){
  activity_by_type_clusters_stg2 <- activity_by_type_clusters_stg1 |>
    mutate(metric2_rate=metric2_num_total/metric2_denom_total,
           metric5_rate=metric5_num_total/metric5_denom_total,
           metric6_rate=metric6_total/list_size_total,
           metric7_rate=metric6_total/list_size_total,
           metric8_rate=metric6_total/list_size_total,
           metric9_rate=metric6_total/list_size_total,
           metric11_rate=metric11_num_total/metric11_denom_total,
           metric12_rate=metric12_num_total/metric12_denom_total,
           #13
           metric14_rate=metric14_total/metric1_total,
           metric15_rate=metric15_total/metric1_total,
           #16
           metric17_rate=metric17_num_total/metric17_denom_total,
           metric18_rate=metric18_total/metric1_total,
           metric19_rate=metric19_total/metric1_total,
           metric20_rate=metric20_total/metric1_total,
           metric21_rate=metric21_total/metric1_total,
           metric22_rate=metric22_total/metric1_total,
           metric23_rate=metric23_total/metric1_total,
           #24
           metric25_rate=metric25_num_total/metric25_denom_total, 
           metric27_rate=metric27_total/metric1_total,
           metric28_rate=metric28_total/metric1_total,
           metric29_rate=metric29_total/metric1_total,
           metric31_rate=metric31_num_total/metric31_denom_total,
           metric32_rate=metric32_num_total/metric32_denom_total,
           metric33_rate=metric33_num_total/metric33_denom_total
           #34
           #38
           #39
           #40
           
           
    )
  return(activity_by_type_clusters_stg2)
}
