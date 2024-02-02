# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","fingertipsR","readxl","tidyverse","utils","janitor",
               "readr","visNetwork","odbc","stringr","MLID","sf","tidygeocoder",
               "cluster","factoextra","purrr","broom","glue","RColorBrewer","leaflet"), # packages that your targets need to run
  format = "rds"

 
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")


# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Target list:
list(
  #Get data
  tar_target(data_path1, "data/metric14.csv", format = "file"), # SUS data metric 14
  tar_target(metric14,read_csv_file(data_path1)|> 
               rename(metric14=number_ct_angiography)),
  tar_target(data_path2, "data/metric15.csv", format = "file"), # SUS data metric 15
  tar_target(metric15,read_csv_file(data_path2)|> 
               rename(metric15=number_electrocardiography)),
  tar_target(data_path3, "data/metric18.csv", format = "file"), # SUS data metric 18
  tar_target(metric18,read_csv_file(data_path3)|> 
               rename(metric18=cardi_op_attend)),
  tar_target(data_path4, "data/metric19.csv", format = "file"), # SUS data metric 19
  tar_target(metric19,read_csv_file(data_path4)|> 
               rename(metric19=cardi_opdna)),
  tar_target(data_path5, "data/metric20.csv", format = "file"), # SUS data metric 20
  tar_target(metric20,read_csv_file(data_path5)|> 
               rename(metric20=elective_pci)),
  tar_target(data_path6, "data/metric21.csv", format = "file"), # SUS data metric 21
  tar_target(metric21,read_csv_file(data_path6)|> 
               rename(metric21=elective_cabg)),
  
  tar_target(data_path7, "data/metric22.csv", format = "file"), # SUS data metric 22
  tar_target(metric22,read_csv_file(data_path7)|>
               group_by(gp_practice_code) |>
               summarise(metric22=median(der_spell_lo_s))),
  
  tar_target(data_path8, "data/metric23.csv", format = "file"), # SUS data metric 23
  tar_target(metric23,read_csv_file(data_path8)),
  
  tar_target(data_path9, "data/metric27.csv", format = "file"), # SUS data metric 27
  tar_target(metric27,read_csv_file(data_path9)|> 
               rename(metric27=number_chd_emerg_admits)),
  tar_target(data_path10, "data/metric28.csv", format = "file"), # SUS data metric 28
  tar_target(metric28,read_csv_file(data_path10)|> 
               rename(metric28=number_chd_deaths)),
  tar_target(data_path11, "data/metric29.csv", format = "file"), # SUS data metric 29
  tar_target(metric29,read_csv_file(data_path11)|> 
               rename(metric29=deaths)),
  
  # tar_target(data_path12, "data/April22GPListLSOA.csv", format = "file"), #GP list size
  # tar_target(gp_list_lsoa,read_csv_file(data_path12)),
  tar_target(data_path13, "data/EthnicityByLSOACensus21.csv", format = "file"), #Ethnicity by LSOA Census 2021
  tar_target(eth_lsoa_census,read_csv_file(data_path13)),
  tar_target(data_path14, "data/gp-reg-pat-prac-lsoa-all.csv", format = "file"), #GP Reg Pat Prac LSOA October 2022
  tar_target(gp_reg_pat_prac_lsoa,read_csv_file(data_path14)),
  tar_target(data_path26, "data/gp-reg-pat-prac-sing-age-female.csv", format = "file"), #GP Reg Pat Prac Females October 2022
  tar_target(gp_reg_pat_prac_sing_age_female, read_csv_file(data_path26)|>
                                                select(org_code,age,number_of_patients)), 
  tar_target(data_path27, "data/gp-reg-pat-prac-sing-age-male.csv", format = "file"), #GP Reg Pat Prac Males October 2022
  tar_target(gp_reg_pat_prac_sing_age_male,read_csv_file(data_path27)|>
               select(org_code,age,number_of_patients)), 
  tar_target(gp_over45_perc,get_over45_perc(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)),
  tar_target(gp_16andover_pop,get_gp_16andover_pop(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)),
 
  tar_target(data_path15, "data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv", format = "file"), #lookup
  tar_target(lsoa_lookup,read_csv_file(data_path15)),
  tar_target(data_path17,"data/gp-reg-pat-prac-map.csv", format = "file"),
  tar_target(gp_icb_mapping,read_csv_file(data_path17)|> 
               select(practice_code,practice_name,sub_icb_location_code, sub_icb_location_name,
                      icb_code,icb_name,comm_region_code,comm_region_name)),  
  
  
  tar_target(data_path21,"data/CVDP008.csv", format = "file"), 
  tar_target(metric38,read_csv_file(data_path21) |>
               select(gp_practice_code=area_code,metric38=numerator)),  #metric38
  
  tar_target(data_path22,"data/CVDP009.csv", format = "file"), 
  tar_target(metric34,read_csv_file(data_path22) |>
               select(gp_practice_code=area_code,metric34=numerator)),  #metric34
  
  tar_target(data_path23,"data/NCDesMarch23.csv", format = "file"), 
  tar_target(ncdes_data,read_csv_file(data_path23)|>
               filter(ind_code %in% c("NCD002","NCD003"),
                      measure %in% c("Denominator", "Numerator")) |>
               pivot_wider(names_from=c(ind_code,measure),
                           names_sep="_",
                           values_from = value)),
  
  tar_target(data_path24,"data/20231201_92847.xlsx", format="file"), #CHD synthetic prevalence metric 1
  tar_target(metric1,read_excel_file(data_path24)), #updated later in pipeline
  tar_target(data_path25,"data/epraccur_gp.csv", format="file"), #All practices
  tar_target(gp_history,read_csv_file(data_path25)),
  tar_target(data_path29,"data/2223_trimpoints.xlsx", format="file"), #TrimPoint
  tar_target(metric23trimpoints,read_excel_file(data_path29)|>
               clean_names()),
  tar_target(metric23_updated,metric23_below_trimpoint(metric23,metric23trimpoints)), #process metric 23
  
  tar_target(metric13,get_my_fingertips_gp_data(273,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric13=Value)), # metric 13
  tar_target(metric6,get_my_fingertips_gp_data(92588,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric6=Value)), # metric 6
  tar_target(metric7,get_my_fingertips_gp_data(241,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric7=Value)), # metric 7
  tar_target(metric8,get_my_fingertips_gp_data(848,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric8=Value)), # metric 8
  
  tar_target(metric16,get_my_fingertips_gp_data(90999,"2021/22")|>
               rename(gp_practice_code=AreaCode,
                      metric16=Value)), # metric 16
  tar_target(metric17,get_my_fingertips_gp_data(91000,"2020/21")|>
               rename(gp_practice_code=AreaCode,
                      metric17=Value)), # metric 17

# metric 9 has been removed from Fingertips :-(  this was the original code
# tar_target(metric9,get_my_fingertips_gp_data(92589,"2019/20")), # metric 9
# had already extracted a copy so now need to use this code to pull into the pipeline
  tar_target(data_path28,"data/metric9.csv", format = "file"),  
  tar_target(metric9,read_csv_file(data_path28)|>
               rename(gp_practice_code=area_code,
                      metric9=value)),# metric 9




# metric 10 has been removed from Fingertips :-(
# tar_target(metric10,get_my_fingertips_gp_data(91248,"2019/20")|>
#  rename(gp_practice_code=AreaCode,
#         metric10=Value)), # metric 10

  tar_target(metric11,get_my_fingertips_gp_data(90619,"2022/23")|>
  rename(gp_practice_code=AreaCode,
         metric11=Value)), # metric 11

  tar_target(data_path16, "data/QOF_CHD_2022_23.xlsx", format = "file"), #QOF CHD indicators
  tar_target(qof_chd_2223,read_qof_excel_file(data_path16) |>
                            select(1,2,3,6,7,15,32,35,37,38,46)|>
                            clean_names() |>
                            filter(practice_code != "NA")),
  tar_target(metric16b, qof_chd_2223 |> 
                          select(4,10) |>
                          rename(gp_practice_code=practice_code,
                                 metric16b=patients_receiving_intervention_percent_38)), # metric 16b 22/23
  tar_target(metric31, qof_chd_2223 |> 
                          select(4,8) |>
                          rename(gp_practice_code=practice_code,
                                 metric31=pc_as_35)), # metric 31 22/23
  tar_target(metric25b, qof_chd_2223 |> 
                          select(4,11) |>
                          rename(gp_practice_code=practice_code,
                                 metric25b=patients_receiving_intervention_percent_46)),   # metric 25b 22/23
  tar_target(metric32,get_data_via_server() |>
               rename(gp_practice_code=practice_code,
                      metric32=value)), # metric 32 20/21
  tar_target(metric13b,qof_chd_2223 |> 
                          select(4,6) |>
                          rename(gp_practice_code=practice_code,
                                 metric13b=prevalence_percent_15)), # metric 13b 22/23
  # tar_target(metric39, ncdes_data|>
  #                         mutate(ncd002_percent =(NCD002_Numerator / NCD002_Denominator)*100) |>
  #                         select(practice_code,ncd002_percent)|>
  #                         rename(gp_practice_code=practice_code,
  #                                metric39=ncd002_percent)), # metric 39 march 23
tar_target(metric39, ncdes_data|>
             select(practice_code,NCD002_Numerator)|>
             rename(gp_practice_code=practice_code,
                    metric39=NCD002_Numerator)), # metric 39 march 23
  # tar_target(metric40, ncdes_data|>
  #                         mutate(ncd003_percent =(NCD003_Numerator / NCD003_Denominator)*100) |>
  #                         select(practice_code,ncd003_percent)|>
  #                         rename(gp_practice_code=practice_code,
  #                                metric40=ncd003_percent)), # metric 40 march 23
tar_target(metric40, ncdes_data|>
             select(practice_code,NCD003_Numerator)|>
             rename(gp_practice_code=practice_code,
                    metric40=NCD003_Numerator)), # metric 40 march 23

  #process ethnicity data
  tar_target(lsoa_eth_sum,process_census21data(eth_lsoa_census)),
  tar_target(gp_lsoa,process_gpdata(gp_reg_pat_prac_lsoa) |>
               left_join(gp_icb_mapping)
             ),
  tar_target(joined,join_gp_and_eth(lsoa_eth_sum,gp_lsoa)),
  tar_target(lsoa_lookup_eng,lsoa_lookup |>
                                filter(str_detect(lsoa21cd, '^E'))),
  tar_target(eth_joined,sum_eth_by_lsoa11(lsoa_lookup_eng,lsoa_eth_sum)),
  #join the gp list to this via the 2011 lsoa column
  #and calculate the number for each ethnicity based on ethnicity and list size
  tar_target(gp_lsoa_with_eth,join_gp_and_eth(eth_joined,gp_lsoa)),
  tar_target(gp_lsoa_with_eth_sum,sum_ethnicities(gp_lsoa_with_eth)),
  #add the perc over 45 on the gp list
  tar_target(gp_lsoa_with_eth_sum_over45perc,join_over45_to_gp_lsoa_with_eth_sum(gp_lsoa_with_eth_sum,gp_over45_perc)),
  
  #where chd synthetic prevalence is missing, calculate an estimate
  tar_target(gp_history_short,get_gp_history_short(gp_history)),
  tar_target(gp_list_summary,get_gp_list_summary(gp_reg_pat_prac_lsoa,gp_history_short)),
  tar_target(joined_gp_history_and_chd_prev,get_joined_gp_history_and_chd_prev(metric1,gp_history_short)),
  tar_target(gp_geocoded,get_geocoded_data(gp_list_summary,orig,joined_gp_history_and_chd_prev)),
  tar_target(metric1_updated,get_missing_chd_prevalence(metric1,gp_history_short,joined_gp_history_and_chd_prev,gp_list_summary,gp_geocoded)|>
               rename(gp_practice_code=org_code,metric1=chd_prev_to_use)), #imputes some of the 151 with no chd prev
  
  #cluster the practices 1
  tar_target(full_cats_percents_over45,get_full_cats_percents_over45(gp_lsoa_with_eth_sum_over45perc)),
  tar_target(scale_full_cats_percents_over45,get_scale_full_cats_percents_over45(full_cats_percents_over45)),
  tar_target(pams_full_cats_percents_over45,get_pams(scale_full_cats_percents_over45)),
  tar_target(final_data_full_cats_percent_over45_5_clusters,get_clusters(pams_full_cats_percents_over45,full_cats_percents_over45)),
  tar_target(elbow_plot_over45,get_elbow_plot(scale_full_cats_percents_over45)),
  tar_target(cluster_plot_over45,get_cluster_plot(pams_full_cats_percents_over45)),
  #cluster the practices 2
  tar_target(full_cats_percents,get_full_cats_percents(gp_lsoa_with_eth_sum)),
  tar_target(scale_full_cats_percents,get_scale_full_cats_percents(full_cats_percents)),
  tar_target(pams_full_cats_percents,get_pams(scale_full_cats_percents)),
  tar_target(final_data_full_cats_percent_5_clusters,get_clusters(pams_full_cats_percents,full_cats_percents)),
  tar_target(elbow_plot,get_elbow_plot(scale_full_cats_percents)),
  tar_target(cluster_plot,get_cluster_plot(pams_full_cats_percents)),


#add in the missing metrics*******

#join the metrics together with the clusters
tar_target(clustered_gp_and_metrics,
           add_all_metrics(final_data_full_cats_percent_over45_5_clusters, 
                           final_data_full_cats_percent_5_clusters,
                           gp_16andover_pop,
                           metric1_updated,metric6,metric7,metric8,metric9,metric11,
                           metric13,metric13b,metric14,metric15,metric16,
                           metric16b,metric17,metric18,metric19,metric20,
                           metric21,metric22,metric23_updated,
                           metric25b,metric27,metric28, metric29,metric31,
                           metric32,metric34,metric38,
                           metric39,metric40)),
#plot clusters
tar_target(cluster2_map,get_cluster2_map(clustered_gp_and_metrics,gp_geocoded)),
tar_target(cluster1_map,get_cluster1_map(clustered_gp_and_metrics,gp_geocoded)),
tar_target(cluster2_chart,get_cluster2_chart(clustered_gp_and_metrics)),
tar_target(cluster1_chart,get_cluster1_chart(clustered_gp_and_metrics)),

#process the data into the correct format inc dividing some things etc
tar_target(activity_by_type_clusters_stg1,process_metrics(clustered_gp_and_metrics))

#######################################################################################
#calculate Disparity Ratio
#tar_target(activity_long,get_disparity_ratio(activity_by_type_clusters_stg1))


)


#-----------------------------------------------------------------------------
#Useful commands:
#targets::tar_meta(fields = error, complete_only = TRUE)
#tar_make()
#tar_read(<name of target>) e.g. tar_read(LSOAPopMales)
#tar_read(<name of target>) |> as_tibble()
#tar_visnetwork()
#tar_meta()
#meta <- tar_meta() 
#diagram <- tar_visnetwork()
#diagram |> visInteraction(navigationButtons=TRUE) |> visOptions(manipulation=TRUE) |> visHierarchicalLayout(direction="UD")
#tar_prune()
#-----------------------------------------------------------------------------