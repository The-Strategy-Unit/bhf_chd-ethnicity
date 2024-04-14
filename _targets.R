# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","fingertipsR","readxl","tidyverse","utils","janitor","forcats",
               "readr","visNetwork","odbc","stringr","MLID","sf","tidygeocoder",
               "cluster","factoextra","purrr","broom","glue","RColorBrewer","leaflet","treemapify"), # packages that your targets need to run
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
  
  tar_target(data_path34, "data/metric26.csv", format = "file"), # SUS data metric 26
  tar_target(metric26,read_csv_file(data_path34)|> 
               rename(metric26=num)),
  
  tar_target(data_path9, "data/metric27.csv", format = "file"), # SUS data metric 27
  tar_target(metric27,read_csv_file(data_path9)|> 
               rename(metric27=number_chd_emerg_admits)),
  tar_target(data_path10, "data/metric28.csv", format = "file"), # SUS data metric 28
  tar_target(metric28,read_csv_file(data_path10)|> 
               rename(metric28=number_chd_deaths)),
  tar_target(data_path32, "data/metric28b.csv", format = "file"), # SUS data metric 28b deaths <75yrs
  tar_target(metric28b,read_csv_file(data_path32)|>
               rename(metric28b=number_chd_deaths)),
  tar_target(data_path11, "data/metric29.csv", format = "file"), # SUS data metric 29
  tar_target(metric29,read_csv_file(data_path11)|> 
               rename(metric29=deaths)),
  tar_target(data_path33, "data/metric29b.csv", format = "file"), # SUS data metric 29b deaths <75yrs
  tar_target(metric29b,read_csv_file(data_path33)|>
               rename(metric29b=deaths)),
  
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
  tar_target(gp_list_45over,get_45over_list(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)),
  tar_target(gp_16andover_pop,get_gp_16andover_pop(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)),
  tar_target(gp_list_all_age,get_allage_list(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)),
 
  tar_target(data_path15, "data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv", format = "file"), #lookup
  tar_target(lsoa_lookup,read_csv_file(data_path15)),
  tar_target(data_path17,"data/gp-reg-pat-prac-map.csv", format = "file"),
  tar_target(gp_icb_mapping,read_csv_file(data_path17)|> 
               select(practice_code,practice_name,sub_icb_location_code, sub_icb_location_name,
                      icb_code,icb_name,comm_region_code,comm_region_name)),  
  
  
  tar_target(data_path21,"data/CVDP008.csv", format = "file"), 
  tar_target(metric38,read_csv_file(data_path21) |>
               select(gp_practice_code=area_code,metric38_num=numerator,metric38_denom=denominator)),  #metric38
  
  tar_target(data_path22,"data/CVDP009.csv", format = "file"), 
  tar_target(metric34,read_csv_file(data_path22) |>
               select(gp_practice_code=area_code,metric34_num=numerator,metric34_denom=denominator)),  #metric34
  
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
  
  tar_target(data_path30,"data/gp_survey_smoking_prevalence.xlsx", format="file"), #Smoking synthetic prevalence estimates metric 2
  tar_target(metric2,read_excel_file(data_path30)|>
               clean_names()|>
               select(gp_practice_code=practice_code,metric2_denom=smoking_habits_unweighted_total_responses,percent_occasional_smoker,percent_regular_smoker)|>
               mutate(metric2_num=(as.numeric(percent_occasional_smoker)+as.numeric(percent_regular_smoker))*metric2_denom)|>
               select(-percent_occasional_smoker,-percent_regular_smoker)), #metric2
  
  tar_target(data_path31,"data/QOFPCASMOK2223.xlsx", format="file"), #Smoking synthetic prevalence estimates metric 12
  tar_target(metric12,read_excel_file(data_path31)|>
               clean_names()|>
               select(gp_practice_code=practice_code,metric12_denom=denominator_plus_pc_as,metric12_num=pc_as)), #metric12
  
  tar_target(metric13,get_my_fingertips_gp_data(273,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric13=Value)), # metric 13
  tar_target(metric5,get_my_fingertips_gp_data_count(91280,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric5_num=Count,
                      metric5_denom=Denominator)), # metric 5
  tar_target(metric6,get_my_fingertips_gp_data(92588,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric6=Value)), # metric 6
  tar_target(metric7,get_my_fingertips_gp_data(241,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric7=Value)), # metric 7
  tar_target(metric8,get_my_fingertips_gp_data(848,"2022/23")|>
               rename(gp_practice_code=AreaCode,
                      metric8=Value)), # metric 8
  tar_target(metric16,get_my_fingertips_gp_data_count(90999,"2021/22")|>
               rename(gp_practice_code=AreaCode,
                      metric16_num=Count,
                      metric16_denom=Denominator)), # metric 16
  # tar_target(metric17,get_my_fingertips_gp_data_count(91000,"2020/21")|>
  #              rename(gp_practice_code=AreaCode,
  #                     metric17_num=Count,
  #                     metric17_denom=Denominator)), # metric 17

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

  tar_target(metric11,get_my_fingertips_gp_data_count(90619,"2022/23")|>
                      rename(gp_practice_code=AreaCode,
                      metric11_num=Count,
                      metric11_denom=Denominator)), # metric 11

  tar_target(data_path16, "data/QOF_CHD_2022_23.xlsx", format = "file"), #QOF CHD indicators
  tar_target(qof_chd_2223,read_qof_excel_file(data_path16) |>
                            select(1,2,3,6,7,11,14,15,32,35,37,38,40,45)|>
                            clean_names() |>
                            filter(practice_code != "NA")),
  tar_target(metric16b, qof_chd_2223 |> 
                          select(4,12) |> #38
                          rename(gp_practice_code=practice_code,
                                 metric16b=patients_receiving_intervention_percent_38)), # metric 16b 22/23
  tar_target(metric31, qof_chd_2223 |> 
                          select(4,10,11) |> #35
                          rename(gp_practice_code=practice_code,
                                 metric31_num=pc_as_35,
                                 metric31_denom=denominator_plus_pc_as_37)), # metric 31 22/23
  tar_target(metric25b, qof_chd_2223 |>
                          select(4,13,14) |> #40 and #45
                          rename(gp_practice_code=practice_code,
                                 metric25_num=numerator_40,
                                 metric25_denom=denominator_plus_pc_as_45)),   # metric 25b 22/23
  # tar_target(metric32,get_data_via_server() |>
  #              rename(gp_practice_code=practice_code,
  #                     metric32_num=value)), # metric 32 20/21
  # tar_target(metric32_updated,combine_metrics17_and_32(metric32,metric17)),
  tar_target(metric13b,qof_chd_2223 |> 
                          select(4,8,6,7) |> #15
                          rename(gp_practice_code=practice_code,
                                 metric13b=prevalence_percent_15,
                                 metric13_num=register_14,
                                 metric13_denom=list_size_11)), # metric 13b 22/23

tar_target(metric33,get_my_fingertips_gp_data_count(91262,"2022/23")|>
             rename(gp_practice_code=AreaCode,
                    metric33_num=Count,
                    metric33_denom=Denominator)), # metric 33


tar_target(metric39, ncdes_data|>
             select(practice_code,NCD002_Numerator,NCD002_Denominator)|>
             rename(gp_practice_code=practice_code,
                    metric39_num=NCD002_Numerator,
                    metric39_denom=NCD002_Denominator)), # metric 39 march 23

tar_target(metric40, ncdes_data|>
             select(practice_code,NCD003_Numerator,NCD003_Denominator)|>
             rename(gp_practice_code=practice_code,
                    metric40_num=NCD003_Numerator,
                    metric40_denom=NCD003_Denominator)), # metric 40 march 23

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
  #add regional breakdown
  tar_target(gp_lsoa_with_eth_sum_region,sum_ethnicities_region(gp_lsoa_with_eth)|>filter(comm_region_code=="Y60")),
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
  tar_target(clusters_for_nacr,get_clusters_for_nacr(clustered_gp_and_metrics,gp_icb_mapping)),

  #cluster the practices by region
#  tar_target(full_cats_percents_region,get_full_cats_percents(gp_lsoa_with_eth_sum_region)),
 # tar_target(scale_full_cats_percents_region,get_scale_full_cats_percents(full_cats_percents_region)),
#  tar_target(pams_full_cats_percents_region,get_pams(scale_full_cats_percents_region)),
 # tar_target(final_data_full_cats_percent_5_clusters_region,get_clusters(pams_full_cats_percents_region,full_cats_percents_region)|>
  #                                                          rename(cluster2=cluster)),
 # tar_target(elbow_plot_region,get_elbow_plot(scale_full_cats_percents_region)),
 # tar_target(cluster_plot_region,get_cluster_plot(pams_full_cats_percents_region)),


#add in the missing metrics*******

#join the metrics together with the clusters
tar_target(clustered_gp_and_metrics,
           add_all_metrics(final_data_full_cats_percent_over45_5_clusters,
                           final_data_full_cats_percent_5_clusters,
                           gp_16andover_pop,gp_list_45over,gp_list_all_age,
                           metric1_updated,metric2,metric5,metric6,metric7,metric8,metric9,metric11,
                           metric12,metric13,metric13b,metric14,metric15,metric16,
                           metric16b,
                           #metric17,
                           metric18,metric19,metric20,
                           metric21,metric22,metric23_updated,metric25b,
                           metric26,metric27,metric28, metric28b, metric29,metric29b,metric31,
                           #metric32_updated,
                           metric33,metric34,metric38,
                           metric39,metric40)),
#plot clusters
tar_target(cluster1_map,get_cluster1_map(clustered_gp_and_metrics,gp_geocoded)),
tar_target(cluster2_map,get_cluster2_map(clustered_gp_and_metrics,gp_geocoded)),
tar_target(cluster2_chart,get_cluster2_chart(clustered_gp_and_metrics)),
tar_target(cluster1_chart,get_cluster1_chart(clustered_gp_and_metrics)),
tar_target(cluster2_treemap_data,get_cluster2_treemap_data(clustered_gp_and_metrics)),
tar_target(cluster2_treemap_1,get_cluster2_treemap_1(cluster2_treemap_data)),
tar_target(cluster2_treemap_2,get_cluster2_treemap_2(cluster2_treemap_data)),
tar_target(cluster2_treemap_3,get_cluster2_treemap_3(cluster2_treemap_data)),
tar_target(cluster2_treemap_4,get_cluster2_treemap_4(cluster2_treemap_data)),
tar_target(cluster2_treemap_5,get_cluster2_treemap_5(cluster2_treemap_data)),
tar_target(cluster2_eth_chart_1,get_cluster2_14_eth_chart(clustered_gp_and_metrics,1)),
tar_target(cluster2_eth_chart_2,get_cluster2_14_eth_chart(clustered_gp_and_metrics,2)),
tar_target(cluster2_eth_chart_3,get_cluster2_14_eth_chart(clustered_gp_and_metrics,3)),
tar_target(cluster2_eth_chart_4,get_cluster2_14_eth_chart(clustered_gp_and_metrics,4)),
tar_target(cluster2_eth_chart_5,get_cluster2_14_eth_chart(clustered_gp_and_metrics,5)),
#plot regional cluster
#tar_target(cluster_reg_map,get_cluster2_map(final_data_full_cats_percent_5_clusters_region,gp_geocoded)),
#tar_target(cluster_reg_chart,get_cluster2_chart(final_data_full_cats_percent_5_clusters_region)),

#process the data into the correct format and perform IoD calcs
tar_target(activity_by_type_clusters_stg1,process_metrics(clustered_gp_and_metrics)),
tar_target(activity_by_type_clusters_stg2,calc_iod_rate(activity_by_type_clusters_stg1)),
tar_target(activity_by_type_clusters_stg3,calc_iod_global_rate(activity_by_type_clusters_stg2)),
tar_target(activity_by_type_clusters_stg4,calc_iod_diff_rate(activity_by_type_clusters_stg3)),
tar_target(activity_by_type_clusters_stg5,calc_iod_diff(activity_by_type_clusters_stg4)),
tar_target(activity_by_type_clusters_stg6,calc_abs_iod(activity_by_type_clusters_stg5)),

#get metrics individually, ready to calculate IoD confidence intervals using a bootstrap method
tar_target(metric2_data,get_one_metric(activity_by_type_clusters_stg6,"metric2_","list_size_total")|>
             rename(numerator=metric2_num_total,denominator=list_size_total)),
tar_target(metric5_data,get_one_metric(activity_by_type_clusters_stg6,"metric5","list_size_total")|>
             rename(numerator=metric5_num_total,denominator=list_size_total)),
tar_target(metric6_data,get_one_metric(activity_by_type_clusters_stg6,"metric6","list_size_total")|>
             rename(numerator=metric6_total,denominator=list_size_total)),
tar_target(metric7_data,get_one_metric(activity_by_type_clusters_stg6,"metric7","list_size_total")|>
             rename(numerator=metric7_total,denominator=list_size_total)),
tar_target(metric8_data,get_one_metric(activity_by_type_clusters_stg6,"metric8","list_size_total")|>
             rename(numerator=metric8_total,denominator=list_size_total)),
tar_target(metric9_data,get_one_metric(activity_by_type_clusters_stg6,"metric9","list_size_total")|>
             rename(numerator=metric9_total,denominator=list_size_total)),
tar_target(metric11_data,get_one_metric(activity_by_type_clusters_stg6,"metric11","metric1_total")|>
             rename(numerator=metric11_num_total,denominator=metric1_total)),
tar_target(metric12_data,get_one_metric(activity_by_type_clusters_stg6,"metric12","metric1_total")|>
             rename(numerator=metric12_num_total,denominator=metric1_total)),
tar_target(metric13_data,get_one_metric(activity_by_type_clusters_stg6,"metric13","metric1_total")|>
             rename(numerator=metric13_num_total,denominator=metric1_total)),
tar_target(metric14_data,get_one_metric(activity_by_type_clusters_stg6,"metric14","metric1_total")|>
             rename(numerator=metric14_total,denominator=metric1_total)),
tar_target(metric15_data,get_one_metric(activity_by_type_clusters_stg6,"metric15","metric1_total")|>
             rename(numerator=metric15_total,denominator=metric1_total)),
tar_target(metric16_data,get_one_metric(activity_by_type_clusters_stg6,"metric16","metric1_total")|>
             rename(numerator=metric16_num_total,denominator=metric1_total)),
# tar_target(metric17_data,get_one_metric(activity_by_type_clusters_stg6,"metric17","metric1_total")|>
#              rename(numerator=metric17_num_total,denominator=metric1_total)),
tar_target(metric18_data,get_one_metric(activity_by_type_clusters_stg6,"metric18","metric1_total")|>
             rename(numerator=metric18_total,denominator=metric1_total)),
tar_target(metric19_data,get_one_metric(activity_by_type_clusters_stg6,"metric19","metric1_total")|>
             rename(numerator=metric19_total,denominator=metric1_total)),
tar_target(metric20_data,get_one_metric(activity_by_type_clusters_stg6,"metric20","metric1_total")|>
             rename(numerator=metric20_total,denominator=metric1_total)),
tar_target(metric21_data,get_one_metric(activity_by_type_clusters_stg6,"metric21","metric1_total")|>
             rename(numerator=metric21_total,denominator=metric1_total)),
tar_target(metric22_data,get_one_metric(activity_by_type_clusters_stg6,"metric22","metric1_total")|>
             rename(numerator=metric22_total,denominator=metric1_total)),
tar_target(metric23_data,get_one_metric(activity_by_type_clusters_stg6,"metric23","metric1_total")|>
             rename(numerator=metric23_total,denominator=metric1_total)),
#24
tar_target(metric25_data,get_one_metric(activity_by_type_clusters_stg6,"metric25","metric1_total")|>
             rename(numerator=metric25_num_total,denominator=metric1_total)),
tar_target(metric26_data,get_one_metric(activity_by_type_clusters_stg6,"metric26","metric1_total")|>
             rename(numerator=metric26_total,denominator=metric1_total)),
tar_target(metric27_data,get_one_metric(activity_by_type_clusters_stg6,"metric27","metric1_total")|>
             rename(numerator=metric27_total,denominator=metric1_total)),
tar_target(metric28_data,get_one_metric(activity_by_type_clusters_stg6,"metric28_","metric1_total")|>
             rename(numerator=metric28_total,denominator=metric1_total)),
tar_target(metric28b_data,get_one_metric(activity_by_type_clusters_stg6,"metric28b","metric1_total")|>
             rename(numerator=metric28b_total,denominator=metric1_total)),
tar_target(metric29_data,get_one_metric(activity_by_type_clusters_stg6,"metric29_","metric1_total")|>
             rename(numerator=metric29_total,denominator=metric1_total)),
tar_target(metric29b_data,get_one_metric(activity_by_type_clusters_stg6,"metric29b","metric1_total")|>
             rename(numerator=metric29b_total,denominator=metric1_total)),
tar_target(metric31_data,get_one_metric(activity_by_type_clusters_stg6,"metric31","metric1_total")|>
             rename(numerator=metric31_num_total,denominator=metric1_total)),
# tar_target(metric32_data,get_one_metric(activity_by_type_clusters_stg6,"metric32","metric1_total")|>
#              rename(numerator=metric32_num_total,denominator=metric1_total)),
tar_target(metric33_data,get_one_metric(activity_by_type_clusters_stg6,"metric33","list_size_45_total")|>
             rename(numerator=metric33_num_total,denominator=list_size_45_total)),
tar_target(metric34_data,get_one_metric(activity_by_type_clusters_stg6,"metric34","metric1_total")|>
             rename(numerator=metric34_num_total,denominator=metric1_total)),
tar_target(metric38_data,get_one_metric(activity_by_type_clusters_stg6,"metric38","metric1_total")|>
             rename(numerator=metric38_num_total,denominator=metric1_total)),
tar_target(metric39_data,get_one_metric(activity_by_type_clusters_stg6,"metric39","metric1_total")|>
             rename(numerator=metric39_num_total,denominator=metric1_total)),
tar_target(metric40_data,get_one_metric(activity_by_type_clusters_stg6,"metric40","metric1_total")|>
             rename(numerator=metric40_num_total,denominator=metric1_total)),
#get 1000 random sets of data per metric to bootstrap the conf ints
tar_target(metric2_rates_per_cluster,get_rates_per_cluster(metric2_data)),
tar_target(metric5_rates_per_cluster,get_rates_per_cluster(metric5_data)),
tar_target(metric6_rates_per_cluster,get_rates_per_cluster(metric6_data)),
tar_target(metric7_rates_per_cluster,get_rates_per_cluster(metric7_data)),
tar_target(metric8_rates_per_cluster,get_rates_per_cluster(metric8_data)),
tar_target(metric9_rates_per_cluster,get_rates_per_cluster(metric9_data)),
tar_target(metric11_rates_per_cluster,get_rates_per_cluster(metric11_data)),
tar_target(metric12_rates_per_cluster,get_rates_per_cluster(metric12_data)),
tar_target(metric13_rates_per_cluster,get_rates_per_cluster(metric13_data)),
tar_target(metric14_rates_per_cluster,get_rates_per_cluster(metric14_data)),
tar_target(metric15_rates_per_cluster,get_rates_per_cluster(metric15_data)),
tar_target(metric16_rates_per_cluster,get_rates_per_cluster(metric16_data)),
# tar_target(metric17_rates_per_cluster,get_rates_per_cluster(metric17_data)),
tar_target(metric18_rates_per_cluster,get_rates_per_cluster(metric18_data)),
tar_target(metric19_rates_per_cluster,get_rates_per_cluster(metric19_data)),
tar_target(metric20_rates_per_cluster,get_rates_per_cluster(metric20_data)),
tar_target(metric21_rates_per_cluster,get_rates_per_cluster(metric21_data)),
tar_target(metric22_rates_per_cluster,get_rates_per_cluster(metric22_data)),
tar_target(metric23_rates_per_cluster,get_rates_per_cluster(metric23_data)),
#24
tar_target(metric25_rates_per_cluster,get_rates_per_cluster(metric25_data)),
tar_target(metric26_rates_per_cluster,get_rates_per_cluster(metric26_data)),
tar_target(metric27_rates_per_cluster,get_rates_per_cluster(metric27_data)),
tar_target(metric28_rates_per_cluster,get_rates_per_cluster(metric28_data)),
tar_target(metric28b_rates_per_cluster,get_rates_per_cluster(metric28b_data)),
tar_target(metric29_rates_per_cluster,get_rates_per_cluster(metric29_data)),
tar_target(metric29b_rates_per_cluster,get_rates_per_cluster(metric29b_data)),
tar_target(metric31_rates_per_cluster,get_rates_per_cluster(metric31_data)),
# tar_target(metric32_rates_per_cluster,get_rates_per_cluster(metric32_data)),
tar_target(metric33_rates_per_cluster,get_rates_per_cluster(metric33_data)),
tar_target(metric34_rates_per_cluster,get_rates_per_cluster(metric34_data)),
tar_target(metric38_rates_per_cluster,get_rates_per_cluster(metric38_data)),
tar_target(metric39_rates_per_cluster,get_rates_per_cluster(metric39_data)),
tar_target(metric40_rates_per_cluster,get_rates_per_cluster(metric40_data)),
#join the iod and cis into one dataframe
tar_target(iod_with_ci_metric2,get_ci_iod_together(metric2_rates_per_cluster,"metric2",metric2_data)),
tar_target(iod_with_ci_metric5,get_ci_iod_together(metric5_rates_per_cluster,"metric5",metric5_data)),
tar_target(iod_with_ci_metric6,get_ci_iod_together(metric6_rates_per_cluster,"metric6",metric6_data)),
tar_target(iod_with_ci_metric7,get_ci_iod_together(metric7_rates_per_cluster,"metric7",metric7_data)),
tar_target(iod_with_ci_metric8,get_ci_iod_together(metric8_rates_per_cluster,"metric8",metric8_data)),
tar_target(iod_with_ci_metric9,get_ci_iod_together(metric9_rates_per_cluster,"metric9",metric9_data)),
tar_target(iod_with_ci_metric11,get_ci_iod_together(metric11_rates_per_cluster,"metric11",metric11_data)),
tar_target(iod_with_ci_metric12,get_ci_iod_together(metric12_rates_per_cluster,"metric12",metric12_data)),
tar_target(iod_with_ci_metric13,get_ci_iod_together(metric13_rates_per_cluster,"metric13",metric13_data)),
tar_target(iod_with_ci_metric14,get_ci_iod_together(metric14_rates_per_cluster,"metric14",metric14_data)),
tar_target(iod_with_ci_metric15,get_ci_iod_together(metric15_rates_per_cluster,"metric15",metric15_data)),
tar_target(iod_with_ci_metric16,get_ci_iod_together(metric16_rates_per_cluster,"metric16",metric16_data)),
# tar_target(iod_with_ci_metric17,get_ci_iod_together(metric17_rates_per_cluster,"metric17",metric17_data)),
tar_target(iod_with_ci_metric18,get_ci_iod_together(metric18_rates_per_cluster,"metric18",metric18_data)),
tar_target(iod_with_ci_metric19,get_ci_iod_together(metric19_rates_per_cluster,"metric19",metric19_data)),
tar_target(iod_with_ci_metric20,get_ci_iod_together(metric20_rates_per_cluster,"metric20",metric20_data)),
tar_target(iod_with_ci_metric21,get_ci_iod_together(metric21_rates_per_cluster,"metric21",metric21_data)),
tar_target(iod_with_ci_metric22,get_ci_iod_together(metric22_rates_per_cluster,"metric22",metric22_data)),
tar_target(iod_with_ci_metric23,get_ci_iod_together(metric23_rates_per_cluster,"metric23",metric23_data)),
#24
tar_target(iod_with_ci_metric25,get_ci_iod_together(metric25_rates_per_cluster,"metric25",metric25_data)),
tar_target(iod_with_ci_metric26,get_ci_iod_together(metric26_rates_per_cluster,"metric26",metric26_data)),
tar_target(iod_with_ci_metric27,get_ci_iod_together(metric27_rates_per_cluster,"metric27",metric27_data)),
tar_target(iod_with_ci_metric28,get_ci_iod_together(metric28_rates_per_cluster,"metric28",metric28_data)),
tar_target(iod_with_ci_metric28b,get_ci_iod_together(metric28b_rates_per_cluster,"metric28b",metric28b_data)),
tar_target(iod_with_ci_metric29,get_ci_iod_together(metric29_rates_per_cluster,"metric29",metric29_data)),
tar_target(iod_with_ci_metric29b,get_ci_iod_together(metric29b_rates_per_cluster,"metric29b",metric29b_data)),
tar_target(iod_with_ci_metric31,get_ci_iod_together(metric31_rates_per_cluster,"metric31",metric31_data)),
# tar_target(iod_with_ci_metric32,get_ci_iod_together(metric32_rates_per_cluster,"metric32",metric32_data)),
tar_target(iod_with_ci_metric33,get_ci_iod_together(metric33_rates_per_cluster,"metric33",metric33_data)),
tar_target(iod_with_ci_metric34,get_ci_iod_together(metric34_rates_per_cluster,"metric34",metric34_data)),
tar_target(iod_with_ci_metric38,get_ci_iod_together(metric38_rates_per_cluster,"metric38",metric38_data)),
tar_target(iod_with_ci_metric39,get_ci_iod_together(metric39_rates_per_cluster,"metric39",metric39_data)),
tar_target(iod_with_ci_metric40,get_ci_iod_together(metric40_rates_per_cluster,"metric40",metric40_data)),
tar_target(iod_with_ci,iod_with_ci_metric2|>bind_rows(iod_with_ci_metric5,iod_with_ci_metric6,iod_with_ci_metric7,iod_with_ci_metric8,
                                                      iod_with_ci_metric9,iod_with_ci_metric11,iod_with_ci_metric12,iod_with_ci_metric13,
                                                      iod_with_ci_metric14,iod_with_ci_metric15,iod_with_ci_metric16,
                                                     # iod_with_ci_metric17,
                                                      iod_with_ci_metric18,iod_with_ci_metric19,iod_with_ci_metric20,iod_with_ci_metric21,
                                                      iod_with_ci_metric22,iod_with_ci_metric23,iod_with_ci_metric25,iod_with_ci_metric26,
                                                      iod_with_ci_metric27,iod_with_ci_metric28,iod_with_ci_metric28b,iod_with_ci_metric29,
                                                      iod_with_ci_metric29b,iod_with_ci_metric31,
                                                     # iod_with_ci_metric32,
                                                      iod_with_ci_metric33,
                                                      iod_with_ci_metric34,iod_with_ci_metric38,iod_with_ci_metric39,iod_with_ci_metric40)),

#chart findings
tar_target(chart_iod_data,get_rel_iod_data_for_chart(activity_by_type_clusters_stg6)),
tar_target(rel_iod_chart,get_rel_iod_chart(chart_iod_data)),
tar_target(ci_iod_chart,get_ci_iod_chart(iod_with_ci)),

tar_target(rate_chart_data,get_rate_chart_data(activity_by_type_clusters_stg6)),
tar_target(rate_chart_risk_fact,get_rate_chart(rate_chart_data,"Risk factors")),
tar_target(rate_chart_risk_fact_ident,get_rate_chart(rate_chart_data,"Risk factor identification")),
tar_target(rate_chart_prim_prevent,get_rate_chart(rate_chart_data,"Primary prevention")),
tar_target(rate_chart_disease_ident,get_rate_chart(rate_chart_data,"Disease identification")),
tar_target(rate_chart_second_prevent,get_rate_chart(rate_chart_data,"Secondary prevention")),
tar_target(rate_chart_tert_prevent,get_rate_chart(rate_chart_data,"Tertiary prevention")),
tar_target(rate_chart_int_out,get_rate_chart(rate_chart_data,"Intermediate outcome")),
tar_target(rate_chart_full_out,get_rate_chart(rate_chart_data,"Full outcomes")),


#Regional versions
tar_target(region_clustered_gp_and_metrics,clustered_gp_and_metrics|>
              left_join(gp_icb_mapping, join_by(gp_practice_code==practice_code))),
tar_target(regional_charts,region_clustered_gp_and_metrics |>
             #group_split(comm_region_code)|>
             split(region_clustered_gp_and_metrics$comm_region_code)|>
             map(\(df) process_metrics(df) )|>
             map(\(df) calc_iod_rate(df) )|>
             map(\(df) calc_iod_global_rate(df) )|>
             map(\(df) calc_iod_diff_rate(df) )|>
             map(\(df) calc_iod_diff(df) )|>
             map(\(df) calc_abs_iod(df) )|>
             map(\(df) get_rel_iod_data_for_chart(df) )|>
             map(\(df) get_rel_iod_chart(df) )),
tar_target(region_cluster_chart_data,get_region_cluster_chart_data(region_clustered_gp_and_metrics)),
tar_target(region_cluster_charts,region_cluster_chart_data|>
             group_split(comm_region_name)|>
             map(\(df) get_region_cluster_chart(df) )),


#ICB versions
tar_target(icb_clustered_gp_and_metrics,clustered_gp_and_metrics|>
             left_join(gp_icb_mapping, join_by(gp_practice_code==practice_code))),
tar_target(icb_charts,icb_clustered_gp_and_metrics |>
             #group_split(icb_name)|>
             split(icb_clustered_gp_and_metrics$icb_name) |>
             map(\(df) process_metrics(df) )|>
             map(\(df) calc_iod_rate(df) )|>
             map(\(df) calc_iod_global_rate(df) )|>
             map(\(df) calc_iod_diff_rate(df) )|>
             map(\(df) calc_iod_diff(df) )|>
             map(\(df) calc_abs_iod(df) )|>
             map(\(df) get_rel_iod_data_for_chart(df) )|>
             map(\(df) get_rel_iod_chart(df) ))

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