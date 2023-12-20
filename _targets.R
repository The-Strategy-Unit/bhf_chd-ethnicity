# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble","fingertipsR","readxl","tidyverse","utils","janitor","readr","visNetwork","odbc","stringr","MLID"), # packages that your targets need to run
  format = "rds"
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")


# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(data_path1, "data/metric14.csv", format = "file"), # SUS data metric 14
  tar_target(metric14,read_csv_file(data_path1)),
  tar_target(data_path2, "data/metric15.csv", format = "file"), # SUS data metric 15
  tar_target(metric15,read_csv_file(data_path2)),
  tar_target(data_path3, "data/metric18.csv", format = "file"), # SUS data metric 18
  tar_target(metric18,read_csv_file(data_path3)),
  tar_target(data_path4, "data/metric19.csv", format = "file"), # SUS data metric 19
  tar_target(metric19,read_csv_file(data_path4)),
  tar_target(data_path5, "data/metric20.csv", format = "file"), # SUS data metric 20
  tar_target(metric20,read_csv_file(data_path5)),
  tar_target(data_path6, "data/metric21.csv", format = "file"), # SUS data metric 21
  tar_target(metric21,read_csv_file(data_path6)),
  tar_target(data_path7, "data/metric22.csv", format = "file"), # SUS data metric 22
  tar_target(metric22,read_csv_file(data_path7)),
  tar_target(data_path8, "data/metric23.csv", format = "file"), # SUS data metric 23
  tar_target(metric23,read_csv_file(data_path8)),
  tar_target(data_path9, "data/metric27.csv", format = "file"), # SUS data metric 27
  tar_target(metric27,read_csv_file(data_path9)),
  tar_target(data_path10, "data/metric28.csv", format = "file"), # SUS data metric 28
  tar_target(metric28,read_csv_file(data_path10)),
  tar_target(data_path11, "data/metric29.csv", format = "file"), # SUS data metric 29
  tar_target(metric29,read_csv_file(data_path11)),
  # tar_target(data_path12, "data/April22GPListLSOA.csv", format = "file"), #GP list size
  # tar_target(gp_list_lsoa,read_csv_file(data_path12)),
  tar_target(data_path13, "data/EthnicityByLSOACensus21.csv", format = "file"), #Ethnicity by LSOA Census 2021
  tar_target(eth_lsoa_census,read_csv_file(data_path13)),
  tar_target(data_path14, "data/gp-reg-pat-prac-lsoa-all.csv", format = "file"), #GP Reg Pat Prac LSOA April 2023
  tar_target(gp_reg_pat_prac_lsoa,read_csv_file(data_path14)),
  tar_target(data_path15, "data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv", format = "file"), #lookup
  tar_target(lsoa_lookup,read_csv_file(data_path15)),
  tar_target(data_path17,"data/gp-reg-pat-prac-map.csv", format = "file"),
  tar_target(gp_icb_mapping,read_csv_file(data_path17)|> 
               select(practice_code,practice_name,sub_icb_location_code, sub_icb_location_name,
                      icb_code,icb_name,comm_region_code,comm_region_name)),  
  tar_target(data_path18,"data/CVDP003.csv", format = "file"),  
  tar_target(cvdp003,read_csv_file(data_path18)),
  tar_target(data_path19,"data/CVDP006.csv", format = "file"), 
  tar_target(cvdp006,read_csv_file(data_path19)),  
  tar_target(data_path20,"data/CVDP007.csv", format = "file"), 
  tar_target(cvdp007,read_csv_file(data_path20)),  
  tar_target(data_path21,"data/CVDP008.csv", format = "file"), 
  tar_target(cvdp008,read_csv_file(data_path21)), 
  tar_target(data_path22,"data/CVDP009.csv", format = "file"), 
  tar_target(cvdp009,read_csv_file(data_path22)), 
  tar_target(data_path23,"data/NCDesMarch23.csv", format = "file"), 
  tar_target(ncdes_data,read_csv_file(data_path23)|>
               filter(ind_code %in% c("NCD002","NCD003"),
                      measure %in% c("Denominator", "Numerator")) |>
               pivot_wider(names_from=c(ind_code,measure),
                           names_sep="_",
                           values_from = value)), 
  tar_target(data_path24,"data/20231201_92847.xlsx", format="file"), #CHD synthetic prevalence metric 1
  tar_target(metric1,read_excel_file(data_path24)), 
  tar_target(data_path25,"data/epraccur_gp.csv", format="file"), #All practices
  tar_target(gp_history,read_csv_file(data_path25)), 
  tar_target(metric13,get_my_fingertips_gp_data(273,"2021/22")), # metric 13
  tar_target(metric6,get_my_fingertips_gp_data(93088,"2021/22")), # metric 6
  tar_target(metric7,get_my_fingertips_gp_data(241,"2021/22")), # metric 7
  tar_target(metric8,get_my_fingertips_gp_data(848,"2021/22")), # metric 8
  tar_target(metric16,get_my_fingertips_gp_data(90999,"2021/22")), # metric 16
  tar_target(metric17,get_my_fingertips_gp_data(91000,"2020/21")), # metric 17
  tar_target(metric9,get_my_fingertips_gp_data(92589,"2019/20")), # metric 9
  tar_target(metric10,get_my_fingertips_gp_data(91248,"2019/20")), # metric 10
  tar_target(metric11,get_my_fingertips_gp_data(90619,"2021/22")), # metric 11
  tar_target(data_path16, "data/QOF_CHD_2022_23.xlsx", format = "file"), #QOF CHD indicators
  tar_target(qof_chd_2223,read_qof_excel_file(data_path16) |>
               select(1,2,3,6,7,15,32,35,37,38,46)|>
               clean_names() |>
               filter(practice_code != "NA")),
  tar_target(metric16b,
            qof_chd_2223 |> 
            select(4,10) |>
            rename(value=patients_receiving_intervention_percent_38)), # metric 16b 22/23
  tar_target(metric31,
            qof_chd_2223 |> 
            select(4,8) |>
            rename(value=pc_as_35)), # metric 31 22/23
  tar_target(metric25b,
             qof_chd_2223 |> 
               select(4,11) |>
               rename(value=patients_receiving_intervention_percent_46)),   # metric 25b 22/23
  tar_target(metric32,get_data_via_server()), # metric 32 20/21
  tar_target(metric13b,
             qof_chd_2223 |> 
               select(4,6) |>
               rename(value=prevalence_percent_15)), # metric 13b 22/23
  tar_target(metric39,
             ncdes_data|>
               mutate(ncd002_percent =(NCD002_Numerator / NCD002_Denominator)*100) |>
               select(practice_code,ncd002_percent)|>
               rename(value=ncd002_percent)), # metric 39 march 23
  tar_target(metric40,
             ncdes_data|>
               mutate(ncd003_percent =(NCD003_Numerator / NCD003_Denominator)*100) |>
               select(practice_code,ncd003_percent)|>
               rename(value=ncd003_percent)), # metric 40 march 23
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
  tar_target(gp_lsoa_with_eth_sum,sum_ethnicities(gp_lsoa_with_eth))
  )
  





#-----------------------------------------------------------------------------
#Useful commands:
#tar_make()
#tar_read(<name of target>) e.g. tar_read(LSOAPopMales)
#tar_read(<name of target>) |> as_tibble()
#tar_visnetwork()
#tar_meta()
#-----------------------------------------------------------------------------