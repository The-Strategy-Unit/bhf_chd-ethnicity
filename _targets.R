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
  tar_target(data_path12, "data/April22GPListLSOA.csv", format = "file"), #GP list size
  tar_target(gp_list_lsoa,read_csv_file(data_path12)),
  tar_target(data_path13, "data/EthnicityByLSOACensus21.csv", format = "file"), #Ethnicity by LSOA Census 2021
  tar_target(eth_lsoa_census,read_csv_file(data_path13)),
  tar_target(data_path14, "data/gp-reg-pat-prac-lsoa-all.csv", format = "file"), #GP Reg Pat Prac LSOA
  tar_target(gp_reg_pat_prac_lsoa,read_csv_file(data_path14)),
  tar_target(data_path15, "data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales.csv", format = "file"), #lookup
  tar_target(lsoa_lookup,read_csv_file(data_path15)),
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
  #process ethnicity data
  tar_target(lsoa_eth_sum,process_census21data(eth_lsoa_census)),
  tar_target(gp_lsoa,process_gpdata(gp_reg_pat_prac_lsoa)),
  tar_target(joined,join_gp_and_eth(lsoa_eth_sum,gp_lsoa))
  )
  


#-----------------------------------------------------------------------------
#Useful commands:
#tar_make()
#tar_read(<name of target>) e.g. tar_read(LSOAPopMales)
#tar_read(<name of target>) |> as_tibble()
#tar_visnetwork()
#tar_meta()
#-----------------------------------------------------------------------------