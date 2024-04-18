library(readxl)
library(tidyr)
library(tidyverse)

#Get the IMD Deciles for each GP Practice

GPpatients_by_LSOA <- read_excel(here::here("data", "patients by LSOA to GP.xlsx"))
gp_reg_pat_prac_lsoa <-tar_read(gp_reg_pat_prac_lsoa)|>as_tibble()
clusters_for_nacr <- tar_read(clusters_for_nacr)|>as_tibble()

imd2019 <- read_excel(here::here("data", "IMD2019.xlsx"))


#practice_to_CCGSTP_lookup <- read_excel(here::here("data", "gp-reg-pat-prac-map-jul22.xlsx"))


#appends LSOA IMD score to GPpatients_by_LSOA
gp_reg_pat_prac_lsoa<- gp_reg_pat_prac_lsoa%>% 
  left_join(select(imd2019,`LSOA code (2011)`,`Index of Multiple Deprivation (IMD) Score`), 
            by = c("lsoa_code" = "LSOA code (2011)"))

#creates new field of LSOA IMDscore multiplied by the number of patients from that LSOA
gp_reg_pat_prac_lsoa<-gp_reg_pat_prac_lsoa|>
  mutate(scoretimespoplsoa = `Index of Multiple Deprivation (IMD) Score`*number_of_patients)

#groups data by Practice and sums number of patients and IMD score
patientweighted_practice_imd<-gp_reg_pat_prac_lsoa|>
  filter(!is.na(scoretimespoplsoa))|>
  group_by (practice_code)|>
  summarise(total_imd = sum(scoretimespoplsoa),patients = sum(number_of_patients)) |>
  arrange()|>
  
  #then calculates the average IMD (total IMD score/total Patients) 
  
  mutate(average_imd = total_imd/patients)|>
  #then calculates IMDDecile 
  mutate(gp_imd_decile = ntile(-average_imd, 10))
  #adds in organisational data
  # left_join(select(practice_to_CCGSTP_lookup,`CCG_CODE`,`CCG_NAME`,`STP_CODE`,`STP_NAME`,`COMM_REGION_NAME`,`ICB22ons`,`ICB22`,`ICB22NM`,`New Sub-ICB location name`,`Reg22NM`,`PRACTICE_CODE`), 
  #           by = c("PRACTICE_CODE" = "PRACTICE_CODE"))

patientweighted_practice_imd <-patientweighted_practice_imd |>
  rename(gp_practice_code=practice_code)|>
  left_join(clusters_for_nacr)
