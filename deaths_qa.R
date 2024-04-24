library(targets)
library(janitor)

clustered_gp_and_metrics <- tar_read(clustered_gp_and_metrics)|>as_tibble()
activity_by_type_clusters_stg6 <- tar_read(activity_by_type_clusters_stg6)|>as_tibble()
gp_reg_pat_prac_sing_age_female<- tar_read(gp_reg_pat_prac_sing_age_female)|>as_tibble()
gp_reg_pat_prac_sing_age_male<- tar_read(gp_reg_pat_prac_sing_age_male)|>as_tibble()
gp_reg_pat_prac_lsoa <- tar_read(gp_reg_pat_prac_lsoa)|>as_tibble()
clusters_for_nacr <- tar_read(clusters_for_nacr)|> as_tibble()

need <- activity_by_type_clusters_stg6|>select(cluster2,metric1_total)|>rename(chdprev_need=metric1_total)

gp_75over_perc <- gp_reg_pat_prac_sing_age_male |>
  rbind(gp_reg_pat_prac_sing_age_female)|>
  filter(age != "ALL") |>
  group_by(org_code,age) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                         .default = as.numeric(age))
  )) |>
  mutate(over_75=case_when(age>= 75 ~1,
                           .default = 0)) |>
  group_by(org_code,over_75)|>
  summarise(number_of_patients=sum(number_of_patients))|>
  group_by(org_code) |>
  mutate(total_patients=sum(number_of_patients))|>
  ungroup() |>
  mutate(perc=(number_of_patients/total_patients)*100)|>
  filter(over_75==1) |>
  select(org_code,perc)|>
  rename(gp_practice_code=org_code,perc_over75=perc)

gp_16over_female_num <- gp_reg_pat_prac_sing_age_female |>
  filter(age != "ALL") |>
  group_by(org_code,age) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                         .default = as.numeric(age))
  )) |>
  mutate(over_16=case_when(age>= 16 ~1,
                           .default = 0)) |>
  group_by(org_code,over_16)|>
  summarise(number_of_patients=sum(number_of_patients))|>
  filter(over_16==1)|>
  select(-over_16)|>
  rename(gp_practice_code=org_code,num_females=number_of_patients)


death_data <- clustered_gp_and_metrics |>
  select(gp_practice_code,cluster2,starts_with("gp_perc"),list_size,metric28,metric28b,metric29,metric29b)|>
  mutate(metric28=replace_na(metric28,0),
         metric28b=replace_na(metric28b,0),
         metric29=replace_na(metric29,0),
         metric29b=replace_na(metric29b,0))|>
  mutate(eth_all_white=gp_perc_est_white_british+gp_perc_est_white_irish+gp_perc_est_white_other,
         eth_all_asian=gp_perc_est_bangladeshi+gp_perc_est_chinese+gp_perc_est_indian+gp_perc_est_pakistani+gp_perc_est_other_asian,
         eth_all_black=gp_perc_est_blk_african+gp_perc_est_blk_caribbean+gp_perc_est_other_blk,
         eth_all_mixed=gp_perc_est_all_mixed,
         eth_all_other=gp_perc_est_other_arab+gp_perc_est_other_other
         )|>
  select(-starts_with("gp_perc"))|>
  mutate(eth_all=eth_all_white+eth_all_asian+eth_all_black+eth_all_mixed+eth_all_other)|>
  mutate(white=eth_all_white/100*list_size,
         asian=eth_all_asian/100*list_size,
         black=eth_all_black/100*list_size,
         mixed=eth_all_mixed/100*list_size,
         other=eth_all_other/100*list_size)|>
  left_join(gp_75over_perc)|>
  mutate(gp_75_over=perc_over75/100*list_size)|>
  left_join(gp_16over_female_num)|>
  group_by(cluster2)|>
  summarise(metric28=sum(metric28),
            metric28b=sum(metric28b),
            metric29=sum(metric29),
            metric29b=sum(metric29b),
            list_size=sum(list_size),
            white=sum(white),
            asian=sum(asian),
            black=sum(black),
            mixed=sum(mixed),
            other=sum(other),
            gp_75_over=sum(gp_75_over),
            females=sum(num_females)
            )|>
  mutate(white_perc=white/list_size,
         asian_perc=asian/list_size,
         black_perc=black/list_size,
         mixed_perc=mixed/list_size,
         other_perc=other/list_size,
         list75over=gp_75_over/list_size,
         females=females/list_size)|>
  select(-white,-asian,-black,-mixed,-other)|>
  left_join(need)|>
  mutate(chd_prev_perc=chdprev_need/list_size)


age_sex_gp_reg_pat <-gp_reg_pat_prac_sing_age_female|>
  rbind(gp_reg_pat_prac_sing_age_male)


all_cause_deaths_2223 <- read_csv_file("data/all_cause_deaths_2223.csv")

all_cause_deaths_2223 |>
  group_by(gpnull)|>
  summarise(deaths=sum(num_deaths))

all_cause_deaths_2223 |>
  filter(age_at_death!="NA")|>
  group_by(gpnull)|>
  summarise(deaths=sum(num_deaths))

all_cause_deaths_2223_data <- all_cause_deaths_2223|>
  filter(age_at_death!="NA")|>
  #filter(chd==1)|>
  mutate(age_group=case_when(age_at_death<1~'<1',
                             age_at_death>=1 & age_at_death <5 ~'1-4',
                             age_at_death>=5 & age_at_death <10 ~'5-9',
                             age_at_death>=10 & age_at_death <15 ~'10-14',
                             age_at_death>=15 & age_at_death <20 ~'15-19',
                             age_at_death>=20 & age_at_death <25 ~'20-24',
                             age_at_death>=25 & age_at_death <30 ~'25-29',
                             age_at_death>=30 & age_at_death <35 ~'30-34',
                             age_at_death>=35 & age_at_death <40 ~'35-39',
                             age_at_death>=40 & age_at_death <45 ~'40-44',
                             age_at_death>=45 & age_at_death <50 ~'45-49',
                             age_at_death>=50 & age_at_death <55 ~'50-54',
                             age_at_death>=55 & age_at_death <60 ~'55-59',
                             age_at_death>=60 & age_at_death <65 ~'60-64',
                             age_at_death>=65 & age_at_death <70 ~'65-69',
                             age_at_death>=70 & age_at_death <75 ~'70-74',
                             age_at_death>=75 & age_at_death <80 ~'75-79',
                             age_at_death>=80 & age_at_death <85 ~'80-84',
                             age_at_death>=85 & age_at_death <90 ~'85-89',
                             age_at_death>=90  ~'90+'
                             
  ))|>
  select(-age_at_death)|>
  group_by(sex,age_group)|>
  summarise(deaths=sum(num_deaths))

all_cause_deaths_2223_gp_prac <- all_cause_deaths_2223|>
  filter(age_at_death!="NA")|>
  filter(gp_practice_code!="NULL")|>
  filter(chd==1)|>
  mutate(age_group=case_when(age_at_death<1~'<1',
                             age_at_death>=1 & age_at_death <5 ~'1-4',
                             age_at_death>=5 & age_at_death <10 ~'5-9',
                             age_at_death>=10 & age_at_death <15 ~'10-14',
                             age_at_death>=15 & age_at_death <20 ~'15-19',
                             age_at_death>=20 & age_at_death <25 ~'20-24',
                             age_at_death>=25 & age_at_death <30 ~'25-29',
                             age_at_death>=30 & age_at_death <35 ~'30-34',
                             age_at_death>=35 & age_at_death <40 ~'35-39',
                             age_at_death>=40 & age_at_death <45 ~'40-44',
                             age_at_death>=45 & age_at_death <50 ~'45-49',
                             age_at_death>=50 & age_at_death <55 ~'50-54',
                             age_at_death>=55 & age_at_death <60 ~'55-59',
                             age_at_death>=60 & age_at_death <65 ~'60-64',
                             age_at_death>=65 & age_at_death <70 ~'65-69',
                             age_at_death>=70 & age_at_death <75 ~'70-74',
                             age_at_death>=75 & age_at_death <80 ~'75-79',
                             age_at_death>=80 & age_at_death <85 ~'80-84',
                             age_at_death>=85 & age_at_death <90 ~'85-89',
                             age_at_death>=90  ~'90+'
                             
  ))|>
  select(-age_at_death)|>
  group_by(sex,age_group,gp_practice_code)|>
  summarise(deaths=sum(num_deaths),
            chddeaths=sum(chd))|>
  ungroup()|>
  left_join(clusters_for_nacr|>select(gp_practice_code,cluster))|>
  filter(is.na(cluster)==FALSE)|>
  group_by(sex,age_group,cluster)|>
  summarise(deaths=sum(deaths))

all_cause_deaths_2223|>filter(gp_practice_code!="NULL")|>
  group_by(gp_practice_code)|>summarise(deaths=sum(num_deaths))

#Cluster population data from gp lists

females_gp_reg_pat <- gp_reg_pat_prac_sing_age_female |>
  filter(age != "ALL") |>
  mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                  .default = as.numeric(age))))|>
  mutate(age_group=case_when(age<1~'<1',
                             age>=1 & age <5 ~'1-4',
                             age>=5 & age <10 ~'5-9',
                             age>=10 & age <15 ~'10-14',
                             age>=15 & age <20 ~'15-19',
                             age>=20 & age <25 ~'20-24',
                             age>=25 & age <30 ~'25-29',
                             age>=30 & age <35 ~'30-34',
                             age>=35 & age <40 ~'35-39',
                             age>=40 & age <45 ~'40-44',
                             age>=45 & age <50 ~'45-49',
                             age>=50 & age <55 ~'50-54',
                             age>=55 & age <60 ~'55-59',
                             age>=60 & age <65 ~'60-64',
                             age>=65 & age <70 ~'65-69',
                             age>=70 & age <75 ~'70-74',
                             age>=75 & age <80 ~'75-79',
                             age>=80 & age <85 ~'80-84',
                             age>=85 & age <90 ~'85-89',
                             age>=90  ~'90+'
  ))|>
  select(-age)|>
  mutate(sex=2)

males_gp_reg_pat <- gp_reg_pat_prac_sing_age_male |>
  filter(age != "ALL") |>
  mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                  .default = as.numeric(age))))|>
  mutate(age_group=case_when(age<1~'<1',
                             age>=1 & age <5 ~'1-4',
                             age>=5 & age <10 ~'5-9',
                             age>=10 & age <15 ~'10-14',
                             age>=15 & age <20 ~'15-19',
                             age>=20 & age <25 ~'20-24',
                             age>=25 & age <30 ~'25-29',
                             age>=30 & age <35 ~'30-34',
                             age>=35 & age <40 ~'35-39',
                             age>=40 & age <45 ~'40-44',
                             age>=45 & age <50 ~'45-49',
                             age>=50 & age <55 ~'50-54',
                             age>=55 & age <60 ~'55-59',
                             age>=60 & age <65 ~'60-64',
                             age>=65 & age <70 ~'65-69',
                             age>=70 & age <75 ~'70-74',
                             age>=75 & age <80 ~'75-79',
                             age>=80 & age <85 ~'80-84',
                             age>=85 & age <90 ~'85-89',
                             age>=90  ~'90+'
  ))|>
  select(-age)|>
  mutate(sex=1)

all_gp_reg_pat <- males_gp_reg_pat|>
  rbind(females_gp_reg_pat)|>
  rename(gp_practice_code=org_code)|>
  group_by(sex,age_group,gp_practice_code)|>
  summarise(number_of_patients=sum(number_of_patients))|>
  ungroup()|>
  left_join(clusters_for_nacr|>select(gp_practice_code,cluster))|>
  group_by(sex,age_group,cluster)|>
  summarise(number_of_patients=sum(number_of_patients))


#Describe the clusters


females_gp_reg_pat <- gp_reg_pat_prac_sing_age_female |>
  filter(age != "ALL") |>
  mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                  .default = as.numeric(age))))|>
  mutate(age_group=case_when(age<18~'<18',
                             age>=18 & age <45 ~'18-44',
                             age>=45 & age <65 ~'45-64',
                             age>=65 & age <75 ~'65-74',
                             age>=75  ~'75+'
  ))|>
  select(-age)|>
  mutate(sex=2)

males_gp_reg_pat <- gp_reg_pat_prac_sing_age_male |>
  filter(age != "ALL") |>
  mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                  .default = as.numeric(age))))|>
  mutate(age_group=case_when(age<18~'<18',
                             age>=18 & age <45 ~'18-44',
                             age>=45 & age <65 ~'45-64',
                             age>=65 & age <75 ~'65-74',
                             age>=75  ~'75+'
  ))|>
  select(-age)|>
  mutate(sex=1)

library(gt)

all_gp_reg_pat <- males_gp_reg_pat|>
  rbind(females_gp_reg_pat)|>
  rename(gp_practice_code=org_code)|>
  group_by(sex,age_group,gp_practice_code)|>
  summarise(number_of_patients=sum(number_of_patients))|>
  ungroup()|>
  left_join(clusters_for_nacr|>select(gp_practice_code,cluster))|>
  group_by(sex,age_group,cluster)|>
  summarise(number_of_patients=sum(number_of_patients))|>
  ungroup()|>
  mutate(sex_name=case_when(sex==1~'Male',
                            sex==2~'Female'))|>
  select(-sex)|>
  group_by(cluster)|>
  mutate(patients_total_cluster=sum(number_of_patients))|>
  mutate(perc=number_of_patients/patients_total_cluster)|>
  select(-number_of_patients,-patients_total_cluster)
  
  all_gp_reg_pat|>
  gt(rowname_col = "age_group")|>
  tab_stubhead(label=md("**Age Group**")) |>
  tab_spanner(label = "Cluster", columns = matches("cluster")) |>
  cols_label(
    perc = md("**Percentage**"),
    sex_name = md("**Sex**"))|>
  fmt_number(columns = "perc", decimals = 2)


  
  all_gp_reg_pat |>
    pivot_wider(names_from=c(cluster,sex_name),
                names_sep = "_",
                values_from = perc
                )|>
    select(age_group, starts_with("1"), starts_with("2"), starts_with("3"),starts_with("4"),starts_with("5")) |>
    gt(rowname_col = "age_group")|>
    tab_spanner_delim(delim = "_") |>
    tab_stubhead(label=md("**Age Group**"))|>
    tab_options(heading.title.font.size = 18,
                heading.title.font.weight = "bolder",
                column_labels.font.weight = "bold")|>
    fmt_percent(decimals = 2)|>
    tab_header(title = "Age / Sex Breakdown by Cluster")
  
  