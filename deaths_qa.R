clustered_gp_and_metrics <- tar_read(clustered_gp_and_metrics)|>as_tibble()
activity_by_type_clusters_stg6 <- tar_read(activity_by_type_clusters_stg6)|>as_tibble()
gp_reg_pat_prac_sing_age_female<- tar_read(gp_reg_pat_prac_sing_age_female)|>as_tibble()
gp_reg_pat_prac_sing_age_male<- tar_read(gp_reg_pat_prac_sing_age_male)|>as_tibble()
gp_reg_pat_prac_lsoa <- tar_read(gp_reg_pat_prac_lsoa)|>as_tibble()

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
