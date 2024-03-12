#process metrics ready to calc
library(tidyverse)
library(fingertipsR)
library(readxl)

clustered_gp_and_metrics <- targets::tar_read(clustered_gp_and_metrics) |> dplyr::as_tibble()
activity_by_type_clusters_stg1<- targets::tar_read(activity_by_type_clusters_stg1) |> dplyr::as_tibble()
activity_by_type_clusters_stg2<- targets::tar_read(activity_by_type_clusters_stg2) |> dplyr::as_tibble()
activity_by_type_clusters_stg3<- targets::tar_read(activity_by_type_clusters_stg3) |> dplyr::as_tibble()
activity_by_type_clusters_stg4<- targets::tar_read(activity_by_type_clusters_stg4) |> dplyr::as_tibble()
activity_by_type_clusters_stg5<- targets::tar_read(activity_by_type_clusters_stg5) |> dplyr::as_tibble()
activity_by_type_clusters_stg6<- targets::tar_read(activity_by_type_clusters_stg6) |> dplyr::as_tibble()
ncdes_data <- tar_read(ncdes_data) |> as_tibble()
gp_over45_perc <- tar_read(gp_over45_perc) |> as_tibble()
gp_reg_pat_prac_sing_age_male <- tar_read(gp_reg_pat_prac_sing_age_male) |> as_tibble()
gp_reg_pat_prac_sing_age_female <- tar_read(gp_reg_pat_prac_sing_age_female) |> as_tibble()



activity_by_type_clusters_stg7 <- activity_by_type_clusters_stg6 |>
  select(cluster2,list_size_total,ends_with("rel_iod"))|>
         #metric28_rel_iod,metric29_rel_iod,metric31_rel_iod,metric32_rel_iod,metric33_rel_iod)|>
  pivot_longer(cols=starts_with("metric"),
               names_to="metric")|>
  filter(cluster2==1)|>
  select(-cluster2,-list_size_total)


activity_by_type_clusters_stg7 |>
  mutate(metric=fct_rev(factor(metric,levels=c("metric2_rel_iod","metric5_rel_iod","metric6_rel_iod","metric7_rel_iod","metric8_rel_iod",
                                               "metric33_rel_iod","metric9_rel_iod","metric34_rel_iod","metric38_rel_iod","metric11_rel_iod",
                                               "metric12_rel_iod","metric13_rel_iod","metric14_rel_iod", "metric15_rel_iod","metric16_rel_iod",
                                               "metric31_rel_iod", "metric17_rel_iod","metric32_rel_iod", "metric39_rel_iod",
                                               "metric40_rel_iod","metric18_rel_iod", "metric19_rel_iod","metric20_rel_iod","metric21_rel_iod",
                                               "metric22_rel_iod","metric23_rel_iod", "metric25_rel_iod","metric27_rel_iod",
                                               "metric28_rel_iod","metric28b_rel_iod", "metric29_rel_iod","metric29b_rel_iod")))) |>
  mutate(metric_name=case_when(metric=="metric2_rel_iod"~"Smoking prev est",
                               metric=="metric5_rel_iod"~"Smoking register",
                               metric=="metric6_rel_iod"~"Obesity register",
                               metric=="metric7_rel_iod"~"Diabetes register",
                               metric=="metric8_rel_iod"~"Depression register",
                               metric=="metric33_rel_iod"~"45+ BP check < 5 years",
                               metric=="metric9_rel_iod"~"CVD risk register",
                               metric=="metric34_rel_iod"~"CVD pat treated with LLT",
                               metric=="metric38_rel_iod"~"Pat at risk treated with LLT",
                               metric=="metric11_rel_iod"~"Smoking cessation support offered",
                               metric=="metric12_rel_iod"~"Except rep. smoke cease sup",
                               metric=="metric13_rel_iod"~"CHD register",
                               metric=="metric14_rel_iod"~"CT angiography",
                               metric=="metric15_rel_iod"~"Electrocardiography",
                               metric=="metric16_rel_iod"~"Asprin anti-platelet etc",
                               metric=="metric31_rel_iod"~"Except rep. asprin etc",
                               metric=="metric17_rel_iod"~"Flu vaccination",
                               metric=="metric32_rel_iod"~"Except rep. flu vacc",
                               metric=="metric39_rel_iod"~"65+ flu vaccination",
                               metric=="metric40_rel_iod"~"<65 at risk flu vaccination",
                               metric=="metric18_rel_iod"~"Ref to OP cariology (new)",
                               metric=="metric19_rel_iod"~"Cardiology outpatient DNAs",
                               metric=="metric20_rel_iod"~"Elective PCI",
                               metric=="metric21_rel_iod"~"Elective CABG",
                               metric=="metric22_rel_iod"~"Waiting time for elective PCI / CABG",
                               metric=="metric23_rel_iod"~"PCI/CABG disch < trimpoint",
                               metric=="metric25_rel_iod"~"BP reading < 140/90 (<80 yrs CHD)",
                               metric=="metric27_rel_iod"~"Emergency admissions for CHD",
                               metric=="metric28_rel_iod"~"CHD Hospital Deaths",
                               metric=="metric28b_rel_iod"~"CHD Hospital Deaths <75",
                               metric=="metric29_rel_iod"~"CHD Deaths",
                               metric=="metric29b_rel_iod"~"CHD Deaths <75",
                               .default = metric))|>
ggplot(aes(x=metric, y=value)) +
  geom_segment( aes(x=metric, xend=metric, y=0, yend=value), color="grey") +
  geom_point( color="orange", size=3) +
  coord_flip()+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Relative Index of Disparity (%)")+
  labs(title = paste("Index of Disparity along CHD pathway"))

palette <- brewer.pal(5, "Set1")
activity_by_type_clusters_stg8 <- activity_by_type_clusters_stg6 |>
  select(cluster2,ends_with("_rate"))|>
  select(-ends_with("global_rate"),-ends_with("diff_rate"))|>

  pivot_longer(cols=starts_with("metric"),
               names_to="metric") |>
  mutate(pathway=case_when(metric=="metric2_rate"~"Risk factors",
                           metric=="metric5_rate"|metric=="metric6_rate"|metric=="metric7_rate"|metric=="metric8_rate"
                           |metric=="metric33_rate"~"Risk factor identification",
                           metric=="metric9_rate"|metric=="metric34_rate"|metric=="metric38_rate"|metric=="metric11_rate"|
                             metric=="metric12_rate"~"Primary prevention",
                           metric=="metric13_rate"|metric=="metric14_rate"|metric=="metric15_rate"~"Disease identification",
                           metric=="metric16_rate"|metric=="metric31_rate"|metric=="metric17_rate"|metric=="metric32_rate"|
                             metric=="metric39_rate"|metric=="metric40_rate"|metric=="metric18_rate"|
                             metric=="metric19_rate"~"Secondary prevention",
                           metric=="metric20_rate"|metric=="metric21_rate"|metric=="metric22_rate"|
                             metric=="metric23_rate"~"Tertiary prevention",
                           metric=="metric25_rate"|metric=="metric27_rate"~"Intermediate outcome",
                           metric=="metric28_rate"|metric=="metric28b_rate"|metric=="metric29_rate"|
                             metric=="metric29b_rate"~"Full outcomes",
                           .default="other"))|>
  mutate(metric_name=case_when(metric=="metric2_rate"~"Smoking prev est",
                               metric=="metric5_rate"~"Smoking register",
                               metric=="metric6_rate"~"Obesity register",
                               metric=="metric7_rate"~"Diabetes register",
                               metric=="metric8_rate"~"Depression register",
                               metric=="metric33_rate"~"45+ BP check < 5 years",
                               metric=="metric9_rate"~"CVD risk register",
                               metric=="metric34_rate"~"CVD pat treated with LLT",
                               metric=="metric38_rate"~"Pat at risk treated with LLT",
                               metric=="metric11_rate"~"Smoking cessation support offered",
                               metric=="metric12_rate"~"Except rep. smoke cease sup",
                               metric=="metric13_rate"~"CHD register",
                               metric=="metric14_rate"~"CT angiography",
                               metric=="metric15_rate"~"Electrocardiography",
                               metric=="metric16_rate"~"Asprin anti-platelet etc",
                               metric=="metric31_rate"~"Except rep. asprin etc",
                               metric=="metric17_rate"~"Flu vaccination",
                               metric=="metric32_rate"~"Except rep. flu vacc",
                               metric=="metric39_rate"~"65+ flu vaccination",
                               metric=="metric40_rate"~"<65 at risk flu vaccination",
                               metric=="metric18_rate"~"Ref to OP cariology (new)",
                               metric=="metric19_rate"~"Cardiology outpatient DNAs",
                               metric=="metric20_rate"~"Elective PCI",
                               metric=="metric21_rate"~"Elective CABG",
                               metric=="metric22_rate"~"Waiting time for elective PCI / CABG",
                               metric=="metric23_rate"~"PCI/CABG disch < trimpoint",
                               metric=="metric25_rate"~"BP reading < 140/90 (<80 yrs CHD)",
                               metric=="metric27_rate"~"Emergency admissions for CHD",
                               metric=="metric28_rate"~"CHD Hospital Deaths",
                               metric=="metric28b_rate"~"CHD Hospital Deaths <75",
                               metric=="metric29_rate"~"CHD Deaths",
                               metric=="metric29b_rate"~"CHD Deaths <75",
                               .default = metric))|>
  filter(pathway=="Primary prevention")|>
  ggplot(aes(x=as.factor(cluster2),y=value, fill=as.factor(cluster2) )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette)+
  theme(legend.position="none")+
  xlab("Cluster (1=Least diverse, 5=Most diverse)") +
  ylab("Rate")+
  facet_wrap(vars(metric_name),ncol=3,scales="free_y")



#metric9 - ok - DQ with practice F85002?

#metric 13 - keep 13b and remove 13
# should be a % then as a prop of list size

# check the metric 13b columns