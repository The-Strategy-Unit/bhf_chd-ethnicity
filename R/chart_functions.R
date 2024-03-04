
get_rel_iod_data_for_chart <- function(activity_by_type_clusters_stg6){
  chart_iod_data <- activity_by_type_clusters_stg6 |>
  select(cluster2,list_size_total,ends_with("rel_iod"))|>
  pivot_longer(cols=starts_with("metric"),
               names_to="metric")|>
  filter(cluster2==1)|>
  select(-cluster2,-list_size_total)
return(chart_iod_data)
}


get_rel_iod_chart <- function(chart_iod_data){

  rel_iod_chart <- chart_iod_data |>
  mutate(metric=fct_rev(factor(metric,levels=c("metric2_rel_iod","metric5_rel_iod","metric6_rel_iod","metric7_rel_iod","metric8_rel_iod",
                                               "metric33_rel_iod","metric9_rel_iod","metric34_rel_iod","metric38_rel_iod","metric11_rel_iod",
                                               "metric12_rel_iod","metric13_rel_iod","metric14_rel_iod", "metric15_rel_iod","metric16_rel_iod",
                                               "metric31_rel_iod", "metric17_rel_iod","metric32_rel_iod", "metric39_rel_iod",
                                               "metric40_rel_iod","metric18_rel_iod", "metric19_rel_iod","metric20_rel_iod","metric21_rel_iod",
                                               "metric22_rel_iod","metric23_rel_iod", "metric25_rel_iod","metric26_rel_iod","metric27_rel_iod",
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
                               metric=="metric18_rel_iod"~"Ref to OP cardiology (new)",
                               metric=="metric19_rel_iod"~"Cardiology outpatient DNAs",
                               metric=="metric20_rel_iod"~"Elective PCI",
                               metric=="metric21_rel_iod"~"Elective CABG",
                               metric=="metric22_rel_iod"~"Waiting time for elective PCI / CABG",
                               metric=="metric23_rel_iod"~"PCI/CABG disch < trimpoint",
                               metric=="metric25_rel_iod"~"BP reading < 140/90 (<80 yrs CHD)",
                               metric=="metric26_rel_iod"~"Readmit<30 days of PCI / CABG",
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

return(rel_iod_chart)
}



get_rate_chart_data <- function(activity_by_type_clusters_stg6){
  rate_data <- activity_by_type_clusters_stg6 |>
  select(cluster2,ends_with("_rate"),ends_with("ci"))|>
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
                           metric=="metric25_rate"|metric=="metric26_rate"|metric=="metric27_rate"~"Intermediate outcome",
                           metric=="metric28_rate"|metric=="metric28b_rate"|metric=="metric29_rate"|
                             metric=="metric29b_rate"~"Full outcomes",
                           metric=="metric2_lower_ci"|metric=="metric2_upper_ci"~"Risk factors",
                           metric=="metric5_lower_ci"|metric=="metric6_lower_ci"|metric=="metric7_lower_ci"|metric=="metric8_lower_ci"
                           |metric=="metric33_lower_ci"|metric=="metric5_upper_ci"|metric=="metric6_upper_ci"|metric=="metric7_upper_ci"|
                             metric=="metric8_upper_ci"|metric=="metric33_upper_ci"~"Risk factor identification",
                           metric=="metric9_lower_ci"|metric=="metric34_lower_ci"|metric=="metric38_lower_ci"|metric=="metric11_lower_ci"|
                             metric=="metric12_lower_ci"|metric=="metric9_upper_ci"|metric=="metric34_upper_ci"|metric=="metric38_upper_ci"|
                             metric=="metric11_upper_ci"|metric=="metric12_upper_ci"~"Primary prevention",
                           metric=="metric13_upper_ci"|metric=="metric14_upper_ci"|metric=="metric15_upper_ci"|
                             metric=="metric13_lower_ci"|metric=="metric14_lower_ci"|metric=="metric15_lower_ci"~"Disease identification",
                           metric=="metric16_lower_ci"|metric=="metric31_lower_ci"|metric=="metric17_lower_ci"|metric=="metric32_lower_ci"|
                             metric=="metric39_lower_ci"|metric=="metric40_lower_ci"|metric=="metric18_lower_ci"|
                             metric=="metric19_lower_ci"|metric=="metric16_upper_ci"|metric=="metric31_upper_ci"|metric=="metric17_upper_ci"|
                             metric=="metric32_upper_ci"|metric=="metric39_upper_ci"|metric=="metric40_upper_ci"|metric=="metric18_upper_ci"|
                             metric=="metric19_upper_ci"~"Secondary prevention",
                           metric=="metric20_upper_ci"|metric=="metric21_upper_ci"|metric=="metric22_upper_ci"|
                             metric=="metric23_upper_ci"|metric=="metric20_lower_ci"|metric=="metric21_lower_ci"|metric=="metric22_lower_ci"|
                             metric=="metric23_lower_ci"~"Tertiary prevention",
                           metric=="metric25_upper_ci"|metric=="metric26_upper_ci"|metric=="metric27_upper_ci"|
                             metric=="metric25_lower_ci"|metric=="metric26_lower_ci"|metric=="metric27_lower_ci"~"Intermediate outcome",
                           metric=="metric28_upper_ci"|metric=="metric28b_upper_ci"|metric=="metric29_upper_ci"|
                             metric=="metric29b_upper_ci"|metric=="metric28_lower_ci"|metric=="metric28b_lower_ci"|metric=="metric29_lower_ci"|
                             metric=="metric29b_er_ci"~"Full outcomes",
                           .default="other"))|>
  mutate(metric_name=case_when(metric=="metric2_rate"|metric=="metric2_lower_ci"|metric=="metric2_upper_ci"~"Smoking prev est",
                               metric=="metric5_rate"|metric=="metric5_lower_ci"|metric=="metric5_upper_ci"~"Smoking register",
                               metric=="metric6_rate"|metric=="metric6_lower_ci"|metric=="metric6_upper_ci"~"Obesity register",
                               metric=="metric7_rate"|metric=="metric7_lower_ci"|metric=="metric7_upper_ci"~"Diabetes register",
                               metric=="metric8_rate"|metric=="metric8_lower_ci"|metric=="metric8_upper_ci"~"Depression register",
                               metric=="metric33_rate"|metric=="metric33_lower_ci"|metric=="metric33_upper_ci"~"45+ BP check < 5 years",
                               metric=="metric9_rate"|metric=="metric9_lower_ci"|metric=="metric9_upper_ci"~"CVD risk register",
                               metric=="metric34_rate"|metric=="metric34_lower_ci"|metric=="metric34_upper_ci"~"CVD pat treated with LLT",
                               metric=="metric38_rate"|metric=="metric38_lower_ci"|metric=="metric38_upper_ci"~"Pat at risk treated with LLT",
                               metric=="metric11_rate"|metric=="metric11_lower_ci"|metric=="metric11_upper_ci"~"Smoking cessation support offered",
                               metric=="metric12_rate"|metric=="metric12_lower_ci"|metric=="metric12_upper_ci"~"Except rep. smoke cease sup",
                               metric=="metric13_rate"|metric=="metric13_lower_ci"|metric=="metric13_upper_ci"~"CHD register",
                               metric=="metric14_rate"|metric=="metric14_lower_ci"|metric=="metric14_upper_ci"~"CT angiography",
                               metric=="metric15_rate"|metric=="metric15_lower_ci"|metric=="metric15_upper_ci"~"Electrocardiography",
                               metric=="metric16_rate"|metric=="metric16_lower_ci"|metric=="metric16_upper_ci"~"Asprin anti-platelet etc",
                               metric=="metric31_rate"|metric=="metric31_lower_ci"|metric=="metric31_upper_ci"~"Except rep. asprin etc",
                               metric=="metric17_rate"|metric=="metric17_lower_ci"|metric=="metric17_upper_ci"~"Flu vaccination",
                               metric=="metric32_rate"|metric=="metric32_lower_ci"|metric=="metric32_upper_ci"~"Except rep. flu vacc",
                               metric=="metric39_rate"|metric=="metric39_lower_ci"|metric=="metric39_upper_ci"~"65+ flu vaccination",
                               metric=="metric40_rate"|metric=="metric40_lower_ci"|metric=="metric40_upper_ci"~"<65 at risk flu vaccination",
                               metric=="metric18_rate"|metric=="metric18_lower_ci"|metric=="metric18_upper_ci"~"Ref to OP cardiology (new)",
                               metric=="metric19_rate"|metric=="metric19_lower_ci"|metric=="metric19_upper_ci"~"Cardiology outpatient DNAs",
                               metric=="metric20_rate"|metric=="metric20_lower_ci"|metric=="metric20_upper_ci"~"Elective PCI",
                               metric=="metric21_rate"|metric=="metric21_lower_ci"|metric=="metric21_upper_ci"~"Elective CABG",
                               metric=="metric22_rate"|metric=="metric22_lower_ci"|metric=="metric22_upper_ci"~"Waiting time for elective PCI / CABG",
                               metric=="metric23_rate"|metric=="metric23_lower_ci"|metric=="metric23_upper_ci"~"PCI/CABG disch < trimpoint",
                               metric=="metric25_rate"|metric=="metric25_lower_ci"|metric=="metric25_upper_ci"~"BP reading < 140/90 (<80 yrs CHD)",
                               metric=="metric26_rate"|metric=="metric26_lower_ci"|metric=="metric26_upper_ci"~"Readmit<30 days of PCI / CABG",
                               metric=="metric27_rate"|metric=="metric27_lower_ci"|metric=="metric27_upper_ci"~"Emergency admissions for CHD",
                               metric=="metric28_rate"|metric=="metric28_lower_ci"|metric=="metric28_upper_ci"~"CHD Hospital Deaths",
                               metric=="metric28b_rate"|metric=="metric28b_lower_ci"|metric=="metric28b_upper_ci"~"CHD Hospital Deaths <75",
                               metric=="metric29_rate"|metric=="metric29_lower_ci"|metric=="metric29_upper_ci"~"CHD Deaths",
                               metric=="metric29b_rate"|metric=="metric29b_lower_ci"|metric=="metric29b_upper_ci"~"CHD Deaths <75",
                               .default = metric))|>
  mutate(name_split=str_split(metric,pattern="_"))|>
  rowwise()|>
  mutate(name=unlist(name_split)[[1]],
         type=unlist(name_split)[[2]])|>
  ungroup()|>
  mutate(type=case_when(type=="upper"~"upper_ci",
                        type=="lower"~"lower_ci",
                        .default="rate"))|>
  select(-name_split,-metric)|>
  pivot_wider(names_from=type,
              values_from = value)

global_rates <-activity_by_type_clusters_stg6|>
  select(ends_with("global_rate"))|>
  pivot_longer(cols=starts_with("metric"),
               names_to = "metric",
               values_to = "value")|>
  mutate(name_split=str_split(metric,pattern="_"))|>
  rowwise()|>
  mutate(name=unlist(name_split)[[1]],
         type=unlist(name_split)[[2]])|>
  ungroup()|>
  mutate(type=case_when(type=="global"~"global_rate",
                        .default="error"))|>
  select(-name_split)|>
  unique()

rate_chart_data <-rate_data|>
  left_join(global_rates)|>
  rename(global_rate=value)|>
  select(-metric,-type)

return(rate_chart_data)
}



get_rate_chart <- function(rate_chart_data,pathway_name){
  
palette <- brewer.pal(5, "Set1")
intercept_data <- rate_chart_data|>select(metric_name,global_rate,pathway)|>
  unique()|>
  filter(pathway==pathway_name)

rate_chart <- rate_chart_data|>
  filter(pathway==pathway_name)|>
  ggplot(aes(x=as.factor(cluster2),y=rate, fill=as.factor(cluster2) )) +  
  geom_bar(stat = "identity") +
  geom_errorbar( aes(x=as.factor(cluster2), ymin=lower_ci, ymax=upper_ci), width=0.4, colour="grey", alpha=0.9, size=0.5) +
  scale_fill_manual(values = palette)+
  theme(legend.position="none")+
  xlab("Cluster (1=Least diverse, 5=Most diverse)") +
  ylab("Rate")+
  facet_wrap(vars(metric_name),ncol=3,scales="free_y")+
  geom_hline(aes(yintercept=global_rate),intercept_data)
return(rate_chart)
}
