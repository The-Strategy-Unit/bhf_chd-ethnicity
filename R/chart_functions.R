
get_rel_iod_data_for_chart <- function(activity_by_type_clusters_stg6){
  chart_iod_data <- activity_by_type_clusters_stg6 |>
  select(cluster2,list_size_total,ends_with("rel_iod"))|>
  pivot_longer(cols=starts_with("metric"),
               names_to="metric")|>
  select(-cluster2,-list_size_total)|>
  dplyr::distinct(metric,value)
return(chart_iod_data)
}

remove_metric24_for_regional_charts <- function(data){
  data <- data|>
    filter(metric!=("metric24a_rel_iod"))|>
    filter(metric!=("metric24b_rel_iod"))
  return(data)
}

get_rel_iod_chart <- function(chart_iod_data){

  rel_iod_chart <- chart_iod_data |>
    mutate(metric_long_name=case_when(metric=="metric2_rel_iod"~"Smoking prev est",
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
                                      # metric=="metric17_rel_iod"~"Flu vaccination",
                                      # metric=="metric32_rel_iod"~"Except rep. flu vacc",
                                      metric=="metric39_rel_iod"~"65+ flu vaccination",
                                      metric=="metric40_rel_iod"~"<65 at risk flu vaccination",
                                      metric=="metric18_rel_iod"~"Ref to OP cardiology (new)",
                                      metric=="metric19_rel_iod"~"Cardiology outpatient DNAs",
                                      metric=="metric20_rel_iod"~"Elective PCI",
                                      metric=="metric21_rel_iod"~"Elective CABG",
                                      metric=="metric22_rel_iod"~"Waiting time for elective PCI / CABG",
                                      metric=="metric23_rel_iod"~"PCI/CABG disch < trimpoint",
                                      metric=="metric24a_rel_iod"~"Cardiac rehabilitation started",
                                      metric=="metric24b_rel_iod"~"Cardiac rehabilitation completed",
                                      metric=="metric25_rel_iod"~"BP reading < 140/90 (<80 yrs CHD)",
                                      metric=="metric26_rel_iod"~"Readmit<30 days of PCI / CABG",
                                      metric=="metric27_rel_iod"~"Emergency admissions for CHD",
                                      metric=="metric28_rel_iod"~"CHD Hospital Deaths",
                                      metric=="metric28b_rel_iod"~"CHD Hospital Deaths <75",
                                      metric=="metric29_rel_iod"~"CHD Deaths",
                                      metric=="metric29b_rel_iod"~"CHD Deaths <75",
                                      .default = metric))|>
    mutate(metric_long_name=fct_rev(factor(metric_long_name,levels=c("Smoking prev est","Smoking register","Obesity register","Diabetes register",
                                                                     "Depression register", "45+ BP check < 5 years","CVD risk register","CVD pat treated with LLT",
                                                                     "Pat at risk treated with LLT","Smoking cessation support offered",
                                                                     "Except rep. smoke cease sup","CHD register","CT angiography","Electrocardiography",
                                                                     "Asprin anti-platelet etc","Except rep. asprin etc", 
                                                                     #"Flu vaccination","Except rep. flu vacc", 
                                                                     "65+ flu vaccination","<65 at risk flu vaccination",
                                                                     "Ref to OP cardiology (new)", "Cardiology outpatient DNAs","Elective PCI",
                                                                     "Elective CABG","Waiting time for elective PCI / CABG","PCI/CABG disch < trimpoint",
                                                                     "Cardiac rehabilitation started","Cardiac rehabilitation completed",
                                                                     "BP reading < 140/90 (<80 yrs CHD)","Readmit<30 days of PCI / CABG",
                                                                     "Emergency admissions for CHD","CHD Hospital Deaths","CHD Hospital Deaths <75", 
                                                                     "CHD Deaths","CHD Deaths <75")))) |>
    mutate(pathway_level = case_when(metric == 'metric2_rel_iod' ~ 'Risk',
                                     metric == 'metric5_rel_iod' ~ 'Risk identif.',
                                     metric == 'metric6_rel_iod' ~ 'Risk identif.',
                                     metric == 'metric7_rel_iod' ~ 'Risk identif.',
                                     metric == 'metric8_rel_iod' ~ 'Risk identif.',
                                     metric == 'metric33_rel_iod' ~ 'Risk identif.',  
                                     metric == 'metric9_rel_iod' ~ 'Risk identif.',
                                     metric == 'metric34_rel_iod' ~ '1°c prevent',
                                     metric == 'metric38_rel_iod' ~ '1°c prevent',
                                     metric == 'metric11_rel_iod' ~ '1°c prevent', 
                                     metric == 'metric12_rel_iod' ~ '1°c prevent', 
                                     metric == 'metric13_rel_iod' ~ 'Dis. ident',
                                     metric == 'metric14_rel_iod' ~ 'Dis. ident',
                                     metric == 'metric15_rel_iod' ~ 'Dis. ident',
                                     metric == 'metric16_rel_iod' ~ '2°c prevent',
                                     metric == 'metric31_rel_iod' ~ '2°c prevent',
                                     # metric == 'metric17_rel_iod' ~ '2°c prevent',
                                     # metric == 'metric32_rel_iod' ~ '2°c prevent',
                                     metric == 'metric39_rel_iod' ~ '2°c prevent',
                                     metric == 'metric40_rel_iod' ~ '2°c prevent',
                                     metric == 'metric18_rel_iod' ~ '2°c prevent',
                                     metric == 'metric19_rel_iod' ~ '2°c prevent',
                                     metric == 'metric20_rel_iod' ~ '3°c prevent',
                                     metric == 'metric21_rel_iod' ~ '3°c prevent',
                                     metric == 'metric22_rel_iod' ~ '3°c prevent',                              
                                     metric == 'metric23_rel_iod' ~ '3°c prevent',   
                                     metric == 'metric24a_rel_iod' ~ '3°c prevent',  
                                     metric == 'metric24b_rel_iod' ~ '3°c prevent', 
                                     metric == 'metric25_rel_iod' ~ 'Inter. outc.',                                 
                                     metric == 'metric26_rel_iod' ~ 'Inter. outc.',
                                     metric == 'metric27_rel_iod' ~ 'Inter. outc.', 
                                     metric == 'metric28_rel_iod' ~ 'Full outcomes', 
                                     metric == 'metric28b_rel_iod' ~ 'Full outcomes', 
                                     metric == 'metric29_rel_iod' ~ 'Full outcomes',
                                     metric == 'metric29b_rel_iod' ~ 'Full outcomes'
    )) |>
    ggplot(aes(x=metric_long_name, y=value)) +
    geom_segment( aes(x=metric_long_name, xend=metric_long_name, y=0, yend=value), color="grey") +
    geom_point( color="#f9bf07", size=3) +
    coord_flip()+
    facet_grid(rows = factor(pathway_level,levels=c('Risk', 'Risk identif.', '1°c prevent', 'Dis. ident','2°c prevent',
                                                    '3°c prevent', 'Inter. outc.', 'Full outcomes'))~.,
               scales='free_y',
               space='free_y'
               , drop = FALSE
    )+ 
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(
        size = 5, color = "black")
    ) +
    ylim(0,30)+
    xlab("") +
    ylab("Relative Index of Disparity (%)")+
    labs(title = paste("Index of Disparity along CHD pathway"))

return(rel_iod_chart)
}


get_ci_iod_chart <- function(iod_with_ci){
  ci_iod_chart <- iod_with_ci |>
    mutate(metric_long_name=case_when(metric_name=="metric2"~"Smoking prev est",
                                      metric_name=="metric5"~"Smoking register",
                                      metric_name=="metric6"~"Obesity register",
                                      metric_name=="metric7"~"Diabetes register",
                                      metric_name=="metric8"~"Depression register",
                                      metric_name=="metric33"~"45+ BP check < 5 years",
                                      metric_name=="metric9"~"CVD risk register",
                                      metric_name=="metric34"~"CVD pat treated with LLT",
                                      metric_name=="metric38"~"Pat at risk treated with LLT",
                                      metric_name=="metric11"~"Smoking cessation support offered",
                                      metric_name=="metric12"~"Except rep. smoke cease sup",
                                      metric_name=="metric13"~"CHD register",
                                      metric_name=="metric14"~"CT angiography",
                                      metric_name=="metric15"~"Electrocardiography",
                                      metric_name=="metric16"~"Asprin anti-platelet etc",
                                      metric_name=="metric31"~"Except rep. asprin etc",
                                      # metric_name=="metric17"~"Flu vaccination",
                                      # metric_name=="metric32"~"Except rep. flu vacc",
                                      metric_name=="metric39"~"65+ flu vaccination",
                                      metric_name=="metric40"~"<65 at risk flu vaccination",
                                      metric_name=="metric18"~"Ref to OP cardiology (new)",
                                      metric_name=="metric19"~"Cardiology outpatient DNAs",
                                      metric_name=="metric20"~"Elective PCI",
                                      metric_name=="metric21"~"Elective CABG",
                                      metric_name=="metric22"~"Waiting time for elective PCI / CABG",
                                      metric_name=="metric23"~"PCI/CABG disch < trimpoint",
                                      metric_name=="metric24a"~"Cardiac rehabilitation started",
                                      metric_name=="metric24b"~"Cardiac rehabilitation completed",
                                      metric_name=="metric25"~"BP reading < 140/90 (<80 yrs CHD)",
                                      metric_name=="metric26"~"Readmit<30 days of PCI / CABG",
                                      metric_name=="metric27"~"Emergency admissions for CHD",
                                      metric_name=="metric28"~"CHD Hospital Deaths",
                                      metric_name=="metric28b"~"CHD Hospital Deaths <75",
                                      metric_name=="metric29"~"CHD Deaths",
                                      metric_name=="metric29b"~"CHD Deaths <75",
                                      .default = metric_name))|>
    mutate(metric_long_name=fct_rev(factor(metric_long_name,levels=c("Smoking prev est","Smoking register","Obesity register","Diabetes register",
                                                                     "Depression register", "45+ BP check < 5 years","CVD risk register","CVD pat treated with LLT",
                                                                     "Pat at risk treated with LLT","Smoking cessation support offered",
                                                                     "Except rep. smoke cease sup","CHD register","CT angiography","Electrocardiography",
                                                                     "Asprin anti-platelet etc","Except rep. asprin etc", 
                                                                    # "Flu vaccination","Except rep. flu vacc",
                                                                     "65+ flu vaccination","<65 at risk flu vaccination",
                                                                     "Ref to OP cardiology (new)", "Cardiology outpatient DNAs","Elective PCI",
                                                                     "Elective CABG","Waiting time for elective PCI / CABG","PCI/CABG disch < trimpoint",
                                                                    "Cardiac rehabilitation started","Cardiac rehabilitation completed",
                                                                     "BP reading < 140/90 (<80 yrs CHD)","Readmit<30 days of PCI / CABG",
                                                                     "Emergency admissions for CHD","CHD Hospital Deaths","CHD Hospital Deaths <75", 
                                                                     "CHD Deaths","CHD Deaths <75")))) |>
    mutate(pathway_level = case_when(metric_name == 'metric2' ~ 'Risk',
                                 metric_name == 'metric5' ~ 'Risk identif.',
                                 metric_name == 'metric6' ~ 'Risk identif.',
                                 metric_name == 'metric7' ~ 'Risk identif.',
                                 metric_name == 'metric8' ~ 'Risk identif.',
                                 metric_name == 'metric33' ~ 'Risk identif.',  
                                 metric_name == 'metric9' ~ 'Risk identif.',
                                 metric_name == 'metric34' ~ '1°c prevent',
                                 metric_name == 'metric38' ~ '1°c prevent',
                                 metric_name == 'metric11' ~ '1°c prevent', 
                                 metric_name == 'metric12' ~ '1°c prevent', 
                                 metric_name == 'metric13' ~ 'Dis. ident',
                                 metric_name == 'metric14' ~ 'Dis. ident',
                                 metric_name == 'metric15' ~ 'Dis. ident',
                                 metric_name == 'metric16' ~ '2°c prevent',
                                 metric_name == 'metric31' ~ '2°c prevent',
                                 # metric_name == 'metric17' ~ '2°c prevent',
                                 # metric_name == 'metric32' ~ '2°c prevent',
                                 metric_name == 'metric39' ~ '2°c prevent',
                                 metric_name == 'metric40' ~ '2°c prevent',
                                 metric_name == 'metric18' ~ '2°c prevent',
                                 metric_name == 'metric19' ~ '2°c prevent',
                                 metric_name == 'metric20' ~ '3°c prevent',
                                 metric_name == 'metric21' ~ '3°c prevent',
                                 metric_name == 'metric22' ~ '3°c prevent',                              
                                 metric_name == 'metric23' ~ '3°c prevent',   
                                 metric_name == 'metric24a' ~ '3°c prevent',
                                 metric_name == 'metric24b' ~ '3°c prevent', 
                                metric_name == 'metric25' ~ 'Inter. outc.',                                 
                                metric_name == 'metric26' ~ 'Inter. outc.',
                                metric_name == 'metric27' ~ 'Inter. outc.', 
                                metric_name == 'metric28' ~ 'Full outcomes', 
                                metric_name == 'metric28b' ~ 'Full outcomes', 
                                metric_name == 'metric29' ~ 'Full outcomes',
                                metric_name == 'metric29b' ~ 'Full outcomes'
    )) |>
    ggplot() +
    geom_segment( aes(x=metric_long_name, xend=metric_long_name, y=lower_ci*100, yend=upper_ci*100), color="#686f73") +
    #geom_point( aes(x=metric_long_name, y=lower_ci*100), color="#686f73", size=2.5 ) +
    #geom_point( aes(x=metric_long_name, y=upper_ci*100), color="#686f73", size=2.5 ) +
    geom_point( aes(x=metric_long_name, y=iod*100), color="#f9bf07", size=2.5 ) +
    coord_flip()+
    facet_grid(rows = factor(pathway_level,levels=c('Risk', 'Risk identif.', '1°c prevent', 'Dis. ident','2°c prevent',
                                                     '3°c prevent', 'Inter. outc.', 'Full outcomes'))~.,
               scales='free_y',
               space='free_y'
               , drop = FALSE
    )+    
    theme_light() +
    theme(
      plot.title=element_text(size=11),
      panel.grid.major.x = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size=7),
      strip.text = element_text(
        size = 5, color = "black")
    ) +
    xlab("") +
    ylab("Relative Index of Disparity (%)") +
    labs(title = paste("Index of Disparity along CHD pathway (with 95% confidence intervals)"))
  
  
  return(ci_iod_chart)
  
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
                           metric=="metric16_rate"|metric=="metric31_rate"|
                             #metric=="metric17_rate"|metric=="metric32_rate"|
                             metric=="metric39_rate"|metric=="metric40_rate"|metric=="metric18_rate"|
                             metric=="metric19_rate"~"Secondary prevention",
                           metric=="metric20_rate"|metric=="metric21_rate"|metric=="metric22_rate"|
                             metric=="metric23_rate"|metric=="metric24a_rate"|metric=="metric24b_rate"~"Tertiary prevention",
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
                           metric=="metric16_lower_ci"|metric=="metric31_lower_ci"|
                             #metric=="metric17_lower_ci"|metric=="metric32_lower_ci"|
                             metric=="metric39_lower_ci"|metric=="metric40_lower_ci"|metric=="metric18_lower_ci"|
                             metric=="metric19_lower_ci"|metric=="metric16_upper_ci"|metric=="metric31_upper_ci"|metric=="metric17_upper_ci"|
                             metric=="metric32_upper_ci"|metric=="metric39_upper_ci"|metric=="metric40_upper_ci"|metric=="metric18_upper_ci"|
                             metric=="metric19_upper_ci"~"Secondary prevention",
                           metric=="metric20_upper_ci"|metric=="metric21_upper_ci"|metric=="metric22_upper_ci"|
                             metric=="metric23_upper_ci"|metric=="metric24a_upper_ci"|metric=="metric24b_upper_ci"|metric=="metric20_lower_ci"|metric=="metric21_lower_ci"|metric=="metric22_lower_ci"|
                             metric=="metric23_lower_ci"|metric=="metric24a_lower_ci"|metric=="metric24b_lower_ci"~"Tertiary prevention",
                           metric=="metric25_upper_ci"|metric=="metric26_upper_ci"|metric=="metric27_upper_ci"|
                             metric=="metric25_lower_ci"|metric=="metric26_lower_ci"|metric=="metric27_lower_ci"~"Intermediate outcome",
                           metric=="metric28_upper_ci"|metric=="metric28b_upper_ci"|metric=="metric29_upper_ci"| metric=="metric29b_upper_ci"|
                             metric=="metric28_lower_ci"|metric=="metric28b_lower_ci"|metric=="metric29_lower_ci"|
                             metric=="metric29b_lower_ci"~"Full outcomes",
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
                              # metric=="metric17_rate"|metric=="metric17_lower_ci"|metric=="metric17_upper_ci"~"Flu vaccination",
                              # metric=="metric32_rate"|metric=="metric32_lower_ci"|metric=="metric32_upper_ci"~"Except rep. flu vacc",
                               metric=="metric39_rate"|metric=="metric39_lower_ci"|metric=="metric39_upper_ci"~"65+ flu vaccination",
                               metric=="metric40_rate"|metric=="metric40_lower_ci"|metric=="metric40_upper_ci"~"<65 at risk flu vaccination",
                               metric=="metric18_rate"|metric=="metric18_lower_ci"|metric=="metric18_upper_ci"~"Ref to OP cardiology (new)",
                               metric=="metric19_rate"|metric=="metric19_lower_ci"|metric=="metric19_upper_ci"~"Cardiology outpatient DNAs",
                               metric=="metric20_rate"|metric=="metric20_lower_ci"|metric=="metric20_upper_ci"~"Elective PCI",
                               metric=="metric21_rate"|metric=="metric21_lower_ci"|metric=="metric21_upper_ci"~"Elective CABG",
                               metric=="metric22_rate"|metric=="metric22_lower_ci"|metric=="metric22_upper_ci"~"Waiting time for elective PCI / CABG",
                               metric=="metric23_rate"|metric=="metric23_lower_ci"|metric=="metric23_upper_ci"~"PCI/CABG disch < trimpoint",
                               metric=="metric24a_rate"|metric=="metric24a_lower_ci"|metric=="metric24a_upper_ci"~"Cardiac rehabilitation started",
                               metric=="metric24b_rate"|metric=="metric24b_lower_ci"|metric=="metric24b_upper_ci"~"Cardiac rehabilitation completed",
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
 # scale_y_continuous(comma(rate,digits=4,format="g"))+#added this line
  theme_light() +
  theme(legend.position="none")+
  xlab("Cluster (1=Least diverse, 5=Most diverse)") +
  ylab("Rate")+
  facet_wrap(vars(metric_name),ncol=3,scales="free_y")+
  geom_hline(aes(yintercept=global_rate),intercept_data)
return(rate_chart)
}

get_region_cluster_chart_data <- function(region_clustered_gp_and_metrics){
  options(scipen=999)
  
  region_cluster_chart_data <- region_clustered_gp_and_metrics|>
    group_by(comm_region_name,cluster2)|>
    summarise(cluster_list_size=sum(list_size))|>
    ungroup()|>
    complete(comm_region_name,nesting(cluster2),fill=list(cluster_list_size=0),explicit=FALSE)
  return(region_cluster_chart_data)
}

get_region_cluster_chart <- function(region_cluster_chart_data){
  palette <- brewer.pal(5, "Set1")
  options(scipen=999)
  region_cluster_chart<- region_cluster_chart_data |>
    ggplot(aes(x=as.factor(cluster2),y=cluster_list_size, fill=as.factor(cluster2) ))+
    geom_bar(stat = "identity") +
    scale_fill_manual(values = palette)+
    theme_light() +
    theme(legend.position="none")+
    xlab("Cluster (1=Least diverse, 5=Most diverse)")+
    ylab("List Size")+
    scale_y_continuous(limits = c(0,NA))+
    labs(title = paste("List Size by Cluster"))
  return(region_cluster_chart)
}
