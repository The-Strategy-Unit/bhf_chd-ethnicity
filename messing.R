
get_rate_chart_data <- function(activity_by_type_clusters_stg6){
  
  
  global_rate <- activity_by_type_clusters_stg6|>
    select(ends_with("global_rate"))|>
    pivot_longer(cols=starts_with("metric"),
                 names_to="metric")|>
    unique()
  
  
  
  rate_chart_data <- activity_by_type_clusters_stg6 |>
    select(cluster2,ends_with("_rate"))|>
    select(-ends_with("global_rate"),-ends_with("diff_rate"))|>
    
    pivot_longer(cols=c(starts_with("metric"),ends_with("global_rate")),
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
                             metric=="metric2_global_rate"~"Risk factors",
                             metric=="metric5_global_rate"|metric=="metric6_global_rate"|metric=="metric7_global_rate"|metric=="metric8_global_rate"
                             |metric=="metric33_global_rate"~"Risk factor identification",
                             metric=="metric9_global_rate"|metric=="metric34_global_rate"|metric=="metric38_global_rate"|metric=="metric11_global_rate"|
                               metric=="metric12_global_rate"~"Primary prevention",
                             metric=="metric13_global_rate"|metric=="metric14_global_rate"|metric=="metric15_global_rate"~"Disease identification",
                             metric=="metric16_global_rate"|metric=="metric31_global_rate"|metric=="metric17_global_rate"|metric=="metric32_global_rate"|
                               metric=="metric39_global_rate"|metric=="metric40_global_rate"|metric=="metric18_global_rate"|
                               metric=="metric19_global_rate"~"Secondary prevention",
                             metric=="metric20_global_rate"|metric=="metric21_global_rate"|metric=="metric22_global_rate"|
                               metric=="metric23_global_rate"~"Tertiary prevention",
                             metric=="metric25_global_rate"|metric=="metric27_global_rate"~"Intermediate outcome",
                             metric=="metric28_global_rate"|metric=="metric28b_global_rate"|metric=="metric29_global_rate"|
                               metric=="metric29b_global_rate"~"Full outcomes",
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
                                 metric=="metric18_rate"~"Ref to OP cardiology (new)",
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
                                 .default = metric))
  
  return(rate_chart_data)
}


chart_data <- get_rate_chart_data(activity_by_type_clusters_stg6)


get_rate_chart(chart_data,"Disease identification")




get_rate_chart <- function(rate_chart_data,pathway_name){
  
  palette <- brewer.pal(5, "Set1")
  
  rate_chart <- rate_chart_data|>
    filter(pathway==pathway_name)|>
    ggplot(aes(x=as.factor(cluster2),y=value, fill=as.factor(cluster2) )) +  
    geom_bar(stat = "identity") +
    geom_hline(global_rate, aes( value, "blue" ))+
#    geom_line(aes(x=as.factor(cluster2),y=0.5),stat="identity")+
    scale_fill_manual(values = palette)+
    theme(legend.position="none")+
    xlab("Cluster (1=Least diverse, 5=Most diverse)") +
    ylab("Rate")+
    facet_wrap(vars(metric_name),ncol=3,scales="free_y")
  return(rate_chart)
}




