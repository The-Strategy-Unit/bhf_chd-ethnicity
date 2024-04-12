rate_chart_data <- tar_read(rate_chart_data)|>as_tibble()
activity_by_type_clusters_stg6 <- tar_read(activity_by_type_clusters_stg6)|>as_tibble()
rate_chart <- tar_read(rate_chart)
clustered_gp_and_metrics<- tar_read(clustered_gp_and_metrics)|>as_tibble()


geom_segment(
  x = 1, y = 1,
  xend = 4, yend = 7,
  lineend = "round", # See available arrow types in example above
  linejoin = "round",
  size = 2, 
  arrow = arrow(length = unit(0.3, "inches")),
  colour = "#EC7014" # Also accepts "red", "blue' etc
)

palette <- brewer.pal(5, "Set1")
intercept_data <- rate_chart_data |>
  select(metric_name,global_rate,pathway)|>
  unique()|>
  filter(pathway=="Full outcomes")

arrow_data <- rate_chart_data |> 

   mutate(x_start=cluster2,
         y_start=1.1*global_rate,
         x_end=cluster2,
         y_end=0.9*rate)|>
  select(-rate,-global_rate,-upper_ci,-lower_ci)


rate_chart <- rate_chart_data|>
  filter(pathway=="Full outcomes")|>
  ggplot(aes(x=as.factor(cluster2),y=rate, fill=as.factor(cluster2) )) +  
  geom_bar(stat = "identity") +
  geom_errorbar( aes(x=as.factor(cluster2), ymin=lower_ci, ymax=upper_ci), width=0.4, colour="grey", alpha=0.9, size=0.5) +
  scale_fill_manual(values = palette)+
  theme(legend.position="none")+
  xlab("Cluster (1=Least diverse, 5=Most diverse)") +
  ylab("Rate")+
  facet_wrap(vars(metric_name),ncol=3,scales="free_y")+
  geom_hline(aes(yintercept=global_rate),intercept_data)+
  geom_segment(aes(arrow_data),x = x_start, y = y_start,
               xend = x_end, yend = y_end,
               lineend = "round", # See available arrow types in example above
               linejoin = "round",
               size = 2, 
               arrow = arrow(length = unit(0.3, "inches")),
               colour = "grey",
               inherit.aes=FALSE
               )
  
