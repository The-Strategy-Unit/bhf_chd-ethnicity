################################################################################

# Functions for clustering GP practices by ethnic group

################################################################################

get_full_cats_percents_over45 <- function(gp_lsoa_with_eth_sum_over45perc)
{
  full_cats_percents_over45 <- gp_lsoa_with_eth_sum_over45perc |>
    select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
           gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
           gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
           gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total,perc_over45) |>
    mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/gp_sum_total)*100,
           gp_perc_est_chinese=(gp_sum_est_chinese/gp_sum_total)*100,
           gp_perc_est_indian=(gp_sum_est_indian/gp_sum_total)*100,
           gp_perc_est_pakistani=(gp_sum_est_pakistani/gp_sum_total)*100,
           gp_perc_est_other_asian=(gp_sum_est_other_asian/gp_sum_total)*100,
           gp_perc_est_blk_african=(gp_sum_est_blk_african/gp_sum_total)*100,
           gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/gp_sum_total)*100,
           gp_perc_est_other_blk=(gp_sum_est_other_blk/gp_sum_total)*100,
           gp_perc_est_all_mixed=(gp_sum_est_all_mixed/gp_sum_total)*100,
           gp_perc_est_white_british=(gp_sum_est_white_british/gp_sum_total)*100,
           gp_perc_est_white_irish=(gp_sum_est_white_irish/gp_sum_total)*100,
           gp_perc_est_white_other=(gp_sum_est_white_other/gp_sum_total)*100,
           gp_perc_est_other_arab=(gp_sum_est_other_arab/gp_sum_total)*100,
           gp_perc_est_other_other=(gp_sum_est_other_other/gp_sum_total)*100
    ) |>
    select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
           -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
           -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
           -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total)
  return(full_cats_percents_over45)
}


get_scale_full_cats_percents_over45 <- function(full_cats_percents_over45){
  scale_full_cats_percents_over45 <- full_cats_percents_over45 |>
    remove_rownames() |>
    column_to_rownames(var = 'practice_code') |>
    na.omit() |>
    scale()
  
  return(scale_full_cats_percents_over45)
  
}

# get_clusters_over45 <- function(scale_full_cats_percents_over45,full_cats_percents_over45){
#   #FULL CATS WITH 15 CLUSTERS PERCENT BASED
#   #make this example reproducible
#   set.seed(10)
#   #perform k-medoids clustering with k = 15 clusters
#   pams_full_cats_percents_over45 <- pam(scale_full_cats_percents_over45, 5, metric = 'euclidean', stand = FALSE)
# 
#   final_data_full_cats_percent_over45_5_clusters <-
#     cbind(full_cats_percents_over45, cluster = pams_full_cats_percents_over45$cluster)|>
#     rename(gp_practice_code=practice_code)
# 
#   return(final_data_full_cats_percent_over45_5_clusters)
# }


get_full_cats_percents <- function(gp_lsoa_with_eth_sum)
{
  
  full_cats_percents <- gp_lsoa_with_eth_sum |>
    select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
           gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
           gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
           gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total) |>
    mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/gp_sum_total)*100,
           gp_perc_est_chinese=(gp_sum_est_chinese/gp_sum_total)*100,
           gp_perc_est_indian=(gp_sum_est_indian/gp_sum_total)*100,
           gp_perc_est_pakistani=(gp_sum_est_pakistani/gp_sum_total)*100,
           gp_perc_est_other_asian=(gp_sum_est_other_asian/gp_sum_total)*100,
           gp_perc_est_blk_african=(gp_sum_est_blk_african/gp_sum_total)*100,
           gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/gp_sum_total)*100,
           gp_perc_est_other_blk=(gp_sum_est_other_blk/gp_sum_total)*100,
           gp_perc_est_all_mixed=(gp_sum_est_all_mixed/gp_sum_total)*100,
           gp_perc_est_white_british=(gp_sum_est_white_british/gp_sum_total)*100,
           gp_perc_est_white_irish=(gp_sum_est_white_irish/gp_sum_total)*100,
           gp_perc_est_white_other=(gp_sum_est_white_other/gp_sum_total)*100,
           gp_perc_est_other_arab=(gp_sum_est_other_arab/gp_sum_total)*100,
           gp_perc_est_other_other=(gp_sum_est_other_other/gp_sum_total)*100
    ) |>
    select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
           -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
           -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
           -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total)
  
  
  return(full_cats_percents)
}


get_scale_full_cats_percents<- function(full_cats_percents){
  scale_full_cats_percents <- full_cats_percents |>
    remove_rownames() |>
    column_to_rownames(var = 'practice_code') |>
    na.omit() |>
    scale()
  
  return(scale_full_cats_percents)
}

get_clusters <- function(pams_full_cats_percents,full_cats_percents){
  
  
  final_data <- 
    cbind(full_cats_percents, cluster = pams_full_cats_percents$cluster)|>
    rename(gp_practice_code=practice_code)|>
    mutate(cluster_renum=case_when(cluster==2~1,
                                   cluster==1~2,
                                   cluster==3~3,
                                   cluster==5~4,
                                   cluster==4~5))|>
    select(-cluster)|>
    rename(cluster=cluster_renum)
  
  return(final_data)
}

get_pams <-  function(scale_full_cats_percents){
  set.seed(99)
  #perform k-medoids clustering with k = 5 clusters
  pams_full_cats_percents <- pam(scale_full_cats_percents, 5, metric = 'euclidean', stand = FALSE)
  return(pams_full_cats_percents)
}

get_elbow_plot <- function(scaled_data){
  
  elbow_plot <- fviz_nbclust(scaled_data, pam, method = "wss")
  return(elbow_plot)
}

get_cluster_plot <- function(pams_data){
  #plot results of final k-medoids model
  cluster_plot <- fviz_cluster(pams_data, data = full_cats)
  return(cluster_plot)
}

get_clusters_for_nacr <- function(clustered_gp_and_metrics,gp_icb_mapping){
  
clusters_for_nacr <- clustered_gp_and_metrics |>
  select(gp_practice_code,cluster=cluster2) |>
  left_join(gp_icb_mapping, join_by(gp_practice_code==practice_code)) |>
  select(-sub_icb_location_code,-sub_icb_location_name)

write_excel_csv(clusters_for_nacr,"data/clusters_for_nacr.csv")

return(clusters_for_nacr)
}

get_cluster2_map <- function(clustered_gp_and_metrics,gp_geocoded){
  
  # Map the clusters
  # get domain of numeric data
  domain <- unique(clustered_gp_and_metrics$cluster2)
  # make a colour palette
  pal <- colorFactor(palette = brewer.pal(5, "Set1"), domain = domain)
  
  map_data <- clustered_gp_and_metrics |>
    rownames_to_column(var = 'practice_code') |>
    select(gp_practice_code,cluster2) |>
    
    left_join(gp_geocoded |>
                mutate(id = row_number()), join_by(gp_practice_code==org_code)) |>
    filter(postcode != "NA") |>
    mutate(lon = sf::st_coordinates(geometry)[,1],
           lat = sf::st_coordinates(geometry)[,2])
  
  map_plot2 <- leaflet(map_data) |>
    addTiles() |>
    addCircleMarkers(color = ~pal(cluster2),radius=3.5,
                     label = ~as.character(paste0(gp_practice_code,"- Cluster: ",cluster2)),
                     stroke = FALSE, fillOpacity = 0.75
    ) |>
    addLegend("topright", pal = pal, values = ~cluster2,
              title = "Clusters",
              labels=c("1","2","3","4","5"),
              labFormat = labelFormat(digits = 1),
              opacity = 1
    )
  
  
  return(map_plot2)
  
}

get_cluster1_map <- function(clustered_gp_and_metrics,gp_geocoded){
  
  # Map the clusters
  # get domain of numeric data
  domain <- unique(clustered_gp_and_metrics$cluster1)
  # make a colour palette
  pal <- colorFactor(palette = brewer.pal(5, "Set1"), domain = domain)
  
  map_data <- clustered_gp_and_metrics |>
    rownames_to_column(var = 'practice_code') |>
    select(gp_practice_code,cluster1) |>
    
    left_join(gp_geocoded |>
                mutate(id = row_number()), join_by(gp_practice_code==org_code)) |>
    filter(postcode != "NA") |>
    mutate(lon = sf::st_coordinates(geometry)[,1],
           lat = sf::st_coordinates(geometry)[,2])
  
  map_plot1 <- leaflet(map_data) |>
    addTiles() |>
    addCircleMarkers(color = ~pal(cluster1),radius=3.5,
                     label = ~as.character(paste0(gp_practice_code,"- Cluster: ",cluster1)),
                     stroke = FALSE, fillOpacity = 0.75
    ) |>
    addLegend("topright", pal = pal, values = ~cluster1,
              title = "Clusters",
              labels=c("1","2","3","4","5"),
              labFormat = labelFormat(digits = 1),
              opacity = 1
    )
  
  return(map_plot1)
  
}



get_cluster2_chart <- function(clustered_gp_and_metrics){
  
  
  titles <-clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    mutate(title= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster2, " - ", round(med_percent, digits = 0), "% White British")),
    ) |>
    filter(ethnicity =="gp_perc_est_white_british")|>
    select(cluster2,title)
  
  cluster2_chart <- clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    filter(ethnicity !="gp_perc_est_white_british")|>
    mutate(ethnicity_name=case_when(ethnicity=="gp_perc_est_all_mixed" ~ "All Mixed",
                                    ethnicity=="gp_perc_est_bangladeshi" ~ "Bangladeshi",
                                    ethnicity=="gp_perc_est_blk_african" ~ "Black African",
                                    ethnicity=="gp_perc_est_blk_caribbean" ~ "Black Caribbean",
                                    ethnicity=="gp_perc_est_chinese" ~ "Chinese",
                                    ethnicity=="gp_perc_est_indian" ~ "Indian",
                                    ethnicity=="gp_perc_est_other_arab" ~ "Other Arab",
                                    ethnicity=="gp_perc_est_other_asian" ~ "Other Asian",
                                    ethnicity=="gp_perc_est_other_blk" ~ "Other Black",
                                    ethnicity=="gp_perc_est_other_other" ~ "Other",
                                    ethnicity=="gp_perc_est_pakistani" ~ "Pakistani",
                                    ethnicity=="gp_perc_est_white_irish" ~ "White Irish",
                                    ethnicity=="gp_perc_est_white_other" ~ "White Other",
                                    .default="error")
          )|>
    left_join(titles)|>
    ggplot(aes(x=fct_rev(factor(ethnicity_name)) , y=med_percent, fill=factor(ethnicity_name))) +
    geom_col() +
    coord_flip() +
    ylab("Median Percent of GP Lists") +
    xlab("") +
    facet_wrap(cluster2 ~ title,ncol=2) +
    ylim(0,25) +
    labs(title="Median Percent by Ethnicity") +
    theme(axis.text.y=element_text(size=6))+
    guides(fill = "none") 
  
  return(cluster2_chart)
  
}


get_cluster2_14_eth_chart <- function(clustered_gp_and_metrics,cluster_num){
  
  
  titles <-clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    mutate(title_name= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster2, " - ", round(med_percent, digits = 0), "% White British")),
    ) |>
    filter(ethnicity =="gp_perc_est_white_british")|>
    select(cluster2,title_name)
  
  cluster2_14_eth_chart <- clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    filter(ethnicity !="gp_perc_est_white_british")|>
    mutate(ethnicity_name=case_when(ethnicity=="gp_perc_est_all_mixed" ~ "All Mixed",
                                    ethnicity=="gp_perc_est_bangladeshi" ~ "Bangladeshi",
                                    ethnicity=="gp_perc_est_blk_african" ~ "Black African",
                                    ethnicity=="gp_perc_est_blk_caribbean" ~ "Black Caribbean",
                                    ethnicity=="gp_perc_est_chinese" ~ "Chinese",
                                    ethnicity=="gp_perc_est_indian" ~ "Indian",
                                    ethnicity=="gp_perc_est_other_arab" ~ "Other Arab",
                                    ethnicity=="gp_perc_est_other_asian" ~ "Other Asian",
                                    ethnicity=="gp_perc_est_other_blk" ~ "Other Black",
                                    ethnicity=="gp_perc_est_other_other" ~ "Other",
                                    ethnicity=="gp_perc_est_pakistani" ~ "Pakistani",
                                    ethnicity=="gp_perc_est_white_irish" ~ "White Irish",
                                    ethnicity=="gp_perc_est_white_other" ~ "White Other",
                                    .default="error")
    )|>
    left_join(titles)|>
    filter(cluster2==cluster_num)|>
    mutate(ethnicity_name=fct_reorder(ethnicity_name,desc(med_percent))) |>
    ggplot(aes(x=fct_rev(factor(ethnicity_name)) , y=med_percent, fill=factor(ethnicity_name))) +
    geom_col() +
    coord_flip() +
    ylab("Median Percent of GP Lists") +
    xlab("") +
  #  facet_wrap(cluster2 ~ title,ncol=2) +
    ylim(0,25) +
    labs(title="Median Percent by Ethnicity") +
    theme(axis.text.y=element_text(size=6))+
    guides(fill = "none") 
  
  return(cluster2_14_eth_chart)
  
}


get_cluster1_chart <- function(clustered_gp_and_metrics){
  
  
  titles <-clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster1,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    mutate(title= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster1, " - ", round(med_percent, digits = 0), "% White British")),
    ) |>
    filter(ethnicity =="gp_perc_est_white_british")|>
    select(cluster1,title)
  
  cluster1_chart <- clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    pivot_longer(
      cols = starts_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster1,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    filter(ethnicity !="gp_perc_est_white_british")|>
    left_join(titles)|>
    ggplot(aes(x=fct_rev(factor(ethnicity)) , y=med_percent, fill=factor(ethnicity))) +
    geom_col() +
    coord_flip() +
    ylab("Median Percent of GP Lists") +
    xlab("") +
    facet_wrap(cluster1 ~ title) +
    ylim(0,25) +
    labs(title="Ethnicity Median Percent by Cluster") +
    guides(fill = FALSE) 
  
  return(cluster1_chart)
  
}

get_cluster2_treemap_data <- function(clustered_gp_and_metrics){

  cluster2_treemap_data<-clustered_gp_and_metrics |> 
    rownames_to_column(var = 'practice_code') |>
    select(cluster2,gp_practice_code,starts_with("gp_perc"))|>
    mutate(all_asian_gp_perc=gp_perc_est_bangladeshi+gp_perc_est_chinese+gp_perc_est_indian+gp_perc_est_pakistani+gp_perc_est_other_asian,
           all_black_gp_perc=gp_perc_est_blk_african+gp_perc_est_blk_caribbean+gp_perc_est_other_blk,
           all_white_gp_perc=gp_perc_est_white_british+gp_perc_est_white_irish+gp_perc_est_white_other,
           all_other_gp_perc=gp_perc_est_other_arab+gp_perc_est_other_other,
           all_mixed_gp_perc=gp_perc_est_all_mixed) |>
    select(cluster2,gp_practice_code,ends_with("gp_perc")) |>
    pivot_longer(
      cols = ends_with("gp_perc"),
      names_to = "ethnicity",
      values_to = "percent"
    ) |>
    group_by(cluster2,ethnicity) |>
    summarise(med_percent = median(percent)) |>
    mutate(ethnicity_name=case_when(ethnicity=="all_asian_gp_perc" ~ "Asian",
                                    ethnicity=="all_black_gp_perc" ~ "Black",
                                    ethnicity=="all_white_gp_perc" ~ "White",
                                    ethnicity=="all_other_gp_perc" ~ "Other",
                                    ethnicity=="all_mixed_gp_perc" ~ "Mixed")) 
  
  
return(cluster2_treemap_data)
}


get_cluster2_treemap_1 <- function(cluster2_treemap_data){
  #SU colours
  su_yellow <- '#f9bf07'
  su_red <- '#ec6555'
  su_blue <- '#5881c1'
  su_grey <- '#686f73'
  su_black <- '#2c2825'

  su_light_yellow <- '#ffe699'
  su_light_red <- '#f4a39a'
  su_light_blue <- '#b4c6e6'
  su_light_grey <- '#d9d9d9'
  

  
  cluster2_treemap_1 <- cluster2_treemap_data |>
    filter(cluster2==1)|>
    ggplot(label = paste(ethnicity_name, med_percent, sep = "\n")) +
    geom_treemap(aes(area = med_percent, 
                     fill = ethnicity_name),
                 colour = 'white',
                 size = 2) +
    geom_treemap_text(aes(area = med_percent, 
                          label = paste(ethnicity_name, round(med_percent,1), sep = "\n")), 
                      grow = FALSE, 
                      size = 12, colour = 'white') +
    scale_fill_manual(values = c(su_blue, su_red, su_grey, su_black, su_yellow)) +
    theme(legend.position = 'none') +
    labs(title = "Cluster 1")
  
return(cluster2_treemap_1)
}

get_cluster2_treemap_2 <- function(cluster2_treemap_data){
  # SU colours
  su_yellow <- '#f9bf07'
  su_red <- '#ec6555'
  su_blue <- '#5881c1'
  su_grey <- '#686f73'
  su_black <- '#2c2825'
  
  su_light_yellow <- '#ffe699'
  su_light_red <- '#f4a39a'
  su_light_blue <- '#b4c6e6'
  su_light_grey <- '#d9d9d9'
  
  cluster2_treemap_2 <- cluster2_treemap_data |>
    filter(cluster2==2)|>
    ggplot() +
    geom_treemap(aes(area = med_percent, 
                     fill = ethnicity_name),
                 colour = 'white',
                 size = 2) +
    geom_treemap_text(aes(area = med_percent, 
                          label = paste(ethnicity_name, round(med_percent,1), sep = "\n")), 
                      grow = FALSE, 
                      size = 12, colour = 'white') +
    scale_fill_manual(values = c(su_blue, su_red, su_grey, su_black, su_yellow)) +
    theme(legend.position = 'none') +
    labs(title = "Cluster 2")
  
  return(cluster2_treemap_2)
}

get_cluster2_treemap_3 <- function(cluster2_treemap_data){
  # SU colours
  su_yellow <- '#f9bf07'
  su_red <- '#ec6555'
  su_blue <- '#5881c1'
  su_grey <- '#686f73'
  su_black <- '#2c2825'
  
  su_light_yellow <- '#ffe699'
  su_light_red <- '#f4a39a'
  su_light_blue <- '#b4c6e6'
  su_light_grey <- '#d9d9d9'
  
  cluster2_treemap_3 <- cluster2_treemap_data |>
    filter(cluster2==3)|>
    ggplot() +
    geom_treemap(aes(area = med_percent, 
                     fill = ethnicity_name),
                 colour = 'white',
                 size = 2) +
    geom_treemap_text(aes(area = med_percent, 
                          label = paste(ethnicity_name, round(med_percent,1), sep = "\n")), 
                      grow = FALSE, 
                      size = 12, colour = 'white') +
    scale_fill_manual(values = c(su_blue, su_red, su_grey, su_black, su_yellow)) +
    theme(legend.position = 'none') +
    labs(title = "Cluster 3")
  
  return(cluster2_treemap_3)
}

get_cluster2_treemap_4 <- function(cluster2_treemap_data){
  # SU colours
  su_yellow <- '#f9bf07'
  su_red <- '#ec6555'
  su_blue <- '#5881c1'
  su_grey <- '#686f73'
  su_black <- '#2c2825'
  
  su_light_yellow <- '#ffe699'
  su_light_red <- '#f4a39a'
  su_light_blue <- '#b4c6e6'
  su_light_grey <- '#d9d9d9'
  
  cluster2_treemap_4 <- cluster2_treemap_data |>
    filter(cluster2==4)|>
    ggplot() +
    geom_treemap(aes(area = med_percent, 
                     fill = ethnicity_name),
                 colour = 'white',
                 size = 2) +
    geom_treemap_text(aes(area = med_percent, 
                          label = paste(ethnicity_name, round(med_percent,1), sep = "\n")), 
                      grow = FALSE, 
                      size = 12, colour = 'white') +
    scale_fill_manual(values = c(su_blue, su_red, su_grey, su_black, su_yellow)) +
    theme(legend.position = 'none') +
    labs(title = "Cluster 4")
  
  return(cluster2_treemap_4)
}

get_cluster2_treemap_5 <- function(cluster2_treemap_data){
  # SU colours
  su_yellow <- '#f9bf07'
  su_red <- '#ec6555'
  su_blue <- '#5881c1'
  su_grey <- '#686f73'
  su_black <- '#2c2825'
  
  su_light_yellow <- '#ffe699'
  su_light_red <- '#f4a39a'
  su_light_blue <- '#b4c6e6'
  su_light_grey <- '#d9d9d9'
  
  cluster2_treemap_5 <- cluster2_treemap_data |>
    filter(cluster2==5)|>
    ggplot() +
    geom_treemap(aes(area = med_percent, 
                     fill = ethnicity_name),
                 colour = 'white',
                 size = 2) +
    geom_treemap_text(aes(area = med_percent, 
                          label = paste(ethnicity_name, round(med_percent,1), sep = "\n")), 
                      grow = FALSE, 
                      size = 12, colour = 'white') +
    scale_fill_manual(values = c(su_blue, su_red, su_grey, su_black, su_yellow)) +
    theme(legend.position = 'none') +
    labs(title = "Cluster 5")
  
  return(cluster2_treemap_5)
}
