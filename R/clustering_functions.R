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

get_clusters <- function(scale_full_cats_percents_over45,full_cats_percents_over45){
  #FULL CATS WITH 15 CLUSTERS PERCENT BASED
  #make this example reproducible
  set.seed(10)
  #perform k-medoids clustering with k = 15 clusters
  pams_full_cats_percents_over45 <- pam(scale_full_cats_percents_over45, 5, metric = 'euclidean', stand = FALSE)
  
  final_data_full_cats_percent_over45_5_clusters <- 
    cbind(full_cats_percents_over45, cluster = pams_full_cats_percents_over45$cluster)|>
    rename(gp_practice_code=practice_code)
  
  return(final_data_full_cats_percent_over45_5_clusters)
}


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
    left_join(titles)|>
    ggplot(aes(x=fct_rev(factor(ethnicity)) , y=med_percent, fill=factor(ethnicity))) +
    geom_col() +
    coord_flip() +
    ylab("Median Percent of GP Lists") +
    xlab("") +
    facet_wrap(cluster2 ~ title) +
    ylim(0,25) +
    labs(title="Ethnicity Median Percent by Cluster") +
    guides(fill = FALSE) 
  
  return(cluster2_chart)
  
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

