library(factoextra)
library(cluster)
library(dplyr)
library(targets)
library(GGally)
library(gt)
library(tidyverse)
library(targets)

load("cluster_analysis_v1.Rdata")


View(final_data_full_cats_percents_ex_85whitebritish7_clusters)
View(final_data_full_cats_percents_ex_whitebritish8_clusters)

pams_full_cats_percents_ex_85whitebritish7_clusters_plot

final_data_full_cats_percents_ex_85whitebritish7_clusters|>
 # rownames_to_column(var = 'practice_code') |>
  filter(practice_code=="P85016")

wb <-full_cats_percents |>
  filter(gp_perc_est_white_british>= 85)

final_data_full_cats_percents_ex_85whitebritish7_clusters
  
eight_clusters <- wb |>
  rownames_to_column(var = 'practice_code') |>
  select(practice_code)|>
  mutate(cluster=8)|>
  rbind(final_data_full_cats_percents_ex_85whitebritish7_clusters|>select(practice_code,cluster))

gp_lsoa_with_eth_sum <- tar_read(gp_lsoa_with_eth_sum)|> as_tibble()

eight_clusters <- eight_clusters |> left_join(gp_lsoa_with_eth_sum)
eight_full_cats_percents <- eight_full_cats_percents |> left_join(eight_clusters)

eight_full_cats_percents <- get_full_cats_percents(eight_clusters)

final_data_full_cats_percent_8_clusters <- final_data_full_cats_percent_8_clusters |> rename(cluster2=cluster)
head(eight_clusters)
View(eight_full_cats_percents)

final_data_full_cats_percent_8_clusters <- get_clusters(eight_clusters,eight_full_cats_percents)
gp_geocoded <- tar_read(gp_geocoded) |> as_tibble()

eight_full_cats_percents <- eight_full_cats_percents |> rename(gp_practice_code=practice_code)


cluster_eight_map <- get_cluster2_map(eight_full_cats_percents,gp_geocoded)
cluster_eight_chart <- get_cluster2_chart(eight_full_cats_percents)

get_cluster2_chart <- function(clustered_gp_and_metrics){
  
  
  titles <-clustered_gp_and_metrics |> 
  #  rownames_to_column(var = 'practice_code') |>
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
    #rownames_to_column(var = 'practice_code') |>
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

get_cluster2_map <- function(clustered_gp_and_metrics,gp_geocoded){
  
  # Map the clusters
  # get domain of numeric data
  domain <- unique(clustered_gp_and_metrics$cluster2)
  # make a colour palette
  pal <- colorFactor(palette = brewer.pal(5, "Set1"), domain = domain)
  
  map_data <- clustered_gp_and_metrics |>
 #   rownames_to_column(var = 'practice_code') |>
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