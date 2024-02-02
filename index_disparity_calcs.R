library(glue)
library(dplyr)
library(tidyverse)

get_disparity_ratio <- function(activity_by_type_clusters_stg1) {
  options(scipen=999) # disable scientific notation
  #options(scipen=0)
  
  activity_type_decile <- activity_by_type_clusters_stg1
  
  # Metrics 2 - 9 listed as use GP list size 16+ as denominator
  # else uses CHD prevalence
  by_list_total <- c(
    "2", "5",
    "6", "7",
    "8", "9"
  )
  
  # Produces a string to be used in an if(else) in later code to match against
  # the metric numbers listed in by_list_total
  by_list_regex <- glue("^metric({paste(by_list_total, collapse = '|')})_total$")
  
  # Data manipulation code --------------------------------------------------
  activity_long <- activity_type_decile |>
    pivot_longer(
      cols = c(starts_with("metric"), -metric1_total),
      names_to = "metric_name",
      values_to = "metric_total"
    ) |>
    mutate(
      total_column = ifelse(
        str_detect(metric_name, by_list_regex),
        list_size_total,
        metric1_total
      ),
      total_rate = metric_total / total_column * 1000
    ) |>
    select(
      quantile = cluster,
      metric1_total,
      metric_name,
      metric_total,
      total_rate
    ) |>
    mutate(activity = (total_rate * metric1_total / 1000)) |>
    group_by(metric_name) |>
    # arrange() required for min() and max() functions to calculate
    # largest_value and smallest_value
    arrange(
      metric_name,
      quantile
    ) |>
    mutate(
      total_activity = sum(activity),
      prevalence = sum(metric1_total),
      overall_value = total_activity / prevalence * 1000,
      population = metric1_total,
      largest_value = max(total_rate),
      smallest_value = min(total_rate),
      total_pop = sum(population),
      proportion_pop = population / total_pop,
      abs_range = largest_value - smallest_value, # not used in this particular code
      rel_range = largest_value / smallest_value,
      disparity_ratio = total_rate / largest_value,
      abs_diff = abs(total_rate - largest_value)
    ) |>
    ungroup() 
 
  
  return(activity_long)
}

table_data <- activity_long |>
  select(cluster=quantile, metric_name, disparity_ratio, largest_value)
