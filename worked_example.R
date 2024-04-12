one_metric_example_data <- tar_read(activity_by_type_clusters_stg6) |>
  as_tibble()|>
  select(cluster2,metric1_total,starts_with("metric29b"))|>
  rename(denominator=metric1_total,numerator=metric29b_total)




# Function to compute index of disparity
compute_id <- function(one_metric_example_data) {
  # Compute index of disparity
  data <- one_metric_example_data|>
    mutate(upper_ci=(numerator+(sqrt(numerator)*1.96))*(1/denominator),
           lower_ci=(numerator-(sqrt(numerator)*1.96))*(1/denominator),
           se=(numerator-upper_ci)/1.96,
           sd=(numerator-mean(numerator))
           rate=numerator/denominator,
           global_rate=sum(numerator)/sum(denominator),
           diff = rate-global_rate,
           abs_diff=abs(diff*denominator))
  iod_abs <- sum(data$abs_diff)/2
  iod_rel <- sum(data$abs_diff)/(2*sum(data$numerator))
  return(iod_rel)
}



rnorm()
