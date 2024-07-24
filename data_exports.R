
  
# This is it!  
the_data <- purrr::map(icb_charts, "data") |> dplyr::bind_rows(.id = "icb") 
write.csv(the_data, "icb_data.csv")


rate_chart_data <- targets::tar_read(rate_chart_data) |> dplyr::as_tibble()
rate_chart_data <- rate_chart_data |> 
  dplyr::rename(cluster=cluster2)
write.csv(rate_chart_data, "rate_chart_data.csv")

iod_with_ci <- targets::tar_read(iod_with_ci) |> dplyr::as_tibble()
disparity_data <- iod_with_ci |> dplyr::left_join(metric_names)
write.csv(disparity_data, "iod_data.csv")
