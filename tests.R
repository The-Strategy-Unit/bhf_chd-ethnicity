# load packages
library(targets)


source("R/functions.R")


use_targets()

new_metric_16 <- tar_read(qof_chd_2223) |> as_tibble()

metric_16 <- tar_read(metric16) |> as_tibble()

metric_13 <- tar_read(metric13) |> as_tibble()

new_metric_16_short <-new_metric_16 |> select(1,2,3,6,7,32,33,35,38)



new_metric_16$patients_receiving_intervention_percent_38


metric31 <- tar_read(metric31) |> as_tibble()

metric13b <- tar_read(metric13b) |> as_tibble()

metric25b <- tar_read(metric25b) |> as_tibble()

metric32 <- tar_read(metric32) |> as_tibble()

