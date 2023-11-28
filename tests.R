# load packages
library(targets)


source("R/functions.R")


use_targets()

new_metric_16 <- tar_read(qof_chd_2223) |> as_tibble()

metric_16 <- tar_read(metric16) |> as_tibble()

metric_13 <- tar_read(metric13) |> as_tibble()

new_metric_16_short <-new_metric_16 |> select(1,2,3,6,7,32,33,35,38)



new_metric_16$patients_receiving_intervention_percent_38


lsoa_eth_sum <- tar_read(lsoa_eth_sum) |> as_tibble()

metric13b <- tar_read(metric13b) |> as_tibble()

metric25b <- tar_read(metric25b) |> as_tibble()

metric32 <- tar_read(metric32) |> as_tibble()

indid <- 92304
smok <- fingertips_data(IndicatorID = indid,AreaTypeID ="All")
Metric05 <- Metric05 |> filter(AreaType=="GPs")
Metric05 <- Metric05 %>% select(AreaCode,Value, Count)
colnames(Metric05)[colnames(Metric05) == 'Value'] <- 'Metric05Prev'
colnames(Metric05)[colnames(Metric05) == 'Count'] <- 'Metric05Count'
#View(Metric05)