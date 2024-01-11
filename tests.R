# load packages
library(targets)
library(dplyr)

source("R/functions.R")


use_targets()

new_metric_16 <- tar_read(qof_chd_2223) |> as_tibble()

metric_16 <- tar_read(metric16) |> as_tibble()

metric_13 <- tar_read(metric13) |> as_tibble()

new_metric_16_short <-new_metric_16 |> select(1,2,3,6,7,32,33,35,38)

joined <- tar_read(joined) |> as_tibble()


new_metric_16$patients_receiving_intervention_percent_38

gp_lsoa <- tar_read(gp_lsoa) |> as_tibble()

lsoa_eth_sum <- tar_read(lsoa_eth_sum) |> as_tibble()

lsoa_lookup <- tar_read(lsoa_lookup) |> as_tibble()

gp_lsoa_with_eth_sum <- tar_read(gp_lsoa_with_eth_sum) |> as_tibble()

metric1 <- tar_read(metric1) |> as_tibble()
gp_history <- tar_read(gp_history) |> as_tibble()

metric40 <- tar_read(metric40) |> as_tibble()

metric25b <- tar_read(metric25b) |> as_tibble()

metric32 <- tar_read(metric32) |> as_tibble()

indid <- 92304
smok <- fingertips_data(IndicatorID = indid,AreaTypeID ="All")
Metric05 <- Metric05 |> filter(AreaType=="GPs")
Metric05 <- Metric05 %>% select(AreaCode,Value, Count)
colnames(Metric05)[colnames(Metric05) == 'Value'] <- 'Metric05Prev'
colnames(Metric05)[colnames(Metric05) == 'Count'] <- 'Metric05Count'
#View(Metric05)



