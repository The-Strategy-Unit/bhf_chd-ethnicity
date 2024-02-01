
library(factoextra)
library(cluster)
library(dplyr)
library(targets)
library(tidyverse)
#calculate gap statistic based on number of clusters for percent based data
gap_stat_full_cats_percent_over45 <- clusGap(scale_full_cats_percents_over45,
                                             FUN = pam,
                                             K.max = 15, #max clusters to consider
                                             B = 50) #total bootstrapped iterations