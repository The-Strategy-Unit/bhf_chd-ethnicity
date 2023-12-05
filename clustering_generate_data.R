
##### Clustering tests#####
library(factoextra)
library(cluster)
library(dplyr)
library(targets)
gp_reg_pat_prac <- tar_read(gp_reg_pat_prac_lsoa) |> as_tibble() |>
  group_by(practice_code) |>
  summarise(list_tot= sum(number_of_patients))

gp_lsoa_with_eth_sum <- tar_read(gp_lsoa_with_eth_sum) |> as_tibble()

full_cats <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
         gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
         gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
         gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other)

five_cats <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_all_asian,gp_sum_est_all_black, gp_sum_est_all_mixed, gp_sum_est_all_white,gp_sum_est_all_other)

#Move the practice code into row names
full_cats <-
  full_cats |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

#Move the practice code into row names
five_cats <-
  five_cats |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')


#remove rows with missing values
full_cats <- na.omit(full_cats)
five_cats <- na.omit(five_cats)

#scale each variable to have a mean of 0 and sd of 1
# full_cats <- scale(full_cats)
# five_cats <- scale(five_cats)

scale_full_cats <- scale(full_cats)
scale_five_cats <- scale(five_cats)

# v <- prcomp(scale_full_cats)$x[,1]
# scale_full_cats <- scale_full_cats[order(v),]
# 
# v <- prcomp(scale_five_cats)$x[,1]
# scale_five_cats <- scale_five_cats[order(v),]

head(scale_full_cats)
head(scale_five_cats)

pams_full_cats <- pam(scale_full_cats, 5, metric = 'euclidean', stand = FALSE)
pams_five_cats <- pam(scale_five_cats, 4, metric = 'euclidean', stand = FALSE)

full_cats_plot <- fviz_nbclust(scale_full_cats, pam, method = "wss")
five_cats_plot <- fviz_nbclust(scale_five_cats, pam, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat_full_cats <- clusGap(scale_full_cats,
                              FUN = pam,
                              K.max = 15, #max clusters to consider
                              B = 50) #total bootstrapped iterations

gap_stat_five_cats <- clusGap(scale_five_cats,
                              FUN = pam,
                              K.max = 15, #max clusters to consider
                              B = 50) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
full_cats_gap_plot <- fviz_gap_stat(gap_stat_full_cats)
five_cats_gap_plot <- fviz_gap_stat(gap_stat_five_cats)

#FULL CATS WITH 15 CLUSTERS
#make this example reproducible
set.seed(1)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats <- pam(scale_full_cats, 15, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats

#FIVE CATS WITH 4 CLUSTERS
#make this example reproducible
set.seed(1)
#perform k-medoids clustering with k = 4 clusters
pams_five_cats <- pam(scale_five_cats, 4, metric = 'euclidean', stand = FALSE)
#view results
pams_five_cats

#plot results of final k-medoids model
fviz_cluster(pams_full_cats, data = full_cats)
fviz_cluster(pams_five_cats, data = five_cats)

#add cluster assignment to original data
final_data_full_cats <- cbind(full_cats, cluster = pams_full_cats$cluster)
final_data_five_cats <- cbind(five_cats, cluster = pams_five_cats$cluster)

final_data_full_cats |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")


final_data_five_cats |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#view final data
head(final_data_full_cats)
head(final_data_five_cats)


#These are the 15 cluster medoids for full cats
plot_data <- full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("C81022 ","J82145","G81059","J83053","C81036 ","E85735",
                              "K81030","F81083","K84009 ","B83657","F82011","E85058",
                              "E85033","F84105","F84025"))


#These are the 4 cluster medoids for five cats
five_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("N81115","D82048","K81026","M85058"))

#FULL CATS WITH 8 CLUSTERS
#make this example reproducible
set.seed(1)
#perform k-medoids clustering with k = 8 clusters
pams_full_cats_8_clusters <- pam(scale_full_cats, 8, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_8_clusters

#plot results of final k-medoids model
fviz_cluster(pams_full_cats_8_clusters, data = full_cats)

#add cluster assignment to original data
final_data_full_cats_8_clusters <- cbind(full_cats, cluster = pams_full_cats_8_clusters$cluster)

final_data_full_cats_8_clusters |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

plot_data_8_clusters <- full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("P92621","N81062","M82060","L81669","E86009","E86015",
                              "B83657","G85086"))


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

five_cats_percents <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_all_asian,gp_sum_est_all_black, gp_sum_est_all_mixed, gp_sum_est_all_white,gp_sum_est_all_other,gp_sum_total) |>
  mutate(gp_perc_est_all_asian=(gp_sum_est_all_asian/gp_sum_total)*100,
         gp_perc_est_all_black=(gp_sum_est_all_black/gp_sum_total)*100,
         gp_perc_est_all_mixed=(gp_sum_est_all_mixed/gp_sum_total)*100,
         gp_perc_est_all_white=(gp_sum_est_all_white/gp_sum_total)*100,
         gp_perc_est_all_other=(gp_sum_est_all_other/gp_sum_total)*100
         ) |>
  select(-gp_sum_est_all_asian,-gp_sum_est_all_black,-gp_sum_est_all_mixed,-gp_sum_est_all_white,-gp_sum_est_all_other,-gp_sum_total)


full_cats_percents <-
  full_cats_percents |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

five_cats_percents <-
  five_cats_percents |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

#remove rows with missing values
full_cats_percents <- na.omit(full_cats_percents)
five_cats_percents <- na.omit(five_cats_percents)

#scale each variable to have a mean of 0 and sd of 1
scale_full_cats_percents <- scale(full_cats_percents)
scale_five_cats_percents <- scale(five_cats_percents)

full_cats_percents_plot <- fviz_nbclust(scale_full_cats_percents, pam, method = "wss")
five_cats_percents_plot <- fviz_nbclust(scale_five_cats_percents, pam, method = "wss")

#calculate gap statistic based on number of clusters for percent based data
gap_stat_full_cats_percent <- clusGap(scale_full_cats_percents,
                              FUN = pam,
                              K.max = 15, #max clusters to consider
                              B = 50) #total bootstrapped iterations

gap_stat_five_cats_percent <- clusGap(scale_five_cats_percents,
                              FUN = pam,
                              K.max = 15, #max clusters to consider
                              B = 50) #total bootstrapped iterations


#FULL CATS WITH 15 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(10)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats_percents <- pam(scale_full_cats_percents, 6, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents

#plot results of final k-medoids model
pams_full_cats_percents15_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 15 cluster using percents
pams_full_cats_percents8_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 8 cluster using percents
pams_full_cats_percents6_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 6 cluster using percents
pams_full_cats_percents4_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 4 cluster using percents

pams_full_cats_6_clusters_plot <- fviz_cluster(pams_full_cats_6, data = full_cats) # 6 cluster without percents


#add cluster assignment to original data
final_data_full_cats_percent_6_clusters <- cbind(full_cats, cluster = pams_full_cats_percents$cluster)

final_data_full_cats_percent_6_clusters |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#These are the 4 cluster medoids for full cats with percents
full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("J81626","G81059","H81048","F86018"))

#These are the 6 cluster medoids for full cats with percents
full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("N84617","M83738","C81036","F86612","E85649","G85083"))
rm(temp)
