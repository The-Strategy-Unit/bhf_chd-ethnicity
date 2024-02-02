
##### Clustering tests#####
library(factoextra)
library(cluster)
library(dplyr)
library(targets)
library(tidyverse)
library(sf)
library(tidygeocoder)
library(leaflet)
library(viridisLite)
library(RColorBrewer)



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

# full_cats_plot <- fviz_nbclust(scale_full_cats, pam, method = "silhouette")
# 
# five_cats_plot <- fviz_nbclust(scale_five_cats, pam, method = "silhouette")



#calculate gap statistic based on number of clusters THIS TAKES AN AGE TO RUN
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

#plot number of clusters vs. gap statistic
full_cats_percent_gap_plot <- fviz_gap_stat(gap_stat_full_cats_percent)
five_cats_percent_gap_plot <- fviz_gap_stat(gap_stat_five_cats_percent)

#FULL CATS WITH 15 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(10)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats_percents <- pam(scale_full_cats_percents, 14, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents

#plot results of final k-medoids model
pams_full_cats_percents15_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 15 cluster using percents
pams_full_cats_percents8_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 8 cluster using percents
pams_full_cats_percents6_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 6 cluster using percents
pams_full_cats_percents4_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 4 cluster using percents

pams_full_cats_percents14_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 14 cluster using percents

pams_full_cats_6_clusters_plot <- fviz_cluster(pams_full_cats_6, data = full_cats) # 6 cluster without percents


#FIVE CATS WITH 5 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(10)
#perform k-medoids clustering with k = 5 clusters
pams_five_cats_percents <- pam(scale_five_cats_percents, 5, metric = 'euclidean', stand = FALSE)
#view results
pams_five_cats_percents

pams_five_cats_percents5_clusters_plot <- fviz_cluster(pams_five_cats_percents, data = full_cats) # 5 cluster using percents

#add cluster assignment to original data
final_data_full_cats_percent_6_clusters <- cbind(full_cats, cluster = pams_full_cats_percents$cluster)
final_data_five_cats_percent_5_clusters <- cbind(five_cats, cluster = pams_five_cats_percents$cluster)

final_data_full_cats_percent_6_clusters |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#These are the 4 cluster medoids for full cats with percents
full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("J81626","G81059","H81048","F86018"))

#These are the 5 cluster medoids for full cats with percents
output_data <- five_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("P83007","D82034","H82088","K81089","P84669"))

#These are the 6 cluster medoids for full cats with percents
full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("N84617","M83738","C81036","F86612","E85649","G85083"))


#Correlation Matrix
library(GGally)
ggcorr(full_cats,
       method = "pairwise",
       nbreaks = 10,
       hjust = 0.9,
       label = TRUE,
       label_size = 3,
       layout.exp = 3)+
  theme(legend.position = c(0.25,0.65))

pca <- princomp(scale_full_cats)
pca$loadings

loadings <- pca$loadings
ggplot(data=full_cats,aes(x=gp_sum_est_blk_caribbean, y=gp_sum_est_blk_african)) +
  geom_point(apha=.3) +
  stat_ellipse(type='norm',level=.99)+
  geom_abline(intercept=0,slope=loadings[2,1]/loadings[1,1]) +
  geom_abline(intercept=0,slope=loadings[2,2]/loadings[1,2])

screeplot(pca, npcs=14)

pca2 <- princomp(scale_full_cats_percents_ex_whitebritish)

pca2$loadings

screeplot(pca2, npcs=14)



################################################################################
  
# Experiment with removal of White British completely but leave all practices in the dataset
full_cats_percents_ex_whitebritish <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
         gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
         gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
         gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total) |>
  mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_chinese=(gp_sum_est_chinese/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_indian=(gp_sum_est_indian/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_pakistani=(gp_sum_est_pakistani/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_asian=(gp_sum_est_other_asian/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_blk_african=(gp_sum_est_blk_african/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_blk=(gp_sum_est_other_blk/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_all_mixed=(gp_sum_est_all_mixed/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_white_irish=(gp_sum_est_white_irish/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_white_other=(gp_sum_est_white_other/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_arab=(gp_sum_est_other_arab/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_other=(gp_sum_est_other_other/(gp_sum_total-gp_sum_est_white_british))*100
  ) |>
  select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
         -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
         -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
         -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total) |>
  mutate(total=gp_perc_est_bangladeshi+gp_perc_est_chinese+gp_perc_est_indian+gp_perc_est_pakistani+gp_perc_est_other_asian+
           gp_perc_est_blk_african+gp_perc_est_blk_caribbean+gp_perc_est_other_blk+gp_perc_est_all_mixed+gp_perc_est_white_irish+
           gp_perc_est_white_other+gp_perc_est_other_arab+gp_perc_est_other_other)


full_cats_percents_ex_whitebritish <-
  full_cats_percents_ex_whitebritish |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

five_cats_percents_ex_whitebritish <-
  five_cats_percents_ex_whitebritish |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

#remove rows with missing values
full_cats_percents_ex_whitebritish <- na.omit(full_cats_percents_ex_whitebritish)
five_cats_percents_ex_whitebritish <- na.omit(five_cats_percents_ex_whitebritish)

#scale each variable to have a mean of 0 and sd of 1
scale_full_cats_percents_ex_whitebritish <- scale(full_cats_percents_ex_whitebritish)
scale_five_cats_percents_ex_whitebritish <- scale(five_cats_percents_ex_whitebritish)

full_cats_percents_ex_whitebritish_plot <- fviz_nbclust(scale_full_cats_percents_ex_whitebritish, pam, method = "wss")

five_cats_percents_ex_whitebritish_plot <- fviz_nbclust(scale_five_cats_percents_ex_whitebritish, pam, method = "wss")

#FULL CATS WITH 15 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(5)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats_percents_ex_whitebritish <- pam(scale_full_cats_percents_ex_whitebritish, 8, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents_ex_whitebritish
#plot results
pams_full_cats_percents_ex_whitebritish8_clusters_plot <- fviz_cluster(pams_full_cats_percents_ex_whitebritish, data = full_cats) 
pams_full_cats_percents_ex_whitebritish8_clusters_plot

#add cluster assignment to original data
final_data_full_cats_percents_ex_whitebritish8_clusters <- cbind(full_cats, cluster = pams_full_cats_percents_ex_whitebritish$cluster)

final_data_full_cats_percents_ex_whitebritish8_clusters |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#These are the 8 cluster medoids for full cats with percents
output_data <- full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("B83010","E86004","M82060","F83055","F81083","H81103","F84740","C84018"))
##############################################################################################################################
# Experiment with removal of White British completely but remove predominantly white (85%+) manually into their own group (2876 removed)
full_cats_percents_ex_85whitebritish <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
         gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
         gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
         gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total) |>
  mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/(gp_sum_total))*100,
         gp_perc_est_chinese=(gp_sum_est_chinese/(gp_sum_total))*100,
         gp_perc_est_indian=(gp_sum_est_indian/(gp_sum_total))*100,
         gp_perc_est_pakistani=(gp_sum_est_pakistani/(gp_sum_total))*100,
         gp_perc_est_other_asian=(gp_sum_est_other_asian/(gp_sum_total))*100,
         gp_perc_est_blk_african=(gp_sum_est_blk_african/(gp_sum_total))*100,
         gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/(gp_sum_total))*100,
         gp_perc_est_other_blk=(gp_sum_est_other_blk/(gp_sum_total))*100,
         gp_perc_est_all_mixed=(gp_sum_est_all_mixed/(gp_sum_total))*100,
         gp_perc_est_white_british=(gp_sum_est_white_british/(gp_sum_total))*100,         
         gp_perc_est_white_irish=(gp_sum_est_white_irish/(gp_sum_total))*100,
         gp_perc_est_white_other=(gp_sum_est_white_other/(gp_sum_total))*100,
         gp_perc_est_other_arab=(gp_sum_est_other_arab/(gp_sum_total))*100,
         gp_perc_est_other_other=(gp_sum_est_other_other/(gp_sum_total))*100
  ) |>
  select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
         -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
         -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
         -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total) |>
  filter(gp_perc_est_white_british <85) |>
  select(-gp_perc_est_white_british)


full_cats_percents_ex_85whitebritish <-
  full_cats_percents_ex_85whitebritish |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

five_cats_percents_ex_85whitebritish <-
  five_cats_percents_ex_85whitebritish |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

#remove rows with missing values
full_cats_percents_ex_85whitebritish <- na.omit(full_cats_percents_ex_85whitebritish)
five_cats_percents_ex_85whitebritish <- na.omit(five_cats_percents_ex_85whitebritish)

#scale each variable to have a mean of 0 and sd of 1
scale_full_cats_percents_ex_85whitebritish <- scale(full_cats_percents_ex_85whitebritish)
scale_five_cats_percents_ex_85whitebritish <- scale(five_cats_percents_ex_85whitebritish)

full_cats_percents_ex_85whitebritish_plot <- fviz_nbclust(scale_full_cats_percents_ex_85whitebritish, pam, method = "wss")
five_cats_percents_ex_85whitebritish_plot <- fviz_nbclust(scale_five_cats_percents_ex_85whitebritish, pam, method = "wss")

#FULL CATS WITH 15 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(5)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats_percents_ex_85whitebritish <- pam(scale_full_cats_percents_ex_85whitebritish, 7, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents_ex_85whitebritish
#plot results
pams_full_cats_percents_ex_85whitebritish7_clusters_plot <- fviz_cluster(pams_full_cats_percents_ex_85whitebritish, data = full_cats) 
pams_full_cats_percents_ex_85whitebritish7_clusters_plot

#add cluster assignment to original data
#remove the 85% from the original

full_cats_percents_ex_85whitebritish_wb <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
         gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
         gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
         gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total) |>
  mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/(gp_sum_total))*100,
         gp_perc_est_chinese=(gp_sum_est_chinese/(gp_sum_total))*100,
         gp_perc_est_indian=(gp_sum_est_indian/(gp_sum_total))*100,
         gp_perc_est_pakistani=(gp_sum_est_pakistani/(gp_sum_total))*100,
         gp_perc_est_other_asian=(gp_sum_est_other_asian/(gp_sum_total))*100,
         gp_perc_est_blk_african=(gp_sum_est_blk_african/(gp_sum_total))*100,
         gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/(gp_sum_total))*100,
         gp_perc_est_other_blk=(gp_sum_est_other_blk/(gp_sum_total))*100,
         gp_perc_est_all_mixed=(gp_sum_est_all_mixed/(gp_sum_total))*100,
         gp_perc_est_white_british=(gp_sum_est_white_british/(gp_sum_total))*100,         
         gp_perc_est_white_irish=(gp_sum_est_white_irish/(gp_sum_total))*100,
         gp_perc_est_white_other=(gp_sum_est_white_other/(gp_sum_total))*100,
         gp_perc_est_other_arab=(gp_sum_est_other_arab/(gp_sum_total))*100,
         gp_perc_est_other_other=(gp_sum_est_other_other/(gp_sum_total))*100
  ) |>
  select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
         -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
         -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
         -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total) |>
  filter(gp_perc_est_white_british <85) |>
  select(practice_code)


final_data_full_cats_percents_ex_85whitebritish7_clusters <- cbind(full_cats_percents_ex_85whitebritish_wb, cluster = pams_full_cats_percents_ex_85whitebritish$cluster)

pams_full_cats_percents_ex_85whitebritish |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#These are the 7 cluster medoids for full cats with percents
output_data <- full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("F81117","Y02973","F81165","F86612","E85003","P86002","G85083"))


##############################################################################################################################
# Experiment with removal of White British by leaving % whiteB as a variable and then creating % for other groups based on white excluded
full_cats_percents_sep_whitebritish <- gp_lsoa_with_eth_sum |>
  select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
         gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
         gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
         gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,gp_sum_total) |>
  mutate(gp_perc_est_bangladeshi=(gp_sum_est_bangladeshi/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_chinese=(gp_sum_est_chinese/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_indian=(gp_sum_est_indian/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_pakistani=(gp_sum_est_pakistani/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_asian=(gp_sum_est_other_asian/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_blk_african=(gp_sum_est_blk_african/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_blk_caribbean=(gp_sum_est_blk_caribbean/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_blk=(gp_sum_est_other_blk/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_all_mixed=(gp_sum_est_all_mixed/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_white_british=(gp_sum_est_white_british/(gp_sum_total))*100,
         gp_perc_est_white_irish=(gp_sum_est_white_irish/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_white_other=(gp_sum_est_white_other/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_arab=(gp_sum_est_other_arab/(gp_sum_total-gp_sum_est_white_british))*100,
         gp_perc_est_other_other=(gp_sum_est_other_other/(gp_sum_total-gp_sum_est_white_british))*100
  ) |>
  select(-gp_sum_est_bangladeshi,-gp_sum_est_chinese,-gp_sum_est_indian,
         -gp_sum_est_pakistani,-gp_sum_est_other_asian,-gp_sum_est_blk_african,-gp_sum_est_blk_caribbean,
         -gp_sum_est_other_blk,-gp_sum_est_all_mixed,-gp_sum_est_white_british,-gp_sum_est_white_irish,
         -gp_sum_est_white_other,-gp_sum_est_other_arab,-gp_sum_est_other_other,-gp_sum_total)

full_cats_percents_sep_whitebritish <-
  full_cats_percents_sep_whitebritish |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

five_cats_percents_sep_whitebritish <-
  five_cats_percents_sep_whitebritish |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

#remove rows with missing values
full_cats_percents_sep_whitebritish <- na.omit(full_cats_percents_sep_whitebritish)
five_cats_percents_sep_whitebritish <- na.omit(five_cats_percents_sep_whitebritish)

#scale each variable to have a mean of 0 and sd of 1
scale_full_cats_percents_sep_whitebritish <- scale(full_cats_percents_sep_whitebritish)
scale_five_cats_percents_sep_whitebritish <- scale(five_cats_percents_sep_whitebritish)

full_cats_percents_sep_whitebritish_plot <- fviz_nbclust(scale_full_cats_percents_sep_whitebritish, pam, method = "wss")
five_cats_percents_sep_whitebritish_plot <- fviz_nbclust(scale_five_cats_percents_sep_whitebritish, pam, method = "wss")

#FULL CATS WITH 6 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(5)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats_percents_sep_whitebritish <- pam(scale_full_cats_percents_sep_whitebritish, 6, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents_sep_whitebritish
#plot results
pams_full_cats_percents_sep_whitebritish6_clusters_plot <- fviz_cluster(pams_full_cats_percents_sep_whitebritish, data = full_cats) 
pams_full_cats_percents_sep_whitebritish6_clusters_plot

#add cluster assignment to original data

final_data_full_cats_percents_sep_whitebritish_clusters <- cbind(full_cats, cluster = pams_full_cats_percents_sep_whitebritish$cluster)

pams_full_cats_percents_sep_whitebritish |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#These are the 6 cluster medoids for full cats with percents
output_data <- full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("M85736","G82802","H81016","C81064","E86004","C84018"))

#############################################################################
d <- dist(full_cats)

hcl <-hclust(d, method="complete", members=NULL)
plot(hcl)

hcl$labels

cut <- cutree(hcl, k = 10, h = NULL)
df_cut <- as.data.frame(cut)

##############################################################################

gp_lsoa_with_eth_sum_over45perc <- tar_read(gp_lsoa_with_eth_sum_over45perc) |> as_tibble()

full_cats <- gp_lsoa_with_eth_sum_over45perc |>
  select(practice_code,gp_sum_est_bangladeshi,gp_sum_est_chinese,gp_sum_est_indian,
         gp_sum_est_pakistani,gp_sum_est_other_asian,gp_sum_est_blk_african,gp_sum_est_blk_caribbean,
         gp_sum_est_other_blk,gp_sum_est_all_mixed,gp_sum_est_white_british,gp_sum_est_white_irish,
         gp_sum_est_white_other,gp_sum_est_other_arab,gp_sum_est_other_other,perc_over45)

five_cats <- gp_lsoa_with_eth_sum_over45perc |>
  select(practice_code,gp_sum_est_all_asian,gp_sum_est_all_black, gp_sum_est_all_mixed, 
         gp_sum_est_all_white,gp_sum_est_all_other,perc_over45)

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

head(scale_full_cats)
head(scale_five_cats)

pams_full_cats <- pam(scale_full_cats, 5, metric = 'euclidean', stand = FALSE)
pams_five_cats <- pam(scale_five_cats, 5, metric = 'euclidean', stand = FALSE)

full_cats_plot <- fviz_nbclust(scale_full_cats, pam, method = "wss")
five_cats_plot <- fviz_nbclust(scale_five_cats, pam, method = "wss")

# full_cats_plot <- fviz_nbclust(scale_full_cats, pam, method = "silhouette")
# 
# five_cats_plot <- fviz_nbclust(scale_five_cats, pam, method = "silhouette")



#calculate gap statistic based on number of clusters THIS TAKES AN AGE TO RUN
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

#up to row 83 above##############################################

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


#These are the 5 cluster medoids for full cats
plot_data <- full_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("L85046","N81029","C81652","K84048","H85111"))


#These are the 5 cluster medoids for five cats
plot_data <- five_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("P82607","L84047","N85620","F86062", "F86607"))

# re-run using percerntage data for the ethnicities

#########################################################################
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



full_cats_percents_over45 <-
  full_cats_percents_over45 |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')



#remove rows with missing values
full_cats_percents_over45 <- na.omit(full_cats_percents_over45)


#scale each variable to have a mean of 0 and sd of 1
scale_full_cats_percents_over45 <- scale(full_cats_percents_over45)


full_cats_percents_plot_over45 <- fviz_nbclust(scale_full_cats_percents_over45, pam, method = "wss")


#calculate gap statistic based on number of clusters for percent based data
gap_stat_full_cats_percent_over45 <- clusGap(scale_full_cats_percents_over45,
                                             FUN = pam,
                                             K.max = 15, #max clusters to consider
                                             B = 50) #total bootstrapped iterations



#plot number of clusters vs. gap statistic
full_cats_percent_gap_plot_over45 <- fviz_gap_stat(gap_stat_full_cats_percent_over45)


#FULL CATS WITH 15 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(10)
#perform k-medoids clustering with k = 15 clusters
pams_full_cats_percents_over45 <- pam(scale_full_cats_percents_over45, 5, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents_over45

#plot results of final k-medoids model

pams_full_cats_percents_over45_5_clusters_plot <- fviz_cluster(pams_full_cats_percents_over45, data = full_cats) # 5 cluster using percents



#add cluster assignment to original data
final_data_full_cats_percent_over45_5_clusters <- cbind(full_cats_percents_over45, cluster = pams_full_cats_percents_over45$cluster)


final_data_full_cats_percent_over45_5_clusters |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")

#These are the 5 cluster medoids for full cats with percents
full_cats_percents_over45_medoids <-  full_cats_percents_over45 |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("G81059","C81036","M91613","E86009", "F84063"))


#Plots to view the clusters
final_data_full_cats_percent_over45_5_clusters |> 
#  rownames_to_column(var = 'practice_code') |>
  select(-perc_over45) |>
  filter(cluster ==4)|>
  pivot_longer(
    cols = starts_with("gp_perc"),
    names_to = "ethnicity",
    values_to = "percent"
  ) |>
  ggplot(aes(x=fct_rev(factor(ethnicity)) , y=percent, fill=factor(ethnicity))) +
  geom_boxplot(outlier.shape = NA) +
  guides(fill = FALSE) +
  coord_flip() +
  xlab("Ethnicity") +
  ylab("Percent") 

titles <-final_data_full_cats_percent_over45_5_clusters |> 
 # rownames_to_column(var = 'practice_code') |>
  select(-perc_over45) |>
  pivot_longer(
    cols = starts_with("gp_perc"),
    names_to = "ethnicity",
    values_to = "percent"
  ) |>
  group_by(cluster,ethnicity) |>
  summarise(med_percent = median(percent)) |>
  mutate(title= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster, " - ", round(med_percent, digits = 0), "% White British")),
  ) |>
  filter(ethnicity =="gp_perc_est_white_british")|>
  select(cluster,title)

median_ethnicities_by_cluster <- final_data_full_cats_percent_over45_5_clusters |> 
#  rownames_to_column(var = 'practice_code') |>
  select(-perc_over45) |>
  pivot_longer(
    cols = starts_with("gp_perc"),
    names_to = "ethnicity",
    values_to = "percent"
  ) |>
  group_by(cluster,ethnicity) |>
  summarise(med_percent = median(percent)) |>
  filter(ethnicity !="gp_perc_est_white_british")|>
  left_join(titles)|>
  ggplot(aes(x=fct_rev(factor(ethnicity)) , y=med_percent, fill=factor(ethnicity))) +
  geom_col() +
  coord_flip() +
  ylab("Median Percent of GP Lists") +
  xlab("") +
  facet_wrap(cluster ~ title) +
  ylim(0,25) +
  labs(title="Ethnicity Median Percent by Cluster") +
  guides(fill = FALSE) 

# Map the clusters
# get domain of numeric data
domain <- range(final_data_full_cats_percent_over45_5_clusters$cluster)
# make a colour palette
pal <- colorNumeric(palette = brewer.pal(5, "Set1"), domain = domain)

map_data <- final_data_full_cats_percent_over45_5_clusters |>
  select(gp_practice_code,cluster) |>

  left_join(gp_geocoded |>
              mutate(id = row_number()), join_by(gp_practice_code==org_code)) |>
  filter(postcode != "NA") |>
  mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

map_plot <- leaflet(map_data) |>
  addTiles() |>
  addCircleMarkers(color = ~pal(cluster),radius=3.5,
                   label = ~as.character(paste0(gp_practice_code,"- Cluster: ",cluster)),
                   stroke = FALSE, fillOpacity = 0.75
                   )
 
######################################################################################
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



full_cats_percents <-
  full_cats_percents |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')


#remove rows with missing values
full_cats_percents <- na.omit(full_cats_percents)

#scale each variable to have a mean of 0 and sd of 1
scale_full_cats_percents <- scale(full_cats_percents)


full_cats_percents_plot <- fviz_nbclust(scale_full_cats_percents, pam, method = "wss")

#calculate gap statistic based on number of clusters for percent based data
gap_stat_full_cats_percent <- clusGap(scale_full_cats_percents,
                                      FUN = pam,
                                      K.max = 15, #max clusters to consider
                                      B = 50) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
full_cats_percent_gap_plot <- fviz_gap_stat(gap_stat_full_cats_percent)


#FULL CATS WITH 5 CLUSTERS PERCENT BASED
#make this example reproducible
set.seed(99)
#perform k-medoids clustering with k = 5 clusters
pams_full_cats_percents <- pam(scale_full_cats_percents, 5, metric = 'euclidean', stand = FALSE)
#view results
pams_full_cats_percents

#plot results of final k-medoids model
pams_full_cats_percents5_clusters_plot <- fviz_cluster(pams_full_cats_percents, data = full_cats) # 5 cluster using percents


#add cluster assignment to original data
final_data_full_cats_percent_5_clusters <- cbind(full_cats_percents, cluster = pams_full_cats_percents$cluster)

final_data_full_cats_percent_5_clusters |>
  rownames_to_column(var = 'practice_code') |>
  group_by(cluster)|>
  count("practice_code")



#Plots to view the clusters
final_data_full_cats_percent_5_clusters |> 
   rownames_to_column(var = 'practice_code') |>
  filter(cluster ==5)|>
  pivot_longer(
    cols = starts_with("gp_perc"),
    names_to = "ethnicity",
    values_to = "percent"
  ) |>
  ggplot(aes(x=fct_rev(factor(ethnicity)) , y=percent, fill=factor(ethnicity))) +
  geom_boxplot(outlier.shape = NA) +
  guides(fill = FALSE) +
  coord_flip() +
  xlab("Ethnicity") +
  ylab("Percent") 

titles <-final_data_full_cats_percent_5_clusters |> 
   rownames_to_column(var = 'practice_code') |>
  pivot_longer(
    cols = starts_with("gp_perc"),
    names_to = "ethnicity",
    values_to = "percent"
  ) |>
  group_by(cluster,ethnicity) |>
  summarise(med_percent = median(percent)) |>
  mutate(title= case_when(ethnicity=="gp_perc_est_white_british" ~ paste0("Cluster ",cluster, " - ", round(med_percent, digits = 0), "% White British")),
  ) |>
  filter(ethnicity =="gp_perc_est_white_british")|>
  select(cluster,title)

median_ethnicities_by_cluster <- final_data_full_cats_percent_5_clusters |> 
    rownames_to_column(var = 'practice_code') |>
  pivot_longer(
    cols = starts_with("gp_perc"),
    names_to = "ethnicity",
    values_to = "percent"
  ) |>
  group_by(cluster,ethnicity) |>
  summarise(med_percent = median(percent)) |>
  filter(ethnicity !="gp_perc_est_white_british")|>
  left_join(titles)|>
  ggplot(aes(x=fct_rev(factor(ethnicity)) , y=med_percent, fill=factor(ethnicity))) +
  geom_col() +
  coord_flip() +
  ylab("Median Percent of GP Lists") +
  xlab("") +
  facet_wrap(cluster ~ title) +
  ylim(0,25) +
  labs(title="Ethnicity Median Percent by Cluster") +
  guides(fill = FALSE) 

# Map the clusters
# get domain of numeric data
domain <- range(final_data_full_cats_percent_5_clusters$cluster)
# make a colour palette
pal <- colorNumeric(palette = brewer.pal(5, "Set1"), domain = domain)

map_data <- final_data_full_cats_percent_5_clusters |>
  rownames_to_column(var = 'practice_code') |>
  select(gp_practice_code=practice_code,cluster) |>
  
  left_join(gp_geocoded |>
              mutate(id = row_number()), join_by(gp_practice_code==org_code)) |>
  filter(postcode != "NA") |>
  mutate(lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2])

map_plot <- leaflet(map_data) |>
  addTiles() |>
  addCircleMarkers(color = ~pal(cluster),radius=3.5,
                   label = ~as.character(paste0(gp_practice_code,"- Cluster: ",cluster)),
                   stroke = FALSE, fillOpacity = 0.75
  )
