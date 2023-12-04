library(dplyr)
library(stringr)
library(MLID)
library(tidyverse)

lsoa_eth_sum <- ethnicities |>  
  filter(str_detect(LSOA, '^E'))|> 
  group_by(LSOA) |> 
  summarise(sum_persons = sum(Persons),
                                      sum_whitebritish = sum(WhiteBrit),
                                      sum_irish = sum(Irish),
                                      sum_otherwhite = sum(OtherWhite),
                                      sum_othermixed = sum(Mixed),
                                      sum_otherindian = sum(Indian),
                                      sum_otherpakistani = sum(Pakistani),
                                      sum_otherbangladeshi = sum(Bangladeshi),
                                      sum_chinese = sum(Chinese),
                                      sum_otherasian = sum(OtherAsian),
                                      sum_blkafrican = sum(BlkAfrican),
                                      sum_blkcaribbean = sum(BlkCaribbean),
                                      sum_otherblk = sum(OtherBlk),
                                      sum_arab = sum(Arab),
                                      sum_other = sum(Other)
                                      ) |> 
  arrange()


Densham <- gp_reg_pat_prac_lsoa_all |> 
  filter(PRACTICE_CODE == "A81002") |> 
  rename(LSOA = LSOA_CODE)


Test <- gp_reg_pat_prac_lsoa_all |> 
  rename(LSOA = LSOA_CODE)

Densham_join <- Densham |> 
  left_join(lsoa_eth_sum)

Test_join <- Test |> 
  left_join(lsoa_eth_sum)

Test_Eth <- Test_join |> 
  mutate(WhiteBritishEst = (sum_whitebritish/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(IrishEst = (sum_irish/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherWhiteEst = (sum_otherwhite/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherMixedEst = (sum_othermixed/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherIndianEst = (sum_otherindian/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherPakistaniEst = (sum_otherpakistani/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherBangladeshiEst = (sum_otherbangladeshi/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(ChineseEst = (sum_chinese/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherAsianEst = (sum_otherasian/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(BlackAfricanEst = (sum_blkafrican/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(BlackCaribbeanEst = (sum_blkcaribbean/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherBlackEst = (sum_otherblk/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(ArabEst = (sum_arab/sum_persons)*NUMBER_OF_PATIENTS) |>
  mutate(OtherEst = (sum_other/sum_persons)*NUMBER_OF_PATIENTS) |>
  na.omit()

Test_Eth_by_gp <- joined |>
  na.omit() |>
  group_by(practice_code) |> 
  summarise(gp_sum_est_all_asian = sum(est_all_asian),
            gp_sum_est_bangladeshi = sum(est_bangladeshi),
            gp_sum_est_chinese = sum(est_chinese),
            gp_sum_est_indian = sum(est_indian),
            gp_sum_est_pakistani = sum(est_pakistani),
            gp_sum_est_other_asian = sum(est_other_asian),
            gp_sum_est_all_black = sum(est_all_black),
            gp_sum_est_blk_african = sum(est_blk_african),
            gp_sum_est_blk_caribbean = sum(est_blk_caribbean),
            gp_sum_est_other_blk = sum(est_other_blk),
            gp_sum_est_all_mixed = sum(est_all_mixed),
            gp_sum_est_mixed_other = sum(est_mixed_other),
            gp_sum_est_mixed_white_asian = sum(est_mixed_white_asian),
            gp_sum_est_mixed_white_blk_african = sum(est_mixed_white_blk_african),
            gp_sum_est_mixed_white_blk_caribbean = sum(est_mixed_white_blk_caribbean),
            gp_sum_est_all_white = sum(est_all_white),
            gp_sum_est_white_british = sum(est_white_british),
            gp_sum_est_white_irish = sum(est_white_irish),
            gp_sum_est_white_gypsy_or_irish_traveller = sum(est_white_gypsy_or_irish_traveller),
            gp_sum_est_white_roma = sum(est_white_roma),
            gp_sum_est_white_other_white = sum(est_white_other_white),
            gp_sum_est_all_other = sum(est_all_other),
            gp_sum_est_other_arab = sum(est_other_arab),
            gp_sum_est_other_other = sum(est_other_other),
            gp_sum_est_white_other = sum(est_white_other)
  ) |> 
  arrange() 

Test_Eth_by_gp2 <-Test_Eth_by_gp |>
  select(-gp_sum_est_all_asian,-gp_sum_est_all_black,-gp_sum_est_all_white,-gp_sum_est_mixed_other,
         -gp_sum_est_mixed_white_asian,-gp_sum_est_mixed_white_blk_african,-gp_sum_est_mixed_white_blk_caribbean,
         -gp_sum_est_white_gypsy_or_irish_traveller,-gp_sum_est_white_roma,-gp_sum_est_white_other_white,
         -gp_sum_est_all_other
         ) |>
  mutate(gp_sum_total=gp_sum_est_bangladeshi+gp_sum_est_chinese+gp_sum_est_indian+
           gp_sum_est_pakistani+gp_sum_est_other_asian+gp_sum_est_blk_african+gp_sum_est_blk_caribbean+
           gp_sum_est_other_blk+gp_sum_est_all_mixed+gp_sum_est_white_british+gp_sum_est_white_irish+
           gp_sum_est_white_other+gp_sum_est_other_arab+gp_sum_est_other_other
           )

gp_lsoa2 <- gp_lsoa |>
  group_by(practice_code) |>
  summarise(number_of_patients=sum(number_of_patients))

A81001 <- Test_Eth_by_gp2 |>
  filter(practice_code=="A81001")

A81001gp <- gp_lsoa |>
  filter(practice_code=="A81001")

lsoa_eth_sum_A81001 <- lsoa_eth_sum |>
filter(lsoa_code == "E01011959"|
       lsoa_code == "E01012012"|
       lsoa_code == "E01012023"|
       lsoa_code == "E01012031"|
       lsoa_code == "E01012038"|
       lsoa_code == "E01012041"|
       lsoa_code == "E01012044"|
         lsoa_code == "E01012049"|
         lsoa_code == "E01012081"|
         lsoa_code == "E01012088"|
         lsoa_code == "E01012132"|
         lsoa_code == "E01012172"|
         lsoa_code == "E01012187"|
         lsoa_code == "E01012188"|
         lsoa_code == "E01012189"|
         lsoa_code == "E01012190"|
         lsoa_code == "E01012191"|
         lsoa_code == "E01012192"|
         lsoa_code == "E01012194"|
         lsoa_code == "E01012196"|
         lsoa_code == "E01012201"|
         lsoa_code == "E01012202"|
         lsoa_code == "E01012204"|
         lsoa_code == "E01012205"|
         lsoa_code == "E01012206"|
         lsoa_code == "E01012207"|
         lsoa_code == "E01012208"|
         lsoa_code == "E01012209"|
         lsoa_code == "E01012210"|
         lsoa_code == "E01012211"|
         lsoa_code == "E01012212"|
         lsoa_code == "E01012213"|
         lsoa_code == "E01012215"|
         lsoa_code == "E01012216"|
         lsoa_code == "E01012217"|
         lsoa_code == "E01012218"|
         lsoa_code == "E01012219"|
         lsoa_code == "E01012223"|
         lsoa_code == "E01012224"|
         lsoa_code == "E01012225"|
         lsoa_code == "E01012226"|
         lsoa_code == "E01012230"|
         lsoa_code == "E01012232"|
         lsoa_code == "E01012235"|
         lsoa_code == "E01012236"|
         lsoa_code == "E01012239"|
         lsoa_code == "E01012240"|
         lsoa_code == "E01012243"|
         lsoa_code == "E01012245"|
         lsoa_code == "E01012247"|
         lsoa_code == "E01012248"|
         lsoa_code == "E01012249"|
         lsoa_code == "E01012250"|
         lsoa_code == "E01012251"|
         lsoa_code == "E01012252"|
         lsoa_code == "E01012253"|
         lsoa_code == "E01012254"|
         lsoa_code == "E01012258"|
         lsoa_code == "E01012259"|
         lsoa_code == "E01012260"|
         lsoa_code == "E01012261"|
         lsoa_code == "E01012262"|
         lsoa_code == "E01012264"|
         lsoa_code == "E01012266"|
         lsoa_code == "E01012267"|
         lsoa_code == "E01012268"|
         lsoa_code == "E01012270"|
         lsoa_code == "E01012271"|
         lsoa_code == "E01012272"|
         lsoa_code == "E01012273"|
         lsoa_code == "E01012274"|
         lsoa_code == "E01012276"|
         lsoa_code == "E01012284"|
         lsoa_code == "E01012285"|
         lsoa_code == "E01012286"|
         lsoa_code == "E01012287"|
         lsoa_code == "E01012288"|
         lsoa_code == "E01012289"|
         lsoa_code == "E01012290"|
         lsoa_code == "E01012292"|
         lsoa_code == "E01012294"|
         lsoa_code == "E01012298"|
         lsoa_code == "E01012299"|
         lsoa_code == "E01012312"|
         lsoa_code == "E01012356"|
         lsoa_code == "E01020829"|
         lsoa_code == "E01020830"|
         lsoa_code == "E01032542"|
         lsoa_code == "E01032543"|
         lsoa_code == "E01032544"|
         lsoa_code == "E01032594"|
         lsoa_code == "E01033470"|
         lsoa_code == "E01033474"|
         lsoa_code == "E01033475"|
         lsoa_code == "E01033477"|
         lsoa_code == "E01033478"|
         lsoa_code == "E01033480"
)

census21lsoas <-unique(lsoa_eth_sum$lsoa_code)
gp_list_lsoas<- unique(gp_lsoa$lsoa_code)
gp_list_pcode<- unique(gp_lsoa$practice_code)

one <- setdiff(census21lsoas,gp_list_lsoas) #in census but not in gp 1945
two <- setdiff(gp_list_lsoas,census21lsoas) # in gp but not in census 1034

lsoa_lookup <- 
lsoa_lookup |>
filter(str_detect(lsoa21cd, '^E'))

lsoa11<- unique(lsoa_lookup$f_lsoa11cd) #32844
lsoa21<- unique(lsoa_lookup$lsoa21cd) #33755
#35795

three  <- setdiff(lsoa11,gp_list_lsoas) #0

Test_join21 <-  lsoa_lookup |> 
  left_join(lsoa_eth_sum, join_by(lsoa21cd==lsoa_code))

Test_join <-  lsoa_lookup |> 
  left_join(gp_lsoa, join_by(lsoa21cd==lsoa_code))

test_lsoa <- unique(Test_join21$lsoa21cd) #33755

Test_join |> select(lsoa21cd,f_lsoa11cd, sum_all) |> group_by(f_lsoa11cd) |> summarise(sum_all = sum(sum_all))

Test_join |> select(lsoa21cd,f_lsoa11cd, sum_all) |> 
 # filter(f_lsoa11cd == "E01012264") |>
  group_by(f_lsoa11cd) |> 
  summarise(sum_all = sum(sum_all)) 



lsoa_eth_sum |>
  filter(lsoa_code == "E01035201")

Test_join |> select(lsoa21cd,f_lsoa11cd, sum_all) |> 
  filter(lsoa21cd == "E01035201"|
           lsoa21cd == "E01035202" )


# work out where one new lsoa came from  multiple old lsoas
lsoa_lookup |>
  group_by(lsoa21cd) |>
  count(f_lsoa11cd) |>
  summarise(sum_n=sum(n)) |>
  filter(sum_n>1)

# work out where one old lsoa has become  multiple new lsoas
lsoa_lookup |>
  group_by(f_lsoa11cd) |>
  count(lsoa21cd) |>
  summarise(sum_n=sum(n)) |>
  filter(sum_n>1)


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


full_cats <-
full_cats |>
  remove_rownames() |>
  column_to_rownames(var = 'practice_code')

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

#make this example reproducible
set.seed(1)
#perform k-medoids clustering with k = 4 clusters
pams_five_cats <- pam(scale_five_cats, 4, metric = 'euclidean', stand = FALSE)
#view results
pams_five_cats

#plot results of final k-medoids model
fviz_cluster(pams_five_cats, data = five_cats)

#add cluster assignment to original data
final_data <- cbind(five_cats, cluster = pams_five_cats$cluster)

#view final data
head(final_data)


N81115 
D82048 
K81026 
M85058

five_cats |>   rownames_to_column(var = 'practice_code') |>
  filter(practice_code %in% c("N81115","D82048","K81026","M85058")) |>
  ggplot(aes(x=practice_code),) |>
  geom_col()
