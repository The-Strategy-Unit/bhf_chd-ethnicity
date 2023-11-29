library(dplyr)
library(stringr)
library(MLID)

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
