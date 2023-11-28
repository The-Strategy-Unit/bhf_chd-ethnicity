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

Test_Eth_by_gp <- Test_Eth |>
  group_by(PRACTICE_CODE) |> 
  summarise(gp_sum_whitebritish = sum(WhiteBritishEst),
            gp_sum_irish = sum(IrishEst),
            gp_sum_otherwhite = sum(OtherWhiteEst),
            gp_sum_othermixed = sum(OtherMixedEst),
            gp_sum_otherindian = sum(OtherIndianEst),
            gp_sum_otherpakistani = sum(OtherPakistaniEst),
            gp_sum_otherbangladeshi = sum(OtherBangladeshiEst),
            gp_sum_chinese = sum(ChineseEst),
            gp_sum_otherasian = sum(OtherAsianEst),
            gp_sum_blkafrican = sum(BlackAfricanEst),
            gp_sum_blkcaribbean = sum(BlackCaribbeanEst),
            gp_sum_otherblk = sum(OtherBlackEst),
            gp_sum_arab = sum(ArabEst),
            gp_sum_other = sum(OtherEst)
  ) |> 
  arrange() 


