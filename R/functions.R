################################################################################

# Other Functions

################################################################################
#get SU colours
# get_su_colours <- function(){
#   su_yellow <- '#f9bf07'
#   su_red <- '#ec6555'
#   su_blue <- '#5881c1'
#   su_grey <- '#686f73'
#   su_black <- '#2c2825'
#   
#   su_light_yellow <- '#ffe699'
#   su_light_red <- '#f4a39a'
#   su_light_blue <- '#b4c6e6'
#   su_light_grey <- '#d9d9d9'
#   
#   su_colour_list <- list(c(su_yellow,su_red,su_blue,su_grey,su_black,su_light_yellow,su_light_red,su_light_blue,su_light_grey),
#                          c("su_yellow","su_red","su_blue","su_grey","su_black","su_light_yellow","su_light_red","su_light_blue",
#                            "su_light_grey")
#                          )
#   
#   return(su_colour_list)
#   
# }


#join the chd prev to gp history
get_joined_gp_history_and_chd_prev <- function (metric1,gp_history_short){
    orig <- metric1 |> select(AreaCode,AreaName, Value, IsCurrent) |>
    rename(org_code=AreaCode) |>
    left_join(gp_history_short)
  return(orig)
}

# joins gp list to the prevalence and geocodes all practices
get_geocoded_data <- function(gp_list_summary,orig,joined_gp_history_and_chd_prev){
  
  joined <- gp_list_summary |>
    left_join(joined_gp_history_and_chd_prev) |>
    #where there is a matching value from orig in the gp list summary then flag these 
    mutate(has_orig_prev=case_when(Value>=0 ~ 1, .default =  0))
  
  orig_with_geocode <- joined |>
    select(org_code,practice_name,postcode,has_orig_prev) |>
    mutate(id = row_number()) |>
    geocode(postalcode = postcode) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326,na.fail=FALSE)

  return(orig_with_geocode)
}


get_45over_list <- function(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)
{gp_list_45over <- gp_reg_pat_prac_sing_age_male |>
  rbind(gp_reg_pat_prac_sing_age_female)|>
  filter(age != "ALL") |>
  group_by(org_code,age) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                         .default = as.numeric(age))
  )) |>
  mutate(over_45=case_when(age>= 45 ~1,
                           .default = 0)) |>
  group_by(org_code,over_45)|>
  summarise(number_of_patients=sum(number_of_patients))|>
  ungroup()|>
  filter(over_45==1)|>
  select(practice_code=org_code,number_45over=number_of_patients)


return(gp_list_45over)

}



get_over45_perc <- function(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)
{gp_over45_perc <- gp_reg_pat_prac_sing_age_male |>
    rbind(gp_reg_pat_prac_sing_age_female)|>
    filter(age != "ALL") |>
    group_by(org_code,age) |>
    summarise(number_of_patients=sum(number_of_patients)) |>
    dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                           .default = as.numeric(age))
    )) |>
    mutate(over_45=case_when(age>= 45 ~1,
                             .default = 0)) |>
    group_by(org_code,over_45)|>
    summarise(number_of_patients=sum(number_of_patients))|>
    group_by(org_code) |>
    mutate(total_patients=sum(number_of_patients))|>
    ungroup() |>
    mutate(perc=(number_of_patients/total_patients)*100)|>
    filter(over_45==1) |>
    select(org_code,perc)|>
    rename(practice_code=org_code,perc_over45=perc)


return(gp_over45_perc)

}

get_gp_16andover_pop <- function(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)
{gp_16andover_pop <- gp_reg_pat_prac_sing_age_male |>
  rbind(gp_reg_pat_prac_sing_age_female)|>
  filter(age != "ALL") |>
  group_by(org_code,age) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  dplyr::mutate(age=as.numeric(case_when(age=="95+" ~ 95,
                                         .default = as.numeric(age))
  )) |>
  filter(as.numeric(age) >= 16) |>
  group_by(org_code) |>
  summarise(number_of_patients16andover=sum(number_of_patients)) |>
  ungroup() |>
  select(org_code,number_of_patients16andover)|>
  rename(practice_code=org_code)

return(gp_16andover_pop)

}

get_allage_list <- function(gp_reg_pat_prac_sing_age_male,gp_reg_pat_prac_sing_age_female)
{gp_list_allage <- gp_reg_pat_prac_sing_age_male |>
  rbind(gp_reg_pat_prac_sing_age_female)|>
  filter(age == "ALL") |>
  group_by(org_code) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  ungroup() |>
  select(org_code,number_of_patients)|>
  rename(practice_code=org_code)

return(gp_list_allage)
}


join_over45_to_gp_lsoa_with_eth_sum <- function(gp_lsoa_with_eth_sum,gp_over45_perc){
  gp_lsoa_with_eth_sum_over45perc <- gp_lsoa_with_eth_sum |>
    left_join(gp_over45_perc)
  return(gp_lsoa_with_eth_sum_over45perc)
}

show_in_excel <- function(.data){
  
  tmp <- paste0(tempfile(), ".csv")
  
  write.csv(.data, tmp)
  
  fs::file_show(path = tmp)
}
