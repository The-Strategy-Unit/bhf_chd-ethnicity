library(targets)
library(tidyverse)
library(sf)
library(tidygeocoder)
library(leaflet)


metric1 <- tar_read(metric1) |> as_tibble()
gp_history <- tar_read(gp_history) |> as_tibble()
gp_reg_pat_prac_lsoa <- tar_read(gp_reg_pat_prac_lsoa) |> as_tibble()
gp_history_short <- tar_read(gp_history_short) |> as_tibble()
joined_gp_history_and_chd_prev <- tar_read(joined_gp_history_and_chd_prev) |> as_tibble()
gp_list_summary <- tar_read(gp_list_summary) |> as_tibble()
gp_geocoded <- tar_read(gp_geocoded) |> as_tibble()

gp_list_summary <- gp_reg_pat_prac_lsoa |> 
  filter(sex=="ALL") |>
  group_by(practice_code,practice_name) |>
  summarise(number_of_patients=sum(number_of_patients)) |>
  rename(org_code=practice_code)

gp_history_short <- gp_history |> select(org_code, name, postcode,open_date,close_date,join_date,left_date)

gp_list_summary <- gp_list_summary |> left_join(gp_history_short)

orig <- metric1 |> select(AreaCode,AreaName, Value, IsCurrent) |>
   rename(org_code=AreaCode) |>
   left_join(gp_history_short)

#prevalence is in orig 

#join the orig and the gp list summary

joined <- gp_list_summary |>
  left_join(orig) |>
#where there is no matching value from orig in the gp list summary then flag these 
mutate(has_orig_prev=case_when(Value>=0 ~ 1, .default =  0))

# pull those without a prev into their own table and geocode 

# missing_prev <- joined |> filter(has_orig_prev==0) |>
#   select(org_code,practice_name,postcode) |>
#   mutate(id = row_number()) |>
#   geocode(postalcode = postcode) |>
#   st_as_sf(coords = c("long", "lat"), crs = 4326,na.fail=FALSE)

# nothing more than a check that the points look valid
# leaflet(missing_prev) |>
#   addTiles() |>
#   addMarkers()

#find all the practices with a prevalence and geocode
# orig_with_prev <- joined |>
#   filter(has_orig_prev==1) |>
#   select(org_code,practice_name,postcode) |>
#   mutate(id = row_number()) |>
#   geocode(postalcode = postcode) |>
#   st_as_sf(coords = c("long", "lat"), crs = 4326,na.fail=FALSE)

#geocode
orig_with_geocode <- joined |>
  select(org_code,practice_name,postcode,has_orig_prev) |>
  mutate(id = row_number()) |>
  geocode(postalcode = postcode) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326,na.fail=FALSE)

saveRDS(orig_with_geocode,"data/orig_with_geocode.rds")

orig_with_geocode_and_id <- gp_geocoded |>
  select(org_code,practice_name,postcode,has_orig_prev,geometry) |>
  mutate(id=row_number())

######
gp_geocoded <- tar_read(gp_geocoded) |> as_tibble()
gp_geocoded <- gp_geocoded |> select(-id)
#######

no_prev <- orig_with_geocode_and_id |>
  filter(has_orig_prev == 0)

with_prev <- orig_with_geocode_and_id |>
  filter(has_orig_prev == 1)

st_sf(no_prev) |>
  select(no_prev_postcode = postcode) |>
  st_join(st_sf(with_prev), st_nearest_feature)

no_prev_with_neighbours <-  st_sf(no_prev) |>
  select(no_prev_postcode = postcode,no_prev_org_code=org_code) |>
  # finds all the gp's in 1.5 km
  st_join(st_sf(with_prev), st_is_within_distance, 1500)

no_prev_with_neighbours_and_neighbours_prev <- no_prev_with_neighbours |>
  left_join(orig, join_by(org_code)) |>
  group_by(no_prev_org_code) |>
  summarise(prev_est=mean(Value)) |>
  st_drop_geometry(select(no_prev_org_code,prev_est))

#127 estimates out of 156 so just 29 practices missing *******

metric1_updated <- joined |>
  left_join(no_prev_with_neighbours_and_neighbours_prev,join_by(org_code==no_prev_org_code)) |>
  mutate(chd_prev_to_use = case_when(Value>=0 ~ Value,
                                    .default = prev_est)
  ) |>
  select(org_code,chd_prev_to_use)

still_missing_prev <- full_list |>
  filter(is.na(chd_pre_to_use)==TRUE)



get_missing_chd_prevalence <- function(metric1,gp_history_short,joined_gp_history_and_chd_prev,gp_list_summary,gp_geocoded){
  
  
  joined <- gp_list_summary |>
    left_join(joined_gp_history_and_chd_prev) |>
    #where there is no matching value from orig in the gp list summary then flag these 
    mutate(has_orig_prev=case_when(Value>=0 ~ 1, .default =  0))
  
  orig_with_geocode_and_id <- gp_geocoded |>
    select(org_code,practice_name,postcode,has_orig_prev,geometry) |>
    mutate(id=row_number())
  
  no_prev <- orig_with_geocode_and_id |>
    filter(has_orig_prev == 0)
  
  with_prev <- orig_with_geocode_and_id |>
    filter(has_orig_prev == 1)
  
  no_prev |>
    select(no_prev_postcode = postcode) |>
    st_join(with_prev, st_nearest_feature)
  
  no_prev_with_neighbours <-  st_sf(no_prev) |>
    select(no_prev_postcode = postcode,no_prev_org_code=org_code) |>
    # finds all the gp's in 1.5 km
    st_join(st_sf(with_prev), st_is_within_distance, 1500)
  
  no_prev_with_neighbours_and_neighbours_prev <- no_prev_with_neighbours |>
    left_join(joined, join_by(org_code)) |>
    group_by(no_prev_org_code) |>
    summarise(prev_est=mean(Value)) |>
    st_drop_geometry(select(no_prev_org_code,prev_est))
  
  #xxxx estimates out of xxx so just xx practices missing *******
  

  metric1_updated <- joined |>
    left_join(no_prev_with_neighbours_and_neighbours_prev,join_by(org_code==no_prev_org_code)) |>
    mutate(chd_prev_to_use = case_when(Value>=0 ~ Value,
                                       .default = prev_est)
    ) |>
    select(org_code,chd_prev_to_use)
  
  return(metric1_updated)
}

