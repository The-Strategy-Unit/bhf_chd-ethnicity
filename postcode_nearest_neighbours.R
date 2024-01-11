library(tidyverse)
library(sf)
library(tidygeocoder)
library(leaflet)

postcodes <- tribble(
  ~postcode, ~type,
  "WS7 3AE", "home",
  "WS7 3XH", "gp",
  "WS7 3XE", "gp",
  "WS11 9SE", "gp",
  "WS7 1AQ", "gp",
  "WS7 2EY", "gp"
) |>
  mutate(id = row_number()) |>
  geocode(postalcode = postcode) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# nothing more than a check that the points look valid
leaflet(postcodes) |>
  addTiles() |>
  addMarkers()


homes <- postcodes |>
  filter(type == "home")

gps <- postcodes |>
  filter(type == "gp")

homes |>
  select(home_postcode = postcode) |>
  st_join(gps, st_nearest_feature)

homes |>
  select(home_postcode = postcode) |>
  # finds all the gp's in 1.5 km
  st_join(gps, st_is_within_distance, 1500)

# one thing to note, you will see in both of these examples we end up keeping
# geometry from home

# something like this can get back the gp geometry

homes |>
  select(home_postcode = postcode) |>
  # finds all the gp's in 1.5 km
  st_join(gps, st_is_within_distance, 1500) |>
  st_drop_geometry() |>
  select(id, home_postcode) |>
  inner_join(x = gps, by = join_by("id")) # note, we flip the order of the join