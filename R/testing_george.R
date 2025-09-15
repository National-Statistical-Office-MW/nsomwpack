library(sf)
library(terra)
library(tidyverse)
library(haven)



nso_polygon <- st_read("C:/Users/chira/Documents/__CODE/_NSO/nsomwpack/R/data/malemia_modi.shp")

colnames(nso_polygon) <- tolower(colnames(nso_polygon))


nso_polygon <- nso_polygon %>%
  filter(ta_code == 30306)



nso_points <- st_read("C:/Users/chira/Documents/__CODE/_NSO/nsomwpack/R/data/malemia_hh_final.shp")

nso_points <- nso_points %>%
  filter(!is.na(hh_longitu) | !is.na(hh_latitud))

# Convert to WGS84 (GPS coordinates, EPSG:4326)
nso_points <- st_as_sf(nso_points, coords = c("hh_longitu", "hh_latitud"), crs = 4326)




ea_1_points <- nso_points_in_polygon(nso_polygon, nso_points, 1)
