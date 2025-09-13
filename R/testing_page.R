library(sf)
library(terra)
library(tidyverse)
library(haven)



nso_polygon <- st_read("C:/Users/luhan/Documents/office/2018/shapefiles/ea_data/2018_MPHC_EAs_Final.shp")

colnames(nso_polygon) <- tolower(colnames(nso_polygon))


nso_polygon <- nso_polygon %>%
  filter(ta_code == 30306)



nso_points <- read_dta("C:/Users/luhan/Documents/GitHub/census_listing/cm_field_summaries/data/household.dta")

nso_points <- nso_points %>%
  filter(!is.na(hh_longitude) | !is.na(hh_latitude))

# Convert to WGS84 (GPS coordinates, EPSG:4326)
nso_points <- st_as_sf(nso_points, coords = c("hh_longitude", "hh_latitude"), crs = 4326)



