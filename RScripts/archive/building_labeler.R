library(tidyverse)
library(sf)

map = read_sf("data/Shapefiles 2/Tanganya_buildings.shp")
st_crs(map) <- 4326

pnts <- read.csv("data/Trap_Data/Clean_Both_Data_By_Trap.csv")

pnts_sf <- st_as_sf(pnts, coords = c('Longitude', 'Latitude'), crs = st_crs(map))

pnts <- pnts_sf |> mutate(intersects = st_intersects(geometry, map) |> unlist())

,
                       building_type = map$Type[intersects])

pnts
