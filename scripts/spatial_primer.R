# ------------------------------------------------------------------------- ----
# Script: spatial_primer ----
# Project: Population And Environment Data, PAA Virtual Workshop ----
# Author: Dr. Alex Mikulas (CU-Boulder, CACHE) ----
# Script Start Date: Oct. 13, 2026 ----
# ------------------------------------------------------------------------- ----
# ---- Scope of Script ---- ----
# This script produces the shapefiles needed for Part 1 of the workshop.
# ------------------------------------------------------------------------- ----


# Spatial Primer Script ---- ----
# -- Import sample geometries ----
  # world
    world <- spData::world
    st_crs(world)
    
  # states
    us_states <- states(cb = TRUE)
  
  # Wisconsin  
    wi <- us_states |> 
      filter(NAME == "Wisconsin")
    
  # WI counties
    wi_counties <- counties(state = 55, cb = TRUE) 
    
    ggplot() +
      geom_sf(data = wi_counties) +
      geom_sf_text(data = wi_counties,
                   aes(label = COUNTYFP))

  # ashland/Bayfield counties
    ash_bay_counties <- wi_counties |> 
      filter(COUNTYFP == "007" | COUNTYFP == "003")
    
  # primary roads
    roads <- primary_roads()
    
  # reduce roads to WI
    temp <- st_intersection(roads, wi)
    wi_roads <- roads |> 
      filter(LINEARID %in% temp$LINEARID)
    
  # WI landmarks/airports
    wi_airports <- landmarks(state = 55, type = "point") |> 
      filter(str_ends(FULLNAME, "Arprt"))
    
# ---- Import 10km grid ----
  # set filepath and read shpfile
    echo_10km_grid <- here("data", "raw", "echo_lab", "10km_grid_wgs84", "10km_grid_wgs84.shp")
    grid_10km = read_sf(echo_10km_grid)
    
  # re-project to master_crs
    grid_10km <- st_transform(grid_10km, st_crs(wi))
    
  # reduce size of grid to just WI
    temp <- st_intersection(grid_10km, wi)
    wi_10km <- grid_10km |> 
      filter(ID %in% temp$ID)
    
  # mutate a new variable correlated with latitude for mapping example
  # set correlation level
    target_corr <- 0.82
  
    wi_10km <- wi_10km %>%
      mutate(
      # generate random standard normal noise
        noise = rnorm(n()), 
      # standardize latitude to have a mean of 0 and sd of 1
        std_existing = scale(COORDY)[, 1],
      # combine them to create the target correlation
        std_combined = target_corr * std_existing + sqrt(1 - target_corr^2) * noise,
      # rescale to your target mean (40) and standard deviation (10)
        attribute_1 = 40 + 10 * std_combined) %>%
      # Clean up helper columns
      dplyr::select(-noise, -std_existing, -std_combined)
    
# -- Clean up ----
    rm(world, us_states, roads, temp, echo_10km_grid, grid_10km, target_corr)
