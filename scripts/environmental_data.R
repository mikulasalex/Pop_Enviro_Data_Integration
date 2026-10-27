
# -- Stanford ECHOLab ---- ----
# ---- Set parameters ----
  # write filepaths
    echo_10km_path <- c("~/Desktop/Alex_Work_LargeFiles/PAA_VirtWorkshop/data/raw/echo_lab/10km_grid/smokePM2pt5_predictions_daily_10km_20060101-20231231.rds")
    echo_10km_grid <- c("~/Desktop/Alex_Work_LargeFiles/PAA_VirtWorkshop/data/raw/echo_lab/10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")
    echo_county_path <- c("~/Desktop/Alex_Work_LargeFiles/PAA_VirtWorkshop/data/raw/echo_lab/county/smokePM2pt5_predictions_daily_county_20060101-20231231.rds")
    echo_tract_path <- c("~/Desktop/Alex_Work_LargeFiles/PAA_VirtWorkshop/data/raw/echo_lab/tract/smokePM2pt5_predictions_daily_tract_20060101-20231231.rds")

    smoke_data <- c("~/Desktop/Alex_Work_LargeFiles/PAA_VirtWorkshop/data/raw/smoke/hms_smoke20261012.shp")
    
    
  # prepare analysis parameters
    
    master_crs <- st_crs(sf_county_2010)
    
    all_dates = seq.Date(ymd("2013-12-01"), ymd("2023-12-31"), by = "day")
    
    start_date <- ymd("2013-12-01")
    end_date   <- ymd("2023-12-31")
    
    study_counties <- sf_county_2010$GEOID
    study_tracts <- sf_tract_2010$GEOID
    

# ---- Plot example of smoke plume ----
    plumes <- read_sf(smoke_data)
    ggplot() +
      geom_sf(data = plumes, color = "black") +
      geom_sf(data = sf_county_2010, color = "blue", fill = NA) 

# ---- gridded 10km data ---- ----
  # Load smokePM2.5 predictions on smoke days
    preds <- readRDS(echo_10km_path)
    str(preds)

# ------ Reduce to study date dimensions ----
  # reduce raw data to years needed
    preds <- preds |> 
      filter(date %within% interval(start_date, end_date))
    
  # free memory
    gc()
    
# ------ Reduce to study space dimensions ----
  # Load 10 km grid
    grid_10km = read_sf(echo_10km_grid)
    
  # view & confirm grid CRS
    st_crs(grid_10km)
    st_crs(grid_10km) == master_crs
    
  # reproject to master_crs
    grid_10km <- st_transform(grid_10km, master_crs)
    st_crs(grid_10km) == master_crs
    
  # collapse WI counties to one border
    sf_wi <- sf_county_2010 |> 
      mutate(state = "WI") |> 
      group_by(state) |> 
        summarise(geometry = st_union(geometry)) |> 
      ungroup()
    
  # confirm collapse
    ggplot(sf_wi) +
      geom_sf()
    
  # reduce size of grid to just WI
    temp <- st_intersection(grid_10km, sf_wi)
    
    sf_wi_10km <- grid_10km |> 
      filter(ID %in% temp$ID)
    
  # plot to confirm
    ggplot(sf_wi_10km) +
      geom_sf()
    
    ggplot() +
      geom_sf(data = sf_wi_10km, color = "forestgreen", fill = NA) +
      geom_sf(data = sf_wi, color = "blue", fill = NA)
    
  # remove full grid database
    rm(grid_10km,
       temp)
    gc()
    
    
    
# ------ Generate Data Frame with grid ID and Smoke Data for all dates ---- 
    
  # Get full combination of grid cell-days
  # Warning: this may require a large amount of memory if multiple years and states
    days_by_10km = expand.grid(grid_id_10km = sf_wi_10km$ID, date = all_dates)
    
  # Match smoke predictions on smoke days to grid cell-days
    smoke_10km = left_join(days_by_10km, preds, by = c("grid_id_10km", "date"))
    
  # Predict 0 for remaining grid cell-days, which are non-smoke days
    smoke_10km = mutate(smoke_10km, smokePM_pred = replace_na(smokePM_pred, 0))
    
  # generate helper variables
    smoke_10km <- smoke_10km |> 
      arrange(grid_id_10km, date) |> 
      mutate(year = year(date),
             prior_30 = (date - 30),
             prior_7 = (date - 7),
             window_30day = interval(start = prior_30, end = date),
             window_7day = interval(start = prior_7, end = date))


  # clean up environment
    rm(days_by_10km, 
       preds)
  
# ---- County data ---- ----
  # Load smokePM2.5 predictions on smoke days
    smoke_county <- readRDS(echo_county_path)
    str(smoke_county)

# ------ Reduce to study date dimensions ----
  # reduce raw data to years needed
    smoke_county <- smoke_county |> 
      filter(date %within% interval(start_date, end_date))
    
  # free memory
    gc()
    
# ------ Reduce to study space dimensions ----
  # filter to only counties in WI using FIPS codes
    smoke_county <- smoke_county |> 
      filter(GEOID %in% study_counties)
    
 
# ------ Generate county data with county GEOID and Smoke Data for all dates ---- 
  # Get full combination of county/dates
  # Warning: this may require a large amount of memory if multiple years and states
    county_days = expand.grid(GEOID = sf_county_2010$GEOID, date = all_dates)
    
  # Match smoke predictions on smoke days to county/date
    smoke_county = left_join(county_days, smoke_county, by = c("GEOID", "date"))
    
  # Predict 0 for NA county/date days, which are non-smoke days
    smoke_county = mutate(smoke_county, smokePM_pred = replace_na(smokePM_pred, 0))
    
  # generate helper variables
    smoke_county <- smoke_county |> 
      arrange(GEOID, date) |> 
      mutate(year = year(date),
             prior_30 = (date - 30),
             prior_7 = (date - 7),
             window_30day = interval(start = prior_30, end = date),
             window_7day = interval(start = prior_7, end = date))
    
    
  # clean environment
    rm(county_days)
    gc()

        
# ---- Tract data ---- ----
  # Load smokePM2.5 predictions on smoke days
    smoke_tract <- readRDS(echo_tract_path)
    str(smoke_tract)
    
# ------ Reduce to study date dimensions ----
  # reduce raw data to years needed
    smoke_tract <- smoke_tract |> 
      filter(date %within% interval(start_date, end_date))
    
  # free memory
    gc()
    
# ------ Reduce to study space dimensions ----
  # filter to only tracts in WI using FIPS codes
    smoke_tract <- smoke_tract |> 
      filter(GEOID %in% study_tracts)
    
    gc()
    
    
# ------ Generate tract data with tractGEOID and Smoke Data for all dates ---- 
  # Get full combination of tract/dates
  # Warning: this may require a large amount of memory if multiple years and states
    tract_days = expand.grid(GEOID = sf_tract_2010$GEOID, date = all_dates)
    
  # Match smoke predictions on smoke days to tract/date
    smoke_tract = left_join(tract_days, smoke_tract, by = c("GEOID", "date"))
    
  # Predict 0 for NA tract/date days, which are non-smoke days
    smoke_tract = mutate(smoke_tract, smokePM_pred = replace_na(smokePM_pred, 0))
    
  # generate helper variables
    smoke_tract <- smoke_tract |> 
      arrange(GEOID, date) |> 
      mutate(year = year(date),
             prior_30 = (date - 30),
             prior_7 = (date - 7),
             window_30day = interval(start = prior_30, end = date),
             window_7day = interval(start = prior_7, end = date))
    

  # clean environment 
    rm(tract_days)
    gc()
    

# ---- Append Geometry ----
  # append geometry
    sf_smoke_10km <- right_join(sf_wi_10km, smoke_10km, by = c("ID" = "grid_id_10km"))
    sf_smoke_county <- right_join(sf_county_2010, smoke_county, by = c("GEOID" = "GEOID"))
    sf_smoke_tract <- right_join(sf_tract_2010, smoke_tract, by = c("GEOID" = "GEOID"))
    
# ---- Vector to Raster ----









