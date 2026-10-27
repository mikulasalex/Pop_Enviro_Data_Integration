

# Use Case #1 - Annual average, county level ----
# -- Data wrangling ----
  # make annual PM2.5 average
    county_annual_avg <- smoke_county |> 
      group_by(GEOID, year) |> 
        arrange(date) |> 
        mutate(annual_pm25_average = mean(smokePM_pred, na.rm = TRUE)) |> 
      ungroup()
  
  # reduce to 1 column per year
    county_annual_avg <- county_annual_avg |> 
      dplyr::select(GEOID, year, annual_pm25_average) |> 
      distinct()
    
  # stack ACS timeseries data
    county_pop_data <- bind_rows(acs_county)
    
  # merge to county by year and GEOID
    county_pop_data <- left_join(county_pop_data, county_annual_avg, by = c("GEOID", "year"))
    
# -- Make data spatial ----
  # right_join master county shapefile
    county_pop_data <- right_join(sf_county_2010, county_pop_data, by = "GEOID")
    
# -- Plot to confirm ----
    ggplot() +
      geom_sf(data = county_pop_data, 
              color = "gray80",
              aes(fill = annual_pm25_average)) +
      geom_sf(data = sf_wi,
              color = "black",
              fill = NA) +
      facet_wrap(~year) +
      theme_void()

    rm(county_annual_avg)

# Use Case #2: Seasonal average, tract level ----
# -- Data Wrangling ----
  # make seasonal average
    spring <- (3:5)
    summer <- (6:8)
    fall <- (9:11)
    winter1 <- (12)
    winter2 <- (1:2)
    
    tract_seasonal_average <- smoke_tract |> 
      mutate(month = month(date),
             season = case_when(month %in% spring ~ "Spring",
                                month %in% summer ~ "Summer",
                                month %in% fall ~ "Fall",
                                month %in% winter1 ~ "Winter",
                                month %in% winter2 ~ "Winter"))
    
    tract_seasonal_average <- tract_seasonal_average |> 
      group_by(GEOID, year, season) |> 
        arrange(date) |> 
        mutate(seasonal_pm25_average = mean(smokePM_pred, na.rm = TRUE)) |> 
        ungroup()
        
  # reduce to 1 row per year/season
    tract_seasonal_average <- tract_seasonal_average |> 
      dplyr::select(GEOID, year, season, seasonal_pm25_average) |> 
      distinct()
      
  # stack ACS timeseries data
    tract_pop_data <- bind_rows(acs_tract)
    
  # merge to tract by year and GEOID
    tract_pop_data <- left_join(tract_pop_data, tract_seasonal_average, by = c("GEOID", "year"))

# -- Make data spatial ----
  # right_join master county shapefile
    tract_pop_data <- right_join(sf_tract_2010, tract_pop_data, by = "GEOID")
    
# -- Plot to confirm ----
    ggplot() +
      geom_sf(data = tract_pop_data, 
              color = "gray80",
              aes(fill = seasonal_pm25_average)) +
      geom_sf(data = sf_wi,
              color = "black",
              fill = NA) +
      facet_wrap(~season) +
      theme_void()
    
  # clean environment
    rm(tract_seasonal_average)
                                
            
    


# Use Case #3: Individual level running averages, survey data ---- 
# -- Data Wrangling ----
# ---- 10km/tract/county running averages/min/max ----
  # NOTE: slider package for running averages
  
  # 10km grid
    smoke_10km <- smoke_10km |> 
      group_by(grid_id_10km) |> 
      arrange(date) |> 
      mutate(mean_30day = slide_dbl(smokePM_pred, mean, .before = 30, .after = 0, .complete = TRUE),
             max_30day = slide_dbl(smokePM_pred, max, .before = 30, .after = 0, .complete = TRUE),
             min_30day = slide_dbl(smokePM_pred, min, .before = 30, .after = 0, .complete = TRUE),
             mean_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE),
             max_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE),
             min_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE)) |> 
      ungroup() |> 
      arrange(grid_id_10km, date)
    
 # tract
    smoke_tract <- smoke_tract |> 
      group_by(GEOID) |> 
      arrange(date) |> 
      mutate(mean_30day = slide_dbl(smokePM_pred, mean, .before = 30, .after = 0, .complete = TRUE),
             max_30day = slide_dbl(smokePM_pred, max, .before = 30, .after = 0, .complete = TRUE),
             min_30day = slide_dbl(smokePM_pred, min, .before = 30, .after = 0, .complete = TRUE),
             mean_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE),
             max_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE),
             min_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE)) |> 
      ungroup() |> 
      arrange(GEOID, date)
    
  # county
    smoke_county <- smoke_county |> 
      group_by(GEOID) |> 
      arrange(date) |> 
      mutate(mean_30day = slide_dbl(smokePM_pred, mean, .before = 30, .after = 0, .complete = TRUE),
             max_30day = slide_dbl(smokePM_pred, max, .before = 30, .after = 0, .complete = TRUE),
             min_30day = slide_dbl(smokePM_pred, min, .before = 30, .after = 0, .complete = TRUE),
             mean_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE),
             max_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE),
             min_7day = slide_dbl(smokePM_pred, mean, .before = 7, .after = 0, .complete = TRUE)) |> 
      ungroup() |> 
      arrange(GEOID, date)
    
    
# -- Make Survey Data Spatial ----
# ---- Long/lat to sf_point data ----
  # convert to sf_points object
    survey_points <- st_as_sf(survey_df,
                              coords = c("longitude", "latitude"),
                              crs = master_crs) 
    
  # plot
    ggplot() +
      geom_sf(data = sf_wi_10km,
              color = "gray80") +
      geom_sf(data = survey_points,
              color = "red",
              size = 0.30) 

# ---- place point in a 10km grid to get grid_ID ----
  # spatial join attributes from grid geometry 
    survey_points <- st_join(survey_points, sf_wi_10km) |> 
      dplyr::select(-COORDY) |> 
      dplyr::select(-COORDX)
    
# ---- Specific match based on survey date and grid id ----
    colnames(smoke_10km)
    colnames(survey_points)
    
    temp <- smoke_10km |> 
      rename(ID = grid_id_10km,
             survey_date = date)
    
    survey_10km <- left_join(survey_points,
                             temp,
                             by = c("ID", "survey_date"))
    colnames(survey_10km)
# ---- Specific match by county-level GEOIDs ---- 
    colnames(smoke_county)
    colnames(survey_df)  
  # county
    temp <- smoke_county |> 
      rename(county_fips = GEOID,
             survey_date = date)
    
    survey_county <- left_join(survey_df,
                               temp,
                               by = c("county_fips", "survey_date"))
    
# ---- Specific match by tract-level GEOIDs ---- 
    colnames(smoke_tract)
    
    temp <- smoke_tract |> 
      rename(tract_fips = GEOID,
             survey_date = date)
    
    survey_tract <- left_join(survey_df,
                              temp,
                              by = c("tract_fips", "survey_date"))
    
    
  # uhoh, oh, we can do this, but only to the 10x10km grid level. This doesn't feel specific enough to me...

# -- Buffer around respondent long/lat ----
# ---- Make buffer polygon ----
  # check for units in survey_points crs
    
    st_crs(survey_points)
    
    survey_buffer <- st_buffer(survey_points, dist = 10000)
    
    ggplot() + 
      geom_sf(data = sf_wi_10km,
              color = "gray80",
              fill = NA) +
      geom_sf(data = survey_buffer,
              color = "red") +
      geom_sf(data = survey_points,
              color = "blue") +
      theme_void()
    
# ---- All averages for 10km grids overlapping buffer ----
# ------ For Loop over each date ----
    
  # list object
    survey_list <- list()
    
  # list of dates in survey data
    some_dates <- survey_df |> 
      dplyr::select(survey_date) |> 
      group_by(survey_date) |> 
        slice(1) |> 
      ungroup()
    
    some_dates <- c(some_dates$survey_date)
  
  # replace 3 with some number to sample smaller range of dates
    #some_dates <- sample(some_dates$survey_date, 3, replace = FALSE)
    
  # list of variables to merge
    smoke_vars <- c("smokePM_pred",
                    "mean_30day",
                    "mean_7day")

    
    for (d_i in some_dates) {
      
      date_name <- c(d_i)
    
    # filter survey data
      survey_temp <- survey_buffer |> 
        filter(survey_date == d_i)
      
    # skip to next date if no records exist
      if (nrow(survey_temp) == 0) next
      
    # filter smoke_10km data
      smoke_temp <- smoke_10km |> 
        filter(date == d_i) |> 
        rename(ID = grid_id_10km)
      
    # make smoke_temp spatial
      smoke_temp <- right_join(sf_wi_10km, smoke_temp, by = "ID") |> 
        dplyr::select(smoke_vars)
      
    # use_st_interpolate_aw() function
      temp <- st_interpolate_aw(smoke_temp, survey_temp, extensive = FALSE)
      
    # append interpolated columns
      survey_temp$pm25_w_avg <- temp$smokePM_pred
      survey_temp$mean_30day_w_avg <- temp$mean_30day
      survey_temp$mean_7day_w_avg <- temp$mean_7day

    # append to list
      survey_list[[paste0(d_i,"weighted_avg")]] <- survey_temp
      
    # clean up
      rm(survey_temp,
         temp,
         smoke_temp)
      
    }
      
# ------ Stack interpolated dataframe ----
    survey_buffer <- bind_rows(survey_list)
    
# -- Compare Point-level differences ----
# ---- Combine all smokePM_pred values into one df ----
    
  # make our final survey dataset
    survey_final <- as.data.frame(survey_buffer)
      
  # grid to final
    temp <- as.data.frame(survey_10km) |>  
      dplyr::select(respondent_ID, survey_wave, smoke_vars) |> 
      rename(pm25_grid = smokePM_pred,
             mean_30day_grid = mean_30day,
             mean_7day_grid = mean_7day)
    
    survey_final <- left_join(survey_final, temp, by = c("respondent_ID", "survey_wave"))
    
  # tract to final
    temp <- as.data.frame(survey_tract) |>  
      dplyr::select(respondent_ID, survey_wave, smoke_vars) |> 
      rename(pm25_tract = smokePM_pred,
             mean_30day_tract = mean_30day,
             mean_7day_tract = mean_7day)
    
    survey_final <- left_join(survey_final, temp, by = c("respondent_ID", "survey_wave"))
    
  # county to final
    temp <- as.data.frame(survey_county) |> 
      dplyr::select(respondent_ID, survey_wave, smoke_vars) |> 
      rename(pm25_county = smokePM_pred,
             mean_30day_county = mean_30day,
             mean_7day_county = mean_7day)
    
    survey_final <- left_join(survey_final, temp, by = c("respondent_ID", "survey_wave"))
    


    
      
# Process Raster Data ---- ----
# -- Simple example ----
# -- exactextractr example ----

   
      
      