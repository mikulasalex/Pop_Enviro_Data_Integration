# Import Population Data ---- ----
# -- TidyCensus ---- ----
# ---- Setup ----
  # get a Census API Key here: https://api.census.gov/data/key_signup.html  
  
  # set Census API key
    # census_api_key("your_census_api_key_here", 
    #                install = TRUE,
    #                overwrite = TRUE)
    
  # view available ACS data
    acs_2020 <- load_variables(2020, "acs5", cache = TRUE)
    
# ---- Import one year of ACS Data ----
  # import 2020
    my_df <- get_acs(geography = "tract",      # level of aggregation
                     state = "WI",             # restrict to certain state
                     year = 2020,              # year of data
                     variables = "B17020_002", # variable name (poverty status)
                     geometry = TRUE)          # include geometries
    
# ---- Make loop objects ----
  # make vector of variables
    acs_var_list <- c("B01003_001", # tot_pop
                      "B25064_001", # med_gross_rent
                      "B17020_002", # poverty status
                      "B19013_001") # med_hh_income
    
  # make vector of desired years
    years <- c(2014:2023)
    
  # make vector of desired geographies
    geographies <- c("county", "tract", "cbg")
    
  # make vector of block group counties to reduce data size
    cbg_filter <- c("55027", "55131", "55089",
                    "55025", "55055", "55133", "55079",
                    "55105", "55127", "55101", "55059")
  
  # make empty lists for each geography
    acs_county <- list()
    acs_tract <- list()
    acs_cbg <- list()
      
# ---- Run for loop ----
    for (g_i in geographies) {
      for (y_i in years) {
        
      # values to name objects within loop iteration
        geog_value <- g_i
        year_value <- as.character(y_i)
        
      # pull data from API by year and geography
        temp <- get_acs(geography = g_i,
                        state = "WI",
                        year = y_i,
                        variables = acs_var_list,
                        geometry = FALSE)
        
      # make helper vars and geo identifiers
        temp <- temp |> 
          mutate(county_fips = str_sub(GEOID, 1, 5)) |> 
          mutate(geog_level = geog_value) |> 
          mutate(year = y_i)
        
      # if cbg, then filter to certain counties
        temp <- temp |> 
          filter(if (geog_value == "cbg") county_fips %in% cbg_filter else year > 0)
        
      # drop margin or error variable and long name
        temp <- temp |>
          dplyr::select(-c(moe, NAME)) 
        
      # pivot wider
        temp <- temp |> 
          tidyr::pivot_wider(names_from = variable, values_from = estimate) 
      
      # rename ACS table vars
        temp <- temp |> 
          rename(tot_pop = B01003_001,
                 med_gross_rent = B25064_001,
                 pov_status = B17020_002,
                 med_hh_income = B19013_001)
          
      # append data to correct list based on geography
        if (geog_value == "county") acs_county[[paste0(geog_value,"_",year_value)]] <- temp
        if (geog_value == "tract") acs_tract[[paste0(geog_value,"_",year_value)]] <- temp
        if (geog_value == "cbg") acs_cbg[[paste0(geog_value,"_",year_value)]] <- temp
        
      # clean up environment
        rm(geog_value, year_value)
    }}
      

# ---- Clean up Environment ----
    rm(temp, acs_2020)
    rm(echo_10km_grid, echo_10km_path, echo_county_path, echo_tract_path)
    rm(g_i, y_i, geographies, years, acs_var_list)
    
# ---- Import Shapefiles ----
  # import for county, tract, and block group
  # drop any superfluous columns
  # make fips IDs
    
  # county
    sf_county_2010 <- get_acs(geography = "county",
                              state = "WI",
                              year = 2010,
                              variables = "B01003_001",
                              geometry = TRUE)
    
    sf_county_2010 <- sf_county_2010 |> 
      dplyr::select(GEOID) |> 
      mutate(county_fips = GEOID,
             geo_year = 2010)
    
  # tract
    sf_tract_2010 <- get_acs(geography = "tract",
                             state = "WI",
                             year = 2010,
                             variables = "B01003_001",
                             geometry = TRUE)
    
    sf_tract_2010 <- sf_tract_2010 |> 
      dplyr::select(GEOID) |> 
      mutate(county_fips = str_sub(GEOID, 1, 5)) |> 
      mutate(tract_fips = GEOID,
             geo_year = 2010)
    
    
  # cbg
    sf_cbg_2010 <- get_acs(geography = "cbg",
                           state = "WI",
                           year = 2015, # still uses 2010 boundaries
                           variables = "B01003_001",
                           geometry = TRUE)
    
    sf_cbg_2010 <- sf_cbg_2010 |> 
      dplyr::select(GEOID) |> 
      mutate(county_fips = str_sub(GEOID, 1, 5)) |> 
      mutate(tract_fips = str_sub(GEOID, 1, 11)) |> 
      mutate(cbg_fips = GEOID,
             geo_year = 2010) |> 
      filter(county_fips %in% cbg_filter)
    
  # confirm all CRS are the same for all shapefiles
    st_crs(sf_county_2010) == st_crs(sf_tract_2010)
    st_crs(sf_county_2010) == st_crs(sf_cbg_2010)
    st_crs(sf_tract_2010) == st_crs(sf_cbg_2010)
    
  # view CRS metadata
    st_crs(sf_county_2010)
    
  # map each level of geography 
    ggplot() +
      geom_sf(data = sf_tract_2010,
              color = "royalblue") +
      geom_sf(data = sf_county_2010,
              color = "black",
              fill = "lightblue",
              linewidth = 0.5) +
      geom_sf(data = sf_cbg_2010,
              color = "gray20",
              fill = "lightyellow",
              linewidth = 0.5) +
      labs(title = "Wisconsin Census Boundaries",
           subtitle = "Counties (black)  | tracts (blue)  | block groups (yellow)") +
      theme_minimal()
    
# ---- Simplify vector and line data ----
  # simplify county     
    sf_county_2010 <- ms_simplify(
      sf_county_2010, # sf object to simplify
      keep = 0.05, # proportion of vertices to retain
      snap = TRUE, # snap vertices at small distances to fix misallignment
      keep_shapes = TRUE) # prevent small shapes from disappearing (islands)
    
  # simplify tract
    sf_tract_2010 <- ms_simplify(
      sf_tract_2010, # sf object to simplify
      keep = 0.05, # proportion of vertices to retain
      snap = TRUE, # snap vertices at small distances to fix misallignment
      keep_shapes = TRUE) # prevent small shapes from disappearing (islands)
    
  # simplify cbg
    sf_cbg_2010 <- ms_simplify(
      sf_cbg_2010, # sf object to simplify
      keep = 0.05, # proportion of vertices to retain
      snap = TRUE, # snap vertices at small distances to fix misallignment
      keep_shapes = TRUE) # prevent small shapes from disappearing (islands)
    
  # map simplified geometries 
    ggplot() +
      geom_sf(data = sf_tract_2010,
              color = "royalblue") +
      geom_sf(data = sf_county_2010,
              color = "black",
              fill = "lightblue",
              linewidth = 0.5) +
      geom_sf(data = sf_cbg_2010,
              color = "gray20",
              fill = "lightyellow",
              linewidth = 0.5) +
      labs(title = "Wisconsin Census Boundaries",
           subtitle = "Counties (black)  | tracts (blue)  | block groups (yellow)") +
      theme_minimal()
    
  
  # check to see if each geometry value in sf_county_2010 is valid
    st_is_valid(sf_county_2010, reason = TRUE)
    
  # logical check that every geometry value validation is "Valid Geometry"
  # avoids masive console output
    all((st_is_valid(sf_county_2010, reason = TRUE)) == "Valid Geometry")
    all((st_is_valid(sf_tract_2010, reason = TRUE)) == "Valid Geometry")
    all((st_is_valid(sf_cbg_2010, reason = TRUE)) == "Valid Geometry")
    
# ---- Save ----
  # save
    save(acs_cbg, acs_county, acs_tract, 
         sf_county_2010, sf_county_2020,
         sf_tract_2010, sf_tract_2020,
         sf_cbg_2010, sf_cbg_2020,
         file = here("data","clean","tidycensus","acs_pop_data.RData"))
    
    
    
    
    
    
    
    
    