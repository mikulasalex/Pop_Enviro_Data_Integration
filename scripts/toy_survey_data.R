# Generate Toy Survey Data ---- ----
    set.seed(2024)
    n <- 1000
    
# -- Parameters for data generation ----
  # respondent-level attributes ----
    respondents <- tibble(
      respondent_ID = sprintf("R%04d", 1:n),
      sex           = sample(c("Female", "Male"), n, replace = TRUE),
      race          = sample(c("White", "Black", "Asian", "Other"), n,
                             replace = TRUE, prob = c(0.62, 0.18, 0.08, 0.12)),
      date_of_birth = sample(seq(as.Date("1940-01-01"), as.Date("1970-12-31"), by = "day"),
                             n, replace = TRUE),
      person_effect = rnorm(n)   # stable individual propensity, drives within-person correlation
    )
    
  # survey wave date windows
    waves <- tibble(
      survey_wave = 1:3,
      wave_start  = as.Date(c("2015-05-01", "2018-05-01", "2023-05-01")),
      wave_end    = as.Date(c("2015-09-30", "2018-09-20", "2023-09-30")))
    
# -- Expand respondent data 3x based on survey wave dates ----
  # cross respondents with survey windows
  # generate outcome variables
    survey_df <- respondents |>
      crossing(waves) |>
      rowwise() |>
      mutate(survey_date = sample(seq(wave_start, wave_end, by = "day"), 1)) |>
      ungroup() |>
      mutate(age_at_survey = as.numeric(survey_date - date_of_birth) / 365.25,
           # latent severity: person effect + age + sex + wave-specific noise
             latent = 0.8 * person_effect +
               0.02 * (age_at_survey - 60) +
               if_else(sex == "Female", 0.2, 0) +
               rnorm(n(), 0, 0.5),
           # asthma severity based on latent
             asthma_severity = cut(latent,
                                   breaks = c(-Inf, quantile(latent, c(0.50, 0.80, 0.95)), Inf),
                                   labels = c("None", "Mild", "Moderate", "Severe"),
                                   ordered_result = TRUE),
           # probability of a hospital stay in the past 30 days rises with severity
             p_hsp = case_when(asthma_severity == "None"     ~ 0.01,
                               asthma_severity == "Mild"     ~ 0.03,
                               asthma_severity == "Moderate" ~ 0.10,
                               asthma_severity == "Severe"   ~ 0.30),
           # hospital visit in lat 30 days? 0/1
             hsp_30_days = rbinom(n(), 1, p_hsp),
           # date of hospital visit in last 30 days
             hsp_date = case_when(hsp_30_days == 1 ~ (survey_date - (sample(1:30, 1, replace = TRUE))),
                                  hsp_30_days == 0 ~ NA),
           # generic continuous outcome variable
             outcome_cont = rnorm(n(), mean = 40, sd = 10)) |>
      arrange(respondent_ID, survey_wave) |>
      dplyr::select(respondent_ID, sex, race, date_of_birth,
                    survey_wave, survey_date, asthma_severity, 
                    hsp_30_days, hsp_date, outcome_cont)
    
# -- Assign geographies to respondents ----
  # Assign each respondent to one tract 
  # assignment probability proportional to tract population
    wi_tracts <- acs_tract[["tract_2019"]] # some tract shapefile 
    
    tract_assign <- respondents |>
      distinct(respondent_ID) |>
      mutate(tract_fips = sample(wi_tracts$GEOID,
                                 size    = n(),
                                 replace = TRUE,
                                 prob    = wi_tracts$tot_pop))
    
  # join with tract shapefile to gather geometry ID
    tract_assign <- right_join(sf_tract_2010, tract_assign, by = "tract_fips")
    
  # convert tract geometry to to point-in-polygon
    tract_assign <- st_point_on_surface(tract_assign)
    
  # extract coordinates of point
    coord_matrix <- as.data.frame(st_coordinates(tract_assign))
    
  # append X and Y as a-spatial lat/long variables
    tract_assign <- bind_cols(tract_assign, coord_matrix)
    
  # remove sf class and geometry
    tract_assign <- as.data.frame(tract_assign) |> 
      dplyr::select(-geometry) |> 
      rename(latitude = Y,
             longitude = X)
    
  # merge with long survey data 
  # no mobility, same tract across all 3 waves
    survey_df <- survey_df |>
      left_join(tract_assign, by = "respondent_ID")
    
  # save
    save(survey_df,
         file = here("data","clean","sample_survey","survey_data.RData"))
    
  # clean up environment
    rm(tract_assign,
       respondents, 
       waves,
       coord_matrix,
       wi_tracts,
       n)
    

