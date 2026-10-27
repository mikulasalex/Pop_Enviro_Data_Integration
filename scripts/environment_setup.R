# ------------------------------------------------------------------------- ----
# Script: environment_setup ----
# Project: Population And Environment Data, PAA Virtual Workshop ----
# Author: Dr. Alex Mikulas (CU-Boulder, CACHE) ----
# Script Start Date: Oct. 13, 2026 ----
# ------------------------------------------------------------------------- ----
# ---- Scope of Script ---- ----
  # This script establish the working environment. It...
  # 1) installs packages necessary for project,
  # 2) establishes a working directory for the RProject folder, 
  # 3) and creates directory folders if they do not already exist. 
# ------------------------------------------------------------------------- ----
#  ---- Instructions ---- ----
  # Run this script prior to attending the workshop with an internet connection. 
  # It will download the required R package libraries (small chunks of 
  # software that R and RStudio use for analysis and data management) that are 
  # used in the workshop. 
  
  # To run the code in this document, highlight all of the ext in the document and 
  # click the "[-]>Run" button in the top right corner of this window pane.

# ---- Environment Setup ---- ----
# 1.0 - Install Package Manager ---- ----
  # "pacman" is a packacke that easily installs, updates, and loads packages
  # into your R environment.

  # install pacman package
    install.packages("pacman")
    library(pacman)
    
# 2.0 - Install/load packages using "packman" ---- ----
  # use "p_load" command from pacman to load packages. If packages are not in 
  # the machine's package library, pacman will source them and install them on 
  # the local machine before loading them into the RStudio environment.
    
    # use pacman::p_load() to load necessary packages
    p_load(here, # file directory helper enabling cross platform directory uses
           tidyverse, lubridate, magrittr, broom, purrr, haven, janitor, # data management
           sf, raster, stars, exactextractr, terra, rmapshaper, # spatial processing
           slider, # running averages
           spdep, spatialreg, # spatial analysis
           tidycensus, ipumsr, tigris, # external census data
           stargazer, knitr, # tables/reports
           tmap # simple mapping
    )

# 3.0 - Set and Create Working Directory ---- ---- 
  
  # establish base file path
    here::i_am("environment_setup.R")
    here() # view working directory filepath
    

  # # build foundation directory paths
  #   directory_vec <- c(documentation = here("documentation"),
  #                      DROP = here("DROP"),
  #                      tables_dir = here("tables"),
  #                      figures_dir = here("figures"),
  #                      reports_dir = here("reports"),
  #                      data_dir = here("data"),
  #                      scripts_dir = here("scripts"),
  #                      script_versions = here("script_versions"))
  #   
  # # loop to build all directory paths if they do not already exist. 
  # # Does not overwrite existing folders.
  #   for(folder in directory_vec) {
  #       if (!dir.exists(folder)) {
  #           dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  #       }
  #   }