
# Setup ----


# Set directory to source file location

#setwd(getSrcDirectory()[1])

# If using RStudio use this:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
rm(list = ls()) # remove objects in environment
if(!require(pacman)) install.packages('pacman')
if(!require(devtools)) install.packages('devtools')
devtools::install_github(c("timathomas/neighborhood", "jalvesaq/colorout"))
pacman::p_load(BAMMtools, classInt, colorout, rmapshaper, neighborhood, lubridate, googledrive, sf, data.table, tigris, tidyverse)
options(width = Sys.getenv('COLUMNS'), tigris_use_cache = TRUE, gargle_oob_default = TRUE)

# helpers
source("../code/functions.R")


# Data ----

## CA ACS Data process by UDP (source from tiny census in R) ====
# 2010-2019 
ca_acs <- readRDS("../data/census/ca_acs.rds")
## DONT HAVE DECENNIAL (2000) DATA DO WE NEED IT?

## Join acs data with spatial data (from download_data.R, ca_tracts)
ca_tracts_acs <-
  left_join(
    readRDS(""),
    ca_acs,
    by = "GEOID") %>% 
  select(!ends_with("M")) %>%
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) %>%
  rename('Tract' = NAME.x)

# Filter for SF, counts estimates as real values
sf_tracts <-
  left_join(
    readRDS("../data/census/CA_tracts.rds") %>% filter(COUNTYFP == "075"),
    ca_acs,
    by = "GEOID") %>% 
  select(!ends_with("M")) %>%
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$"))

## COVID ----

# SF only covid, can find it on sf_df
 

## Eviction (SF ONLY) ----

# Pull straight from clean UDP data:
sf_df <- readRDS("../hprm_data/output/sf_df.rds")

# set_as_sf() converts object into spatial obj
# https://r-spatial.github.io/sf/reference/st_as_sf.html
# crs - coordinate reference system

# Adapted from HPRM, but without certain variables (ev categories)
sf_ev <- read_csv("../data/evictions/sf_20210331.csv") %>%
  # Create Variables
  mutate(
    # remove word "POINT", opening and closing parentheses
    clean_Shape = str_replace_all(Shape, c("POINT \\(|\\)"), ""),
    # Get calendar object from var 'File Date'
    date = mdy(`File Date`),
    year = year(date)) %>%
  # Make clean_Shape into X and Y coordinates (cars called X and Y)
  separate(clean_Shape, c("X", "Y"), sep = " ") %>%
  filter(!is.na(Shape)) %>%
  # convert into spatial object??
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
  # Spatial Join
  st_join(., sf_tracts %>% select(GEOID), st_intersects) %>%
  st_set_geometry(NULL) %>%
  # Gather a eviction count by GEOID and year
  group_by(GEOID, year) %>%
  summarize(ev_count = n()) %>%
  right_join(sf_tracts %>% select(GEOID, Rent), .) %>%
  # Create eviction rate, relative risk
  mutate(
    ev_rate = ev_count/Rent,
    ev_rr = RR(ev_count, Rent)) %>%
  ungroup()
  


#### In Progress ====

## Unemployment  ----

# From Missourri Census Data Center
# SINCE we want year to be a variable, I need to change this to APPEND ue data instead of MERGE
ca_unemp_2018 <- read_csv("../data/unemployment/mcdc/mcdc_unemp_2018_2019acs.csv") %>%
  mutate(unemp = B23025i5/B23025i3) %>%
  mutate(year = 2018)

# Not certain if this is 2017! Might need to fix
ca_unemp_2017 <- read_csv("../data/unemployment/mcdc/mcdc_unemp_2018_2018acs.csv") %>%
  mutate(unemp = B23025i5/B23025i3) %>%
  mutate(year = 2017)

ca_unemp_2016 <- read_csv("../data/unemployment/mcdc/mcdc_unemp_2016.csv") %>%
  mutate(unemp = B23025i5/B23025i3) %>%
  mutate(year = 2016)

## MERGE
# ca_unemp <- left_join(ca_unemp_2018, ca_unemp_2017 %>% select(Tract, unemp_2017), by = 'Tract') %>%
#   left_join(ca_unemp_2016 %>% select(Tract, unemp_2016, GEOID), by = 'Tract') %>%
#   select(Tract, unemp_2016, unemp_2018, unemp_2017, GEOID)

## APPEND
ca_unemp <- bind_rows(ca_unemp_2016, ca_unemp_2017, ca_unemp_2018) %>%
  select(AreaName, year, Tract, GEOID, unemp)


ca_unemp$Tract <- as.character(ca_unemp$Tract)

# (from deepmaps.io)
# This only includes data for 2020 (monthly)

# time the process, large data
# unemp_time <- proc.time()
# sf_unemployment <-
#   read_csv("../data/unemployment/deepmaps_tractdata_december2020_prelim.zip")
# proc.time() - unemp_time
# 
# names(sf_unemployment)
#### 

## Put it all together! ----

# Sparse as there isn't any eviction data for places other than SF
ca_df <- left_join(ca_tracts_acs %>% filter(year>=2016), ca_unemp, by = c('Tract', 'year')) %>%
  rename('GEOID' = 'GEOID.x') %>%
  st_join(sf_ev %>% filter(year>=2016), by = 'GEOID')

write_csv(ca_df, file = "../data/processed/ca_df.csv")

####

ca_df <- read_csv("../data/processed/ca_df.csv")

sf_check <- ca_df %>% filter(COUNTYFP == "075")
