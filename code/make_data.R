
# Setup ----


# Set directory to source file location

#setwd(getSrcDirectory()[1])

# If using RStdio use this:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
rm(list = ls()) # remove objects in environment
if(!require(pacman)) install.packages('pacman')
if(!require(devtools)) install.packages('devtools')
devtools::install_github(c("timathomas/neighborhood", "jalvesaq/colorout"))
pacman::p_load(BAMMtools, classInt, colorout, rmapshaper, neighborhood, lubridate, googledrive, sf, data.table, tigris, tidyverse)
options(width = Sys.getenv('COLUMNS'), tigris_use_cache = TRUE, gargle_oob_default = TRUE)



# Data ----


## CA ACS Data process by UDP (source from tiny census in R) ====
# 2010-2019 

## DONT HAVE DECENNIAL (2000) DATA DO WE NEED IT? ====

## Join acs data with spatial data (from download_data.R, ca_tracts)
ca_tracts_acs <-
  left_join(
    readRDS("../data/census/CA_tracts.rds"),
    readRDS("../data/census/ca_acs.rds"),
    by = "GEOID")

sf_tracts <-
  left_join(
    readRDS("../data/census/CA_tracts.rds") %>% filter(COUNTYFP == "075"),
    readRDS("../data/census/ca_acs.rds"),
    by = "GEOID")
## Eviction (SF ONLY) ----




#### In Progress ====
# sf_ev <- read_csv("../data/evictions/sf_20210331.csv") %>%
#   # Create Variables 
#   mutate(
#     # remove word "POINT", opening and closing parentheses
#     clean_Shape = str_replace_all(Shape, c("POINT \\(|\\)"), ""),
#     # Get calendar object from var 'File Date' 
#     date = mdy(`File Date`),
#     year = year(date)) %>%
#   # Make clean_Shape into X and Y coordinates (cars called X and Y)
#   separate(clean_Shape, c("X", "Y", sep = ""))
# 
# sf_ev <-
#   read_csv("../data/evictions/california/sf_20210331.csv") %>% 
#   mutate(
#     clean_Shape = str_replace_all(Shape, c("POINT \\(|\\)"), ""),
#     date = mdy(`File Date`),
#     year = year(date)) %>%
#   separate(clean_Shape, c("X", "Y"), sep = " ") %>%
#   filter(!is.na(Shape)) %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
#   st_join(., sf_tracts %>% select(GEOID), st_intersects) %>%
#   st_set_geometry(NULL) %>%
#   group_by(GEOID, year) %>%
#   summarize(ev_count = n()) %>%
#   right_join(sf_tracts %>% select(GEOID, Rent), .) %>%
#   st_set_geometry(NULL) %>%
#   filter(year > 2014) %>%
#   group_by(year) %>%
#   mutate(
#     ev_rate = ev_count/Rent,
#     ev_rr = RR(ev_count, Rent),
#     er_cut = 
#       factor(
#         er_cut(ev_rate), 
#         levels = c("< 1%", "1% to 2%", "2% to 4%", "4% to 8%", "> 8%")),
#     rr_cut = 
#       factor(
#         rr_cut(ev_rr),
#         levels = c("Extremely Low", "Low", "Below Average", "Above Average", "High", "Extremely High")), 
#     ev_count_cut = 
#       factor(
#         count_cut(ev_count), 
#         levels = c("< 10", "10 to 20", "20 to 40", "> 40"))
#   ) %>%
#   mutate(
#     ev_rate_cut = 
#       factor(
#         case_when(
#           ev_rate < .01 ~ "Very Low", 
#           ev_rate >= .01 & ev_rate < .02 ~ "Low", 
#           ev_rate >= .02 & ev_rate < .04 ~ "Moderate", 
#           ev_rate >= .04 & ev_rate < .08 ~ "High", 
#           ev_rate >= .08 ~ "Very High"), 
#         levels = c("Very Low", "Low", "Moderate", "High", "Very High")
#       )
#   ) %>% 
#   ungroup()
