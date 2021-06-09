# ==========================================================================
# Data curation
# ==========================================================================

rm(list = ls()) # remove objects in environment
devtools::install_github(c("timathomas/neighborhood", "jalvesaq/colorout"))
pacman::p_load(rmapshaper, neighborhood, lubridate, googledrive, sf, data.table, tigris, tidyverse)
options(width = Sys.getenv('COLUMNS'), tigris_use_cache = TRUE, gargle_oob_default = TRUE)

#
# source functions
# --------------------------------------------------------------------------

source("~/git/hprm/code/phase1/p1_2_functions.r")


# ==========================================================================
# Data
# ==========================================================================

#
# ACS & merge
# --------------------------------------------------------------------------

chi_tracts <-
  left_join(
    readRDS("~/data/census/US_tracts_sf.rds") %>% filter(NAMELSAD == "Cook County"),
    readRDS("~/git/hprm/data/census/il_acs.rds") %>% filter(year == 2019),
    by = "GEOID")

#
# Displacement
# --------------------------------------------------------------------------

chi_disp <-
  disp(read_csv("~/data/ig/tracts17031_20062019_6.csv")) %>%
  filter(YEARS == "2015-2019", GEOID %in% chi_tracts$GEOID) # %>%
# ms_simplify(keep = 0.5)

#
# Evictions
# --------------------------------------------------------------------------

chi_ev <-
  bind_rows(
    read_csv("~/data/evictions/illinois/Chicago_2010_2018_final.csv") %>%
      transmute(CaseNum, year = year(File_Date), X = cxy_lon, Y = cxy_lat),
    read_csv("~/data/evictions/illinois/Chicago_2019_2021_final.csv") %>% 
      transmute(CaseNum, year = year(File_Date), X = cxy_lon, Y = cxy_lat)) %>%
  filter(!is.na(X)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
  st_join(., chi_tracts %>% select(GEOID), st_intersects) %>%
  st_set_geometry(NULL) %>%
  group_by(GEOID, year) %>%
  summarize(ev_count = n()) %>%
  right_join(chi_tracts %>% select(GEOID, RentE), .) %>%
  st_set_geometry(NULL) %>%
  filter(year > 2014) %>%
  group_by(year) %>%
  mutate(
    ev_rate = ev_count/RentE,
    ev_rr = RR(ev_count, RentE),
    er_cut = er_cut(ev_rate),
    rr_cut = rr_cut(ev_rr),
    ev_count_cut = count_cut(ev_count)
  ) %>%
  ungroup()

#
# Unemployment
# --------------------------------------------------------------------------

chi_unemployment <-
  read_csv("~/data/unemployment/deepmaps_total_long.csv.bz2") %>%
  mutate(GEOID = str_pad(fips, 11, pad = "0")) %>%
  group_by(GEOID) %>%
  summarize(
    unemployed = mean(unemployed, na.rm = TRUE),
    laborforce = mean(laborforce, na.rm = TRUE),
    unemp_rate = unemployed/laborforce
  ) %>%
  group_by(GEOID) %>%
  mutate(u_cut = unemp_cut(unemp_rate))

#
# COVID
# --------------------------------------------------------------------------

il_zips <- readRDS("~/data/census/il_zips.rds")
il_covid <- 
  il_zips %>% 
  transmute(zip = as.numeric(ZCTA5CE10)) %>% 
  right_join(
    read_csv("~/data/covid/illinois/il.csv") %>% 
      mutate(date = mdy_hm(reportDate),
             c_rate = confirmed_cases/total_tested)
  ) %>%
  mutate(c_cut = cut(c_rate,
                     breaks = c(0, .05, .08, .1, 1), 
                     labels = c("< .05", ".05 to .08", ".08 to .1", "> .1"))
  )

chi_covid <- chi_tracts %>% st_join(il_covid, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

#
# Merge data
# --------------------------------------------------------------------------

chi_df <-
  left_join(chi_tracts, chi_disp) %>%
  left_join(chi_ev) %>%
  left_join(chi_unemployment) %>%
  left_join(chi_covid)

saveRDS(chi_df, "~/data/output/chi_df.rds")

#
# Chicago
# --------------------------------------------------------------------------

# Covid data

# chi_covid <-
# 	readRDS("Google Drive/CCI Docs/Current Projects/HPRM/Data/raw/covid/illinois/chicago/chi_covid-2021-02-03.rds")
#
# # Eviction data
#
# chi_ev <-
# 	fread("Google Drive/CCI Docs/Current Projects/HPRM/Data/raw/evictions/illinois/chicago/addresses_2020-06-05_race_sex_geo.csv.bz2")
#
# chi_ev1 <-
# 	read.csv("Google Drive/CCI Docs/Current Projects/HPRM/Data/raw/evictions/illinois/chicago/geocoded_addresses_2020-06-04.csv")
#
# glimpse(chi_ev1)
#
# #
# # San Francisco
# # --------------------------------------------------------------------------
#
# sf_ev <-
# 	fread("Google Drive/CCI Docs/Current Projects/HPRM/Data/raw/evictions/california/sf/Eviction_Notices2021-02-03.csv") %>%
#
# ))
#
# # ==========================================================================
# # process data
# # ==========================================================================
#
