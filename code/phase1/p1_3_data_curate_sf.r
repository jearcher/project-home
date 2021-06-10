# ==========================================================================
# San Francisco Data curation
# ==========================================================================

rm(list = ls()) # remove objects in environment
if(!require(pacman)) install.packages('pacman')
if(!require(devtools)) install.packages('devtools')
devtools::install_github(c("timathomas/neighborhood", "jalvesaq/colorout"))
pacman::p_load(BAMMtools, classInt, colorout, rmapshaper, neighborhood, lubridate, googledrive, sf, data.table, tigris, tidyverse)
options(width = Sys.getenv('COLUMNS'), tigris_use_cache = TRUE, gargle_oob_default = TRUE)

#
# source functions
# --------------------------------------------------------------------------

source("~/git/hprm/code/phase1/p1_1_functions.r")

# ==========================================================================
# Data
# ==========================================================================

ca_2000 <- readRDS("~/data/census/ca_2000.rds")
ca_acs <- readRDS("~/data/census/ca_acs.rds")

#####
# Testbed error in LI count
#####

# Current ACS data 
# glimpse(ca_acs %>% filter(GEOID %in% c("06067008905", "06067000400"), year == 2018))

# # DT acs data
# dt_acs <- read_csv("/Users/timthomas/git/displacement-typologies-og/data/outputs/downloads/SanFranciscocensus_summ_2018.csv")
# dt_acs %>% filter(FIPS %in% c("06067008905", "06067000400")) %>% glimpse()

# area_li_19(ca_acs, ca_dec_li, yr = 2018) %>% filter(GEOID %in% c("06067008905", "06067000400")) %>% glimpse()

# dt_sf <- read_csv("/Users/timthomas/git/displacement-typologies-og/data/outputs/typologies/SanFrancisco_typology_output.csv") 

# dt_sf %>% filter(FIPS %in% c("6067008905", "6067000400")) %>% glimpse()

# export <- 
#   left_join(ca_acs %>% filter(year == 2018), area_li_19(ca_acs, ca_dec_li, yr = 2018), by = "GEOID")
# write_csv(export, "~/Downloads/hprm_acs_results_li.csv.gz")
# glimpse(export)

#####
# End testbed
#####

# ==========================================================================
# Curation
# ==========================================================================

#
# Merge spatial and ca_acs
# --------------------------------------------------------------------------

sf_tracts <-
	left_join(
		readRDS("~/data/census/US_tracts_sf.rds") %>% filter(STATEFP == "06", COUNTYFP == "075"),
    ca_acs %>% filter(year == 2019),
		by = "GEOID")

#
# Displacement - On hold till IG data is fixed
# --------------------------------------------------------------------------

# sf_disp <-
# 	disp(read_csv("~/data/ig/tracts06001_20062019_6.csv")) %>%
# 	filter(YEARS == "2015-2019", GEOID %in% sf_tracts$GEOID) # %>%
#   # ms_simplify(keep = 0.5)

#
# Income categorization & typology creation
# --------------------------------------------------------------------------
ca_li <- area_li_19(ca_acs, dec_li(ca_2000))

#
# Evictions
# --------------------------------------------------------------------------

sf_ev <-
  read_csv("~/data/evictions/california/sf_20210331.csv") %>% 
  mutate(
    clean_Shape = str_replace_all(Shape, c("POINT \\(|\\)"), ""),
    date = mdy(`File Date`),
    year = year(date)) %>%
  separate(clean_Shape, c("X", "Y"), sep = " ") %>%
  filter(!is.na(Shape)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
  st_join(., sf_tracts %>% select(GEOID), st_intersects) %>%
  st_set_geometry(NULL) %>%
  group_by(GEOID, year) %>%
  summarize(ev_count = n()) %>%
  right_join(sf_tracts %>% select(GEOID, Rent), .) %>%
  st_set_geometry(NULL) %>%
  filter(year > 2014) %>%
  group_by(year) %>%
  mutate(
    ev_rate = ev_count/Rent,
    ev_rr = RR(ev_count, Rent),
    er_cut = 
      factor(
        er_cut(ev_rate), 
        levels = c("< 1%", "1% to 2%", "2% to 4%", "4% to 8%", "> 8%")),
    rr_cut = 
      factor(
        rr_cut(ev_rr),
        levels = c("Extremely Low", "Low", "Below Average", "Above Average", "High", "Extremely High")), 
    ev_count_cut = 
      factor(
        count_cut(ev_count), 
        levels = c("< 10", "10 to 20", "20 to 40", "> 40"))
    ) %>%
  mutate(
    ev_rate_cut = 
      factor(
        case_when(
          ev_rate < .01 ~ "Very Low", 
          ev_rate >= .01 & ev_rate < .02 ~ "Low", 
          ev_rate >= .02 & ev_rate < .04 ~ "Moderate", 
          ev_rate >= .04 & ev_rate < .08 ~ "High", 
          ev_rate >= .08 ~ "Very High"), 
        levels = c("Very Low", "Low", "Moderate", "High", "Very High")
          )
      ) %>% 
  ungroup()

#
# Unemployment
# --------------------------------------------------------------------------

sf_unemployment <-
  read_csv("~/data/unemployment/deepmaps_total_long.csv.bz2") %>%
  mutate(GEOID = str_pad(fips, 11, pad = "0")) %>%
    group_by(GEOID) %>%
    summarize(
      unemployed = mean(unemployed, na.rm = TRUE),
      laborforce = mean(laborforce, na.rm = TRUE),
      unemp_rate = unemployed/laborforce
      ) %>%
  group_by(GEOID) %>%
  mutate(
    u_cut = 
      factor(
        unemp_cut(unemp_rate), 
        levels = c("< 5%", "5% to 8%", "8% to 14%", "14% to 20%", "> 20%")
      ))

#
# COVID
# --------------------------------------------------------------------------

sf_covid <-
  right_join(
    sf_tracts %>%
      transmute(id = GEOID),
    read_csv("~/git/hprm/data/covid/bay_counties/sf.csv") %>%
      filter(area_type == "Census Tract")) %>%
  rename(covid_count = count) %>%
  mutate(GEOID = id,
  		 c_rate = covid_count/acs_population,
         c_cut = cut(c_rate,
                     breaks = c(0, .02, .03, .05, 1),
                     labels = c("< .02", ".02 to .03", ".03 to .05", "> .05"))) %>%
  st_set_geometry(NULL)

#
# Neighborhood Segregation Typologies
# --------------------------------------------------------------------------

ca_nt <- ntdf(state = 'CA', year = 2019)

#
# Merge data
# --------------------------------------------------------------------------

sf_df <-
	left_join(sf_tracts, ca_li, by = 'GEOID') %>% 
	left_join(sf_ev %>% filter(year == 2019), by = 'GEOID') %>% 
  left_join(sf_unemployment) %>% 
	left_join(sf_covid) %>% 
  left_join(ca_nt %>% select(!NAME)) %>% 
  mutate(
    unemp_rate_2019 = unemp/totunemp, 
    unemp_ch = unemp_rate-unemp_rate_2019) %>% 
  mutate(      
    unemp_cat = 
      cut(unemp_ch, 
        breaks = c(-1, .04, .08, .14, 1),
        labels = c("Low", "Moderate", "High", "Very High"),
        ordered_result = FALSE), 
      hprm_ev = 
        case_when(
          ev_rate < .02 ~ 0, 
          ev_rate >= .02 & ev_rate < .04 ~ 1, 
          ev_rate >= .04 & ev_rate < .08 ~ 2, 
          ev_rate >= .08 ~ 3),
      hprm_dis = 
        case_when(
          typology == 'Low-income & Susceptible to Displacement' ~ 2, 
          typology == 'Ongoing Displacement' ~ 3, 
          TRUE ~ 0),  
      hprm_unemp = 
        case_when(
          unemp_rate < .08 ~ 0,
          unemp_rate >= .08 & unemp_rate < .14 ~ 1, 
          # unemp_rate >= .08 & unemp_rate < .14 ~ 2, 
          unemp_rate >= .14 ~ 2
          ),
      hprm_ch_unemp = 
        case_when(
          unemp_ch < .08 ~ 0, 
          # unemp_ch >= .04 & unemp_ch < .08 ~ 1, 
          # unemp_ch >= .08 & unemp_ch < .14 ~ 2, 
          unemp_ch >= .08 ~ 1)
    ) %>% 
  group_by(GEOID) %>% 
  mutate(
    hprm_scale = sum(hprm_ev, hprm_dis, hprm_unemp, hprm_ch_unemp, na.rm = TRUE)) %>% 
  mutate(
    hprm_cut = 
      factor(
          case_when(
            hprm_unemp+hprm_ch_unemp == 3 ~ 'High Precarity', 
            hprm_ev == 3 | hprm_dis == 3 ~ 'High Precarity',
            hprm_scale < 3 ~ 'Low Precarity', 
            # hprm_scale >=  & hprm_scale < 3 ~ 'Moderate Precarity', 
            hprm_scale >= 3 & hprm_scale < 6 ~ 'Moderate Precarity', 
            hprm_scale >= 6 ~ 'High Precarity'), 
          levels = c('Low Precarity', 'Moderate Precarity', 'High Precarity'))) %>%
  ungroup()

# 
# getJenksBreaks(sf_df$unemp_rate, 5)

write_rds(sf_df, "~/git/hprm/data/output/sf_df.rds")
# st_write(sf_df, "~/git/hprm/data/output/sf_df.gpkg", delete_dsn = TRUE)