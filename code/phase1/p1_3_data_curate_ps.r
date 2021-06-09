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

ps_tracts <-
  left_join(
    readRDS("~/data/census/US_tracts_sf.rds") %>% filter(STATEFP == 53 & NAMELSAD %in% c("King County", "Snohomish County", "Pierce County")),
    readRDS("~/data/census/wa_acs.rds") %>% filter(year == 2019),
    by = "GEOID")

#
# Displacement
# --------------------------------------------------------------------------

ps_disp <-
  disp(read_csv("~/data/ig/tracts53033_20062019_6.csv")) %>%
  filter(YEARS == "2015-2019", GEOID %in% ps_tracts$GEOID) # %>%
# ms_simplify(keep = 0.5)

#
# Evictions
# --------------------------------------------------------------------------
evictions_us <- 
  read_csv("~/data/evictions/evictions_rr_all.csv")

ps_ev <- evictions_us %>% 
  filter(Region == "Puget") %>% 
  mutate(year = Year) %>%
  right_join(ps_tracts %>% select(GEOID, RentE), .) %>%
  st_set_geometry(NULL) %>%
  #filter(year > 2014) %>%
  group_by(year) %>%
  mutate(
    ev_count = Evictions,
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

ps_unemployment <-
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

ps_covid <- 
  read_xlsx("~/git/hprm/data/covid/washington/biweekly-counts-rates-by-geography-mar-16.xlsx", sheet = 3) %>%
  group_by(ZIP) %>% summarize_all(last) %>%
  right_join(readRDS("~/git/hprm/data/census/wa_zips.rds") %>% transmute(ZIP = ZCTA5CE10), .) %>%
  mutate(
    c_rate = as.numeric(Positive_Rate)/100, 
    c_cut = cut(c_rate,
                breaks = c(0.00, 0.01, 0.02, 0.03, 1.00), 
                labels = c("< .01", ".01 to .02", ".02 to .03", "> .03"))
                #breaks = c(0.00, 0.05, 0.08, 0.10, 1.00), 
                #labels = c("< .05", ".05 to .08", ".08 to .1", "> .1"))
  )

ps_covid <- ps_tracts %>% select(GEOID) %>% st_join(ps_covid, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

#ps_covid %>% st_set_geometry(NULL) %>% group_by(rate) %>% count() %>% data.frame

#
# Merge data
# --------------------------------------------------------------------------

ps_df <-
  left_join(ps_tracts, ps_disp) %>%
  left_join(ps_ev, by = "GEOID") %>%
  left_join(ps_unemployment) %>%
  left_join(ps_covid)

saveRDS(ps_df, "~/data/output/ps_df.rds")
