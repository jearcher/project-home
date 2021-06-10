# Set directory to source file location

#setwd(getSrcDirectory()[1])

# If using RStdio use this:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
if(!require(pacman)) install.packages('pacman')
pacman::p_load(colorout, tidycensus, googledrive, sf, tigris, rmapshaper, tidyverse, data.table)
options(gargle_oob_default = TRUE, scipen=10, tigris_use_cache = TRUE) # avoid scientific notation, oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)

1


# Evictions ----
# Source: HPRM drive https://drive.google.com/drive/folders/1i68Kt9iadjaRNvqKvf-S1Zkln3EAchcj
# UDP Google drive ahs SF evictions
drive_download("https://drive.google.com/file/d/1nXuks3Pa3Lh6RZj0mMGJ5533yElb3Ahh/view?usp=sharing", path = "../data/evictions/sf_20210331.csv")
evictions_sf <- fread("../data/evictions/sf_20210331.csv")
# Create Year var
evictions_sf[ , year := as.numeric(str_sub(evictions_sf$'File Date',-4,-1))]
# Data from 1997-2021
#evictions_sf %>% count(year)
#hist(evictions_sf$year)





# INFOGROUP Data (Private) ----
# Source: HPRM Drive

setwd("../data/infogroup/")
# This looks like household mobility, must join by GEOID
# vars include: 'GEOID	YEAR	CAT	HH	MOVE_OUT	MOVE_IN	MOVE_WITHIN'
# Years 2006-2019
drive_download("https://drive.google.com/file/d/1KAgbuDWId_7HZVnEll50ZtUJNAv5M2Jj/view?usp=sharing")
drive_download("https://drive.google.com/file/d/16LXkY5aAogWeOkO0Qy3SDB_Uy2dUubOc/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1vz7JAwmP9fwpVLCYDamb4z5uH-VXS4yf/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1Mdnu6GOMBCoH5G71NiA_nuURso6YVO6R/view?usp=sharing")

# This is 'cumulative', includes move in/ move out *rates*
# each file seems to correspond (by GEOID) with its respective file above, 4 with counts for tracts, 4 with 'cumulative"
drive_download("https://drive.google.com/file/d/1gZu5uNM2l0vaJsNsgDT5coBcQSU5_v1p/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1d9KJVWeRkYiUlDHbMUlHcuFLn64vqPLd/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1_2NMXO2kFQuTLAMJw17LQ6Jc53AIblia/view?usp=sharing")
# this file is repeated, typo? 
# drive_download("https://drive.google.com/file/d/1_2NMXO2kFQuTLAMJw17LQ6Jc53AIblia/view?usp=sharing")



# COVID DATA ----
# Source: HPRM Drive

setwd('../covid')

# This file does not exist from the link
# Says "You need Access" when I put the link in a browser

# drive_download("https://drive.google.com/file/d/1VelUbiHKbAsspFVaEID87yrzlJl_gV46/view?usp=sharing", path = "covid_rr_all.csv")

# Data exists in the HPRM repo for covid: (254 'multipolygon's Updated on 03/24/2021)
covid_sf <- fread("../../hprm_data/covid/bay_counties/sf.csv")


# Unemployment ----


# Dont have access to this file, also it is not current in the Repo
# drive_download("https://drive.google.com/file/d/1-5qApELgxM8fJge97LvJdOy6jT-r0tON/view?usp=sharing", path = "~/data/unemployment/deepmaps_total_long.csv.bz2")




# Census Data ----

# Note found, but I think we can build it below
# drive_download("https://drive.google.com/file/d/13sZl_rMLMiKJs0Lnp6Chabm33q4cs_h3/view?usp=sharing", path = "~/data/census/US_tracts_sf.rds")


# This Code downloads ALL Us tracts

# us_states <- states() %>% pull(STATEFP) %>% unique()
# us_counties <- counties()
# us_tracts <-
#   map_df(us_states, function(state){
#     tracts(state = state, cb = TRUE)
#   }) %>%
#   ms_simplify(keep = 0.7)


# CA only
ca_tracts <- 
  tracts(state = 06, cb = TRUE) %>% ms_simplify(keep = 0.7)
# ms_simplify(): https://www.rdocumentation.org/packages/rmapshaper/versions/0.4.5/topics/ms_simplify
ca_counties <-
  counties(state = 06)

ca_tracts <-
  st_join(st_centroid(ca_tracts),
          ca_counties %>%
            select(CO_GEOID = GEOID, NAMELSAD),
          st_intersects) %>%
  st_set_geometry(NULL) %>%
  left_join(ca_tracts %>% select(GEOID), .)

saveRDS(ca_counties, "../census/CA_counties.rds")
saveRDS(ca_tracts, "../census/CA_tracts.rds")

# Keep UDPs processed acs data for CA, put in our project repo
saveRDS(readRDS("../hprm_data/census/ca_acs.rds"),
        "../data/census/ca_acs.rds")


ca_acs <- readRDS("../hprm_data/census/ca_acs.rds")

ca_acs %>% count(year)
# there is also a file called "../census/census_wide_ca.csv" which has a bunch of census data (96 vars)
# Only includes 2000, 2009, 2010, 2012, 2018
# ca_census <- fread("../census/census_wide_ca.csv")

# ACS Data




# CDC Data ----

