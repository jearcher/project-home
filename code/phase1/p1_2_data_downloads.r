# ==========================================================================
# Phase 1 data downloads
# ==========================================================================
source("~/git/hprm/code/phase1/p1_1_functions.r")

if(!require(pacman)) install.packages('pacman')
pacman::p_load(colorout, tidycensus, googledrive, sf, tigris, rmapshaper, tidyverse)
options(gargle_oob_default = TRUE, scipen=10, tigris_use_cache = TRUE) # avoid scientific notation, oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)

#
# BEGIN Download from google drive
# --------------------------------------------------------------------------

# IG data
setwd("~/data/ig") # set on local machine or shared drive
drive_download("https://drive.google.com/file/d/1KAgbuDWId_7HZVnEll50ZtUJNAv5M2Jj/view?usp=sharing")
drive_download("https://drive.google.com/file/d/16LXkY5aAogWeOkO0Qy3SDB_Uy2dUubOc/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1vz7JAwmP9fwpVLCYDamb4z5uH-VXS4yf/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1Mdnu6GOMBCoH5G71NiA_nuURso6YVO6R/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1gZu5uNM2l0vaJsNsgDT5coBcQSU5_v1p/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1d9KJVWeRkYiUlDHbMUlHcuFLn64vqPLd/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1_2NMXO2kFQuTLAMJw17LQ6Jc53AIblia/view?usp=sharing")
drive_download("https://drive.google.com/file/d/1_2NMXO2kFQuTLAMJw17LQ6Jc53AIblia/view?usp=sharing")

# Eviction data
drive_download("https://drive.google.com/file/d/1Of3jDZdqwkviHnSsS_4UYIBr3FP-bUDo/view?usp=sharing", path = "~/data/evictions/illinois/Civil_Process_Service_Data_2010_2018.csv.gz")
drive_download("https://drive.google.com/file/d/1-W7yJ1P3ISVxftrZFx8FUA7OfBJkO5d2/view?usp=sharing", path = "~/data/evictions/evictions_rr_all.csv")
drive_download("https://drive.google.com/file/d/1b2VC__UMHPLFPY8Va6wKqtPRXyw1EulL/view?usp=sharing", path = "~/data/evictions/illinois/Chicago_2010_2018_final.csv")
drive_download("https://drive.google.com/file/d/1bAYzCZGh9BkVcX2Ws-komyH5LPxjKXVJ/view?usp=sharing", path = "~/data/evictions/illinois/Chicago_2019_2021_final.csv")
drive_download("https://drive.google.com/file/d/1nXuks3Pa3Lh6RZj0mMGJ5533yElb3Ahh/view?usp=sharing", path = "~/data/evictions/california/sf_20210331.csv")

# Covid Data
drive_download("https://drive.google.com/file/d/1VelUbiHKbAsspFVaEID87yrzlJl_gV46/view?usp=sharing", path = "~/data/covid/covid_rr_all.csv")
drive_download("https://drive.google.com/file/d/1ybnkFvxKW9U6wXi25rkzMUvSYhYwWYZz/view?usp=sharing", path = "~/data/covid/washington/wa_king.xlsx")
drive_download("https://drive.google.com/file/d/1gZ3MvmJXKhNN2An6xatUH5mPBiYAL3P0/view?usp=sharing", path = "~/data/covid/washington/wa_pierce.xlsx")
drive_download("https://drive.google.com/file/d/167qDJyqXF1gytPM7A3qDeJX65oZxz1Nt/view?usp=sharing", path = "~/data/covid/illinois/il.csv")

# Unemployment
drive_download("https://drive.google.com/file/d/1-5qApELgxM8fJge97LvJdOy6jT-r0tON/view?usp=sharing", path = "~/data/unemployment/deepmaps_total_long.csv.bz2")

# Census data
drive_download("https://drive.google.com/file/d/13sZl_rMLMiKJs0Lnp6Chabm33q4cs_h3/view?usp=sharing", path = "~/data/census/US_tracts_sf.rds")

# Other data
# LTDB tract Crosswalk 2000 to 2010
drive_download("https://drive.google.com/file/d/1c6rWT_way9PQC3znUDAVsz1AjN5HpbfG/view?usp=sharing", path = "~/data/census/crosswalk_2000_2010.csv")

#
# END Download from google drive
# --------------------------------------------------------------------------

# ==========================================================================
# Begin Download all US tracts
# ==========================================================================
us_states <- states() %>% pull(STATEFP) %>% unique()
us_counties <- counties()

us_tracts <-
  map_df(us_states, function(state){
    tracts(state = state, cb = TRUE)
    }) %>%
    ms_simplify(keep = 0.7)

us_tracts <-
  st_join(st_centroid(us_tracts),
    us_counties %>%
    select(CO_GEOID = GEOID, NAMELSAD),
    st_intersects) %>%
  st_set_geometry(NULL) %>%
  left_join(us_tracts %>% select(GEOID), .)

saveRDS(us_counties, "~/data/census/US_counties_sf.rds")
saveRDS(us_tracts, "~/data/census/US_tracts_sf.rds")

#
# Doanlowd 2000 Data
# --------------------------------------------------------------------------

ca_2000 <- dl_dec("CA")
il_2000 <- dl_dec("IL")
wa_2000 <- dl_dec("WA")

saveRDS(il_2000, "~/data/census/il_2000.rds")
saveRDS(wa_2000, "~/data/census/wa_2000.rds")
saveRDS(ca_2000, "~/data/census/ca_2000.rds")

#
# download ACS 2011 to 2019
# --------------------------------------------------------------------------

il_acs <- dl_acs("IL")
wa_acs <- dl_acs("WA")
ca_acs <- dl_acs("CA")

saveRDS(il_acs, "~/data/census/il_acs.rds")
saveRDS(wa_acs, "~/data/census/wa_acs.rds")
saveRDS(ca_acs, "~/data/census/ca_acs.rds")

# deal with inf

####
# END ACS Download
####


# ==========================================================================
# End Download
# ==========================================================================

# ==========================================================================
# COVID data
# ==========================================================================

# ==========================================================================
# Begin Download
# ==========================================================================
# chi_covid <-
#   right_join( # use right_join because spatial data is getting ommitted in left_join
#     zctas(cb = TRUE, state = "IL") %>%
#       mutate(zip = as.numeric(ZCTA5CE10)) %>%
#       select(zip),
#     fread("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetZip?format=csv")
#     )

# saveRDS(chi_covid, paste0("~/git/hprm/data/covid/illinois/chi_covid_", Sys.Date(), ".rds"))
#         # paste0("/Volumes/GoogleDrive/My Drive/CCI Docs/Current Projects/HPRM/Data/raw/covid/illinois/chicago/chi_covid-", Sys.Date(), ".rds"))
# ==========================================================================
# End Download
# ==========================================================================