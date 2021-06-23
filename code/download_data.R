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

# Other Eviction Data from HPRM and Eviction Lab, includes 17 metros, 2016 and some 2017
drive_download("https://drive.google.com/file/d/1c46stmOznc84YdLmCBly1eUzI-vc6bwz/view?usp=sharing", path = "../data/evictions/evictions_rr_all.csv")
evictions_rr <- fread("../data/evictions/evictions_rr_all.csv") 
# Need GEOID as character for later joining, then need to put the leading 0 back in
evictions_rr <- evictions_rr[, GEOID := paste("0", as.character(GEOID), sep = "")]






# Census Data ----

# This Code downloads ALL US tracts

us_states <- states() %>% pull(STATEFP) %>% unique()
us_counties <- counties()
us_tracts <-
  map_df(us_states, function(state){
    tracts(state = state, cb = TRUE)
  })
  # %>%
  # ms_simplify(keep = 0.7)
us_tracts <-
  st_join(st_centroid(us_tracts),
          us_counties %>%
            select(CO_GEOID = GEOID, NAMELSAD),
          st_intersects) %>%
  st_set_geometry(NULL) %>%
  left_join(us_tracts %>% select(GEOID), .)

saveRDS(us_tracts, "../data/census/us_tracts.rds")





# I want to download all the ACS data at once
# Some of this is adapted from HPRM

# PLAN
# - Find 17 metros with Evictions Data
# - Download the ACS data for each county represented by 17 metros (except CA, download all state)
# - join evictions and ACS data
# - Drop any rows with NA in evicitons column (this yields our training set)

##### This is HPRM code, slight adjustments to the variables desired
acs_vars = c(
  'Total' = 'B25003_001',
  'Rent' = 'B25003_003',
  'Total_WHITE' = 'B25003A_001',
  'Rent_WHITE' = 'B25003A_003',
  'Total_BLACK' = 'B25003B_001',
  'Rent_BLACK' = 'B25003B_003',
  'Total_AI' = 'B25003C_001',
  'Rent_AI' = 'B25003C_003',
  'Total_ASIAN' = 'B25003D_001',
  'Rent_ASIAN' = 'B25003D_003',
  'Total_NHPI' = 'B25003E_001',
  'Rent_NHPI' = 'B25003E_003',
  'Total_OTHER' = 'B25003F_001',
  'Rent_OTHER' = 'B25003F_003',
  'Total_TWO' = 'B25003G_001',
  'Rent_TWO' = 'B25003G_003',
  'Total_WHITE_NL' = 'B25003H_001',
  'Rent_WHITE_NL' = 'B25003H_003',
  'Total_LATINX' = 'B25003I_001',
  'Rent_LATINX' = 'B25003I_003',
  'MedRent' = 'B25064_001',
  'MHHInc' = 'B19013_001',
  'rb_tot' = 'B25070_001', # GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME
  'rb_10.0' = 'B25070_002', # Less than 10.0 percent GROSS RENT as % hh income
  'rb_14.9' = 'B25070_003', # 10.0 to 14.9 percent GROSS RENT as % hh income
  'rb_19.9' = 'B25070_004', # 15.0 to 19.9 percent GROSS RENT as % hh income
  'rb_24.9' = 'B25070_005', # 20.0 to 24.9 percent GROSS RENT as % hh income
  'rb_29.9' = 'B25070_006', # 25.0 to 29.9 percent GROSS RENT as % hh income
  'rb_34.9' = 'B25070_007', # 30.0 to 34.9 percent GROSS RENT as % hh income
  'rb_39.9' = 'B25070_008', # 35.0 to 39.9 percent GROSS RENT as % hh income
  'rb_49.9' = 'B25070_009', # 40.0 to 49.9 percent GROSS RENT as % hh income
  'rb_55' = 'B25070_010', # 50.0 percent or more GROSS RENT as % hh income
  'rb_nc' = 'B25070_011', # Not computed GROSS RENT as % hh income)
  'totrace' = 'B03002_001',
  'White' = 'B03002_003',
  'Black' = 'B03002_004',
  'Asian' = 'B03002_006',
  'Latinx' = 'B03002_012',
  'totwelf' = 'B19057_001', #
  'welf' = 'B19057_002',
  'totpov' = 'B17017_001', #
  'povfamh' = 'B17017_003',
  'povnonfamh' = 'B17017_020', 
  'totenroll' = 'B14007_001', # Estimate!!Total SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
  'colenroll' = 'B14007_017', # Estimate!!Total!!Enrolled in school!!Enrolled in college undergraduate years  SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
  'proenroll' = 'B14007_018', # Estimate!!Total!!Enrolled in school!!Graduate or professional school  SCHOOL ENROLLMENT BY DETAILED  LEVEL OF SCHOOL FOR THE POPULATION 3 
  'HHInc_Total' = 'B19001_001', # Total HOUSEHOLD INCOME
  'HHInc_10' = 'B19001_002', # Less than $10,000 HOUSEHOLD INCOME
  'HHInc_15' = 'B19001_003', # $10,000 to $14,999 HOUSEHOLD INCOME
  'HHInc_20' = 'B19001_004', # $15,000 to $19,999 HOUSEHOLD INCOME
  'HHInc_25' = 'B19001_005', # $20,000 to $24,999 HOUSEHOLD INCOME
  'HHInc_30' = 'B19001_006', # $25,000 to $29,999 HOUSEHOLD INCOME
  'HHInc_35' = 'B19001_007', # $30,000 to $34,999 HOUSEHOLD INCOME
  'HHInc_40' = 'B19001_008', # $35,000 to $39,999 HOUSEHOLD INCOME
  'HHInc_45' = 'B19001_009', # $40,000 to $44,999 HOUSEHOLD INCOME
  'HHInc_50' = 'B19001_010', # $45,000 to $49,999 HOUSEHOLD INCOME
  'HHInc_60' = 'B19001_011', # $50,000 to $59,999 HOUSEHOLD INCOME
  'HHInc_75' = 'B19001_012', # $60,000 to $74,999 HOUSEHOLD INCOME
  'HHInc_100' = 'B19001_013', # $75,000 to $99,999 HOUSEHOLD INCOME
  'HHInc_125' = 'B19001_014', # $100,000 to $124,999 HOUSEHOLD INCOME
  'HHInc_150' = 'B19001_015', # $125,000 to $149,999 HOUSEHOLD INCOME
  'HHInc_200' = 'B19001_016', # $150,000 to $199,999 HOUSEHOLD INCOME
  'HHInc_250' = 'B19001_017', # $200,000 or more HOUSEHOLD INCOME
  'HHIncTen_Total' = 'B25118_001', # Total
  'HHIncTenOwn' = 'B25118_002', # Owner occupied
  'HHIncTenOwn_5' = 'B25118_003', # Owner occupied!!Less than $5,000
  'HHIncTenOwn_10' = 'B25118_004', # Owner occupied!!$5,000 to $9,999
  'HHIncTenOwn_15' = 'B25118_005', # Owner occupied!!$10,000 to $14,999
  'HHIncTenOwn_20' = 'B25118_006', # Owner occupied!!$15,000 to $19,999
  'HHIncTenOwn_25' = 'B25118_007', # Owner occupied!!$20,000 to $24,999
  'HHIncTenOwn_35' = 'B25118_008', # Owner occupied!!$25,000 to $34,999
  'HHIncTenOwn_50' = 'B25118_009', # Owner occupied!!$35,000 to $49,999
  'HHIncTenOwn_75' = 'B25118_010', # Owner occupied!!$50,000 to $74,999
  'HHIncTenOwn_100' = 'B25118_011', # Owner occupied!!$75,000 to $99,999
  'HHIncTenOwn_150' = 'B25118_012', # Owner occupied!!$100,000 to $149,999
  'HHIncTenOwn_151' = 'B25118_013', # Owner occupied!!$150,000 or more
  'HHIncTenRent' = 'B25118_014', # Renter occupied
  'HHIncTenRent_5' = 'B25118_015', # Renter occupied!!Less than $5,000
  'HHIncTenRent_10' = 'B25118_016', # Renter occupied!!$5,000 to $9,999
  'HHIncTenRent_15' = 'B25118_017', # Renter occupied!!$10,000 to $14,999
  'HHIncTenRent_20' = 'B25118_018', # Renter occupied!!$15,000 to $19,999
  'HHIncTenRent_25' = 'B25118_019', # Renter occupied!!$20,000 to $24,999
  'HHIncTenRent_35' = 'B25118_020', # Renter occupied!!$25,000 to $34,999
  'HHIncTenRent_50' = 'B25118_021', # Renter occupied!!$35,000 to $49,999
  'HHIncTenRent_75' = 'B25118_022', # Renter occupied!!$50,000 to $74,999
  'HHIncTenRent_100' = 'B25118_023', # Renter occupied!!$75,000 to $99,999
  'HHIncTenRent_150' = 'B25118_024', # Renter occupied!!$100,000 to $149,999
  'HHIncTenRent_151' = 'B25118_025', # Renter occupied!!$150,000 or more
  # Added  by project home
  'labor_force' = 'B23025_003E',
  'employed' = 'B23025_004E',
  'unemployed' = 'B23025_005E'
)


# Start with only 2 years to cut processing time during code-writing

# acs_years <- c(2016:2019)
acs_years <- c(2016:2017)

# Metros with labeled evicitons data (via evicions_rr_all.csv)
metros <- read_csv("../data/evictions/labeled_regions.csv", col_names = "Region")

# Denver, CO ====

colorado_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "08",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 


denver_df <- 
  left_join(
  evictions_rr[ , evictions_rr[Region=="Denver"]],
  colorado_df_ph,
  by = c("GEOID", "Year")
  ) %>%
  drop_na(Year)

fwrite(denver_df, file = "../data/processed/denver_df_ev.csv")



#### In Progress ====

# Orlando, Florida ====
florida_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "12",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 


# Miami, Florida ====


# Tampa Bay, FL ====



# Atlanta, GA

georgia_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "13",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

#### Illinois ====

illinois_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "17",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

#### Missourri ====

missouri_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "29",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

#### Kansas (Maybe?)
kansas_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "20",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 


#### Massechuessetts (sp?!?!?)

mass_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "25",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

#### North Carolina ====

nc_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "37",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

#### Ohio ====

ohio_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "39",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 


#### Oklahoma ====

oklahoma_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "39",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

#### South Carolina

sc_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "45",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 


#### Virginia ====

virginia_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "51",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates

#### Washington ====

washington_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "53",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('Tract' = NAME, 'Year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates

#### Put it all together ----

x
acs_data <- rbind(colorado_df_ph, florida_df_ph, illinois_df_ph, missouri_df_ph, 
                  nc_df_ph, ohio_df_ph, oklahoma_df_ph, virginia_df_ph, washington_df_ph)

non_ca_df_ev <- 
  left_join(
    evictions_rr,
    acs_data,
    by = c("GEOID", "Year")
  ) %>%
  drop_na(Year)

write_csv(non_ca_df_ev, file = "../data/processed/non_ca_df_ev.csv")
#################### end test ----

ca_df_ph <- map(
  acs_years,
  ~ get_acs(geography = "tract",
            variables = acs_vars,
            year = .x,
            state = "06",
            output = "wide")
) %>% 
  map2(acs_years, ~ mutate(.x, id = .y)) %>% 
  reduce(rbind)  %>% # stack each year of data (append)
  rename('tract' = NAME, 'year' = id) %>%
  select(!ends_with("M")) %>% # remove margins of error
  rename_at(vars(ends_with("E")), ~ str_remove(., "E$")) # keep only estimates 

ca_df <- data.table(ca_df_ph)

fwrite(ca_df, file = "../data/census/ca_acs.csv")


#########3
ca_unemp <- ca_unemp %>%
  rename(year = id,
         labor_force = B23025_003E,
         employed = B23025_004E,
         unemployed = B23025_005E,
         tract = NAME) %>%
  mutate(unemp_rate = unemployed/labor_force) %>%
  select(GEOID, tract, unemp_rate, year)

ca_unemp %>% count(year)


# GRAVEYARD ----

# # INFOGROUP Data (Private)
# # Source: HPRM Drive
# 
# setwd("../data/infogroup/")
# # This looks like household mobility, must join by GEOID
# # vars include: 'GEOID	YEAR	CAT	HH	MOVE_OUT	MOVE_IN	MOVE_WITHIN'
# # Years 2006-2019
# drive_download("https://drive.google.com/file/d/1KAgbuDWId_7HZVnEll50ZtUJNAv5M2Jj/view?usp=sharing")
# drive_download("https://drive.google.com/file/d/16LXkY5aAogWeOkO0Qy3SDB_Uy2dUubOc/view?usp=sharing")
# drive_download("https://drive.google.com/file/d/1vz7JAwmP9fwpVLCYDamb4z5uH-VXS4yf/view?usp=sharing")
# drive_download("https://drive.google.com/file/d/1Mdnu6GOMBCoH5G71NiA_nuURso6YVO6R/view?usp=sharing")
# 
# # This is 'cumulative', includes move in/ move out *rates*
# # each file seems to correspond (by GEOID) with its respective file above, 4 with counts for tracts, 4 with 'cumulative"
# drive_download("https://drive.google.com/file/d/1gZu5uNM2l0vaJsNsgDT5coBcQSU5_v1p/view?usp=sharing")
# drive_download("https://drive.google.com/file/d/1d9KJVWeRkYiUlDHbMUlHcuFLn64vqPLd/view?usp=sharing")
# drive_download("https://drive.google.com/file/d/1_2NMXO2kFQuTLAMJw17LQ6Jc53AIblia/view?usp=sharing")
# # this file is repeated, typo? 
# # drive_download("https://drive.google.com/file/d/1_2NMXO2kFQuTLAMJw17LQ6Jc53AIblia/view?usp=sharing")
# 


# COVID DATA
# Source: HPRM Drive

# setwd('../covid')

# This file does not exist from the link
# Says "You need Access" when I put the link in a browser

# drive_download("https://drive.google.com/file/d/1VelUbiHKbAsspFVaEID87yrzlJl_gV46/view?usp=sharing", path = "covid_rr_all.csv")

# Data exists in the HPRM repo for covid: (254 'multipolygon's Updated on 03/24/2021)
# covid_sf <- fread("../../hprm_data/covid/bay_counties/sf.csv")


# Unemployment


# Dont have access to this file, also it is not current in the Repo
# drive_download("https://drive.google.com/file/d/1-5qApELgxM8fJge97LvJdOy6jT-r0tON/view?usp=sharing", path = "~/data/unemployment/deepmaps_total_long.csv.bz2")


# Census

# # CA only
# ca_tracts <- 
#   tracts(state = 06, cb = TRUE) %>% ms_simplify(keep = 0.7)
# # ms_simplify(): https://www.rdocumentation.org/packages/rmapshaper/versions/0.4.5/topics/ms_simplify
# ca_counties <-
#   counties(state = 06)
# 
# ca_tracts <-
#   st_join(st_centroid(ca_tracts),
#           ca_counties %>%
#             select(CO_GEOID = GEOID, NAMELSAD),
#           st_intersects) %>%
#   st_set_geometry(NULL) %>%
#   left_join(ca_tracts %>% select(GEOID), .)
# 
# saveRDS(ca_counties, "../data/census/CA_counties.rds")
# saveRDS(ca_tracts, "../data/census/CA_tracts.rds")

# # Keep UDPs processed acs data for CA, put in our project repo
# # 2010-2019
# ca_acs <- readRDS("../data/census/ca_acs.rds")



# Need to add unemployment ACS data to ca_acs data

## This was for just california
# 
# unemp_vars <- c("B23025_003E","B23025_004E","B23025_005E")
# unemp_years <- c(2016:2019)
# 
# ca_unemp <- map(
#   unemp_years,
#   ~ get_acs(geography = "tract",
#             variables = unemp_vars,
#             year = .x,
#             state = "06",
#             output = "wide")
#   ) %>% 
#   map2(unemp_years, ~ mutate(.x, id = .y)) 
# 
# ca_unemp <- (reduce(ca_unemp, rbind)) 
# 
# ca_unemp <- ca_unemp %>%
#   rename(year = id,
#          labor_force = B23025_003E,
#          employed = B23025_004E,
#          unemployed = B23025_005E,
#          tract = NAME) %>%
#   mutate(unemp_rate = unemployed/labor_force) %>%
#   select(GEOID, tract, unemp_rate, year)
# 
# ca_unemp %>% count(year)
# 
# saveRDS(ca_unemp, file = "../data/unemployment/ca_acs_unemp.rds")
