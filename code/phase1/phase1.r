# ==========================================================================
# Phase 1 map development for HPRM
# Tasks: develop maps for eviction, exit, unemployment, and covid
# risk.
# Based on the availability of eviction data, we will work on
# Eviction lab data
#	* Atlanta
#	* Boston
#	* Charleston
#	* Charlotte
#	* Chicago
#	* Cleveland
#	* Denver
#	* Kansas City
#	* Miami
#	* Oklahoma City
#	* Orlando
#	* Raleigh
#	* Richmond
#	* St. Louis
#	* Tampa Bay
#	* Virginia Beach
#
# Evictions study data
# 	* Baltimore
#	* Seattle puget sound
# ==========================================================================

#
# Libraries
# --------------------------------------------------------------------------

pacman::p_load_gh("walkerke/tidycensus")
pacman::p_load(colorout, RCurl, lubridate, readxl, rmapshaper, googledrive, tmap, tigris, sf, tidyverse, leaflet, leaflet.extras, epitools)
options(gargle_oob_default = TRUE, scipen=10, width=Sys.getenv("COLUMNS"), tigris_use_cache = TRUE) # avoid scientific notation, oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)

if(Sys.getenv("USER") == "alexramiller"){
  setwd("/Volumes/GoogleDrive/Shared drives/udpdata/project_outputs/hprm")
} else {
  setwd("~")
}   

#
# Data
# --------------------------------------------------------------------------

il_acs <- readRDS("~/git/hprm/data/census/il_acs.rds")
wa_acs <- readRDS("~/git/hprm/data/census/wa_acs.rds")
ca_acs <- readRDS("~/git/hprm/data/census/ca_acs.rds")

#
# Displacement
# --------------------------------------------------------------------------

ig06 <- read_csv("data/ig/tracts06001_20062019_6.csv")
ig53 <- read_csv("data/ig/tracts53033_20062019_6.csv")
ig17 <- read_csv("data/ig/tracts17031_20062019_6.csv")

### dev ###


### Create and make spatial ### 

# ==========================================================================
# Begin simplify and join displacement data
# ==========================================================================
us_tracts <- readRDS("data/census/US_tracts_sf.rds")
gc()


# il_disp_sf <-
#   right_join(us_tracts, disp(ig17)) %>% 
#   ms_simplify(keep = 0.5)
# st_crs(il_disp_sf) <- 4269 # require transform becuase gdal may be old and was created with new

# wa_disp_sf <-
#   right_join(us_tracts, disp(ig53)) %>% 
#   ms_simplify(keep = 0.5)
# st_crs(wa_disp_sf) <- 4269 # require transform becuase gdal may be old and was created with new

# saveRDS(ca_disp_sf, "~/git/hprm/data/displacement/ca_disp_sf.rds")
# saveRDS(il_disp_sf, "~/git/hprm/data/displacement/il_disp_sf.rds")
# saveRDS(wa_disp_sf, "~/git/hprm/data/displacement/wa_disp_sf.rds")
# ==========================================================================
# END simplify/join
# ==========================================================================

ca_disp_sf <- readRDS("~/git/hprm/data/displacement/ca_disp_sf.rds")
il_disp_sf <- readRDS("~/git/hprm/data/displacement/il_disp_sf.rds")
wa_disp_sf <- readRDS("~/git/hprm/data/displacement/wa_disp_sf.rds")

# --------------------------------------------------------------------------
# Note: 
# You will likely want to use either
# replacement rate: vli_exit_adj, li_exit_adj, livli_exit_adj, mi_exit_adj, hi_exit_adj
# OR
# Net migration rate per 1000 people: net_high_adj, net_middle_adj, net_low_adj, net_very_low_adj, net_livli_adj
# --------------------------------------------------------------------------

#
# Eviction data
# --------------------------------------------------------------------------

### Functions ###

RR <- function(evicted, renters){
  o = evicted
  r = sum(evicted, na.rm = TRUE)/sum(renters, na.rm = TRUE)
  e = renters*r
  rr = o/e
  rr
}

er_cut <- function(x){
  cut(x, 
    breaks = c(0, .01, .02, .03, 0.05, 1),
    labels = c("< 1%", "1% to 2%", "2% to 3%", "3% to 5%", "> 5%"), 
    ordered_result = FALSE)
}

rr_cut <- function(x){
  cut(x, 
    breaks = c(0, .25, .5,1, 2, 3, 30),
    labels = c("Extremely Low", "Low", "Below Average", "Above Average", "High", "Extremely High"), 
    ordered_result = FALSE)
    }

count_cut <- function(x){
  cut(x, 
    breaks = c(0, 10, 20, 40, 300),
    labels = c("< 10", "10 to 20", "20 to 40", "> 40"), 
    ordered_result = FALSE)
    }

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

### Data ###

evictions_us <- 
  read_csv("data/evictions/evictions_rr_all.csv") %>% 
  filter(Region != "Chicago") 

chi_ev_18 <- 
  read_csv("data/evictions/illinois/Chicago_2010_2018_final.csv") %>% 
  mutate(year = year(File_Date)) 

chi_ev_21 <- 
  read_csv("data/evictions/illinois/Chicago_2019_2021_final.csv") %>% 
  mutate(
    year = year(File_Date), 
    ID = paste(CaseNum, FullAddress), 
    GEOID = as.character(GEOID))



wa_ev <- 
  evictions_us %>% 
  filter(Region == "Puget") %>% 
  mutate(year = Year)

# chi_ev <- read_csv("data/evictions/illinois/Civil_Process_Service_Data_2010_2018.csv")

#
# COVID data 
# --------------------------------------------------------------------------
covid <- read_csv("data/health/covid_rr_all.csv")
# il_zips <- zctas(state = "IL", year = 2010)
# saveRDS(il_zips, "~/git/hprm/data/census/il_zips.rds")
# wa_zips <- zctas(state = "WA", year = 2010)
# saveRDS(wa_zips, "~/git/hprm/data/census/wa_zips.rds")
il_zips <- readRDS("~/git/hprm/data/census/il_zips.rds")

il_covid <- 
  il_zips %>% 
  transmute(zip = as.numeric(ZCTA5CE10)) %>% 
  right_join(
    read_csv("~/git/hprm/data/covid/illinois/il.csv") %>% 
      mutate(date = mdy_hm(reportDate),
             rate = confirmed_cases/total_tested)
  ) %>%
  mutate(c_cut = cut(rate,
                     breaks = c(0, .05, .08, .1, 1), 
                     labels = c("< .05", ".05 to .08", ".08 to .1", "> .1"))
  )
  

  
wa_king_covid <- 
  read_xlsx("~/git/hprm/data/covid/washington/biweekly-counts-rates-by-geography-mar-16.xlsx", sheet = 3) %>% 
  right_join(readRDS("~/git/hprm/data/census/wa_zips.rds") %>% transmute(ZIP = ZCTA5CE10), .) %>%
  mutate(
      rate = as.numeric(Positive_Rate), 
      c_cut = cut(rate,
                     breaks = c(0.00, 0.05, 0.08, 0.10, 1.00), 
                     labels = c("< .05", ".05 to .08", ".08 to .1", "> .1"))
  )

wa_king_covid %>% st_set_geometry(NULL) %>% group_by(rate) %>% count() %>% data.frame

#
# Unemployment data
# --------------------------------------------------------------------------

### Unemployment rate cutoffs
# These are based on .035 to .045 being good for economy,
# .05
# .08
# .14
# .20
####


#
# Final data
# --------------------------------------------------------------------------

df <- function(ev_data, year, acs, disp, covid){
  ev_data %>% 
  filter(year == year) %>% 
  group_by(GEOID, year) %>% 
  summarize(ev_count = n()) %>% 
  left_join(acs %>% filter(year == year) %>% select(GEOID, RentE)) %>% 
  left_join(unemp_yly) %>%
  ungroup() %>% 
  mutate(
    ev_rate = case_when(RentE == 0 ~ NA_real_, TRUE ~ ev_count/RentE), 
    ev_rr = case_when(RentE == 0 ~ NA_real_, TRUE ~ RR(ev_count, RentE)), 
    ev_cut = cut(
      ev_rr, 
      breaks = c(0, .25, .5, .9, 1.1, 2, 3, 12),
      labels = c("< .25", ".25 to .5", ".5 to .9", ".9 to 1.1", "1.1 to 2", "2 to 3", "> 3"), 
      ordered_result = FALSE)) %>% 
  right_join(disp, .) %>% 
  mutate(
    # di_vlinet_sc = scale_this(net_very_low_adj),    
    # di_linet_sc = scale_this(net_low_adj),
    # di_livlinet_sc = scale_this(net_livli_adj),
    # di_midnet_sc = scale_this(net_middle_adj),
    # di_high_sc = scale_this(net_high_adj),
    di_vlinet_cut = 
      cut(net_very_low_adj, 
        breaks = c(-400, -50, -25, -5, 5, 25, 50, 700), 
        labels = c("High Out: < -50", "Out: -50 to -25", "Low Out: -25 o -5", "Even: -5 to 5", "Low In: 5 to 25", "In: 25 to 50", "High in: > 50")),    
    di_linet_cut = 
      cut(net_low_adj, 
        breaks = c(-400, -50, -25, -5, 5, 25, 50, 700), 
        labels = c("High Out: < -50", "Out: -50 to -25", "Low Out: -25 o -5", "Even: -5 to 5", "Low In: 5 to 25", "In: 25 to 50", "High in: > 50")),    
    di_livlinet_cut =       
      cut(net_livli_adj, 
        breaks = c(-400, -50, -25, -5, 5, 25, 50, 700), 
        labels = c("High Out: < -50", "Out: -50 to -25", "Low Out: -25 o -5", "Even: -5 to 5", "Low In: 5 to 25", "In: 25 to 50", "High in: > 50")),    
    di_midnet_cut = 
      cut(net_middle_adj, 
        breaks = c(-400, -50, -25, -5, 5, 25, 50, 700), 
        labels = c("High Out: < -50", "Out: -50 to -25", "Low Out: -25 o -5", "Even: -5 to 5", "Low In: 5 to 25", "In: 25 to 50", "High in: > 50")),    
    di_high_cut = 
      cut(net_high_adj, 
        breaks = c(-400, -50, -25, -5, 5, 25, 50, 700), 
        labels = c("High Out: < -50", "Out: -50 to -25", "Low Out: -25 o -5", "Even: -5 to 5", "Low In: 5 to 25", "In: 25 to 50", "High in: > 50")),
    u_cut = 
      cut(unemp_rate,
        breaks = c(0, .05, .08, .14, 1), 
        labels = c("< .05", ".05 to .08", ".08 to .14", "> .14"))
    )}

chi_data <- df(chi_ev_21, 2019, il_acs, il_disp_sf)
sf_data <- df(sf_ev, 2019, ca_acs, ca_disp_sf)
wa_data <- df(wa_ev, 2017, wa_acs, wa_disp_sf)

#   chi_data %>% st_set_geometry(NULL) %>% summarize(sc = round(di_midnet_sc, 2), count = round(net_middle_adj, 2)) %>% distinct() %>% arrange(sc) %>% data.frame()
# vli -2 = -77 || -1 = -29 || 0 = 18 || 1 = 66 || 2 = 115
# li -2 = -95 || -1 = -46 || 0 = 2 || 1 = 51 || 2 = 100
# middle -2 = -105 || -1 = -52 || 0 = 3 || 1 = 56 || 2 = 112
# ==========================================================================
# END data create
# BEGIN Mapping
# ==========================================================================

#
# palettes 
# --------------------------------------------------------------------------

e_pal <- function(data){
  colorFactor(
    c("#2166ac", 
      "#67a9cf",
      "#d1e5f0",
      "#f7f7f7",
      "#fddbc7",
      "#ef8a62",
      "#b2182b"),
    domain = data,
    na.color = "transparent")
}

chi_e_pal <- e_pal(chi_data$ev_cut)
sf_e_pal <- e_pal(sf_data$ev_cut)
wa_e_pal <- e_pal(wa_data$ev_cut)

d_pal <- function(data){
  colorFactor(
    c("#b2182b", 
      "#ef8a62",
      "#fddbc7",
      "#f7f7f7",
      "#d1e5f0",
      "#67a9cf",
      "#2166ac"
      ),
    domain = data,
    na.color = "transparent")
}

chi_d_vli_pal <- d_pal(chi_data$di_vlinet_cut)
chi_d_li_pal <- d_pal(chi_data$di_linet_cut)
chi_d_livli_pal <- d_pal(chi_data$di_livlinet_cut)
chi_d_mid_pal <- d_pal(chi_data$di_midnet_cut)
chi_d_high_pal <- d_pal(chi_data$di_high_cut)

sf_d_vli_pal <- d_pal(sf_data$di_vlinet_cut)
sf_d_li_pal <- d_pal(sf_data$di_linet_cut)
sf_d_livli_pal <- d_pal(sf_data$di_livlinet_cut)
sf_d_mid_pal <- d_pal(sf_data$di_midnet_cut)
sf_d_high_pal <- d_pal(sf_data$di_high_cut)

wa_d_vli_pal <- d_pal(wa_data$di_vlinet_cut)
wa_d_li_pal <- d_pal(wa_data$di_linet_cut)
wa_d_livli_pal <- d_pal(wa_data$di_livlinet_cut)
wa_d_mid_pal <- d_pal(wa_data$di_midnet_cut)
wa_d_high_pal <- d_pal(wa_data$di_high_cut)

c_pal <- function(data){
  colorFactor(
    c("#92c5de",
      "#f7f7f7",
      "#f4a582",
      "#ca0020"),
    domain = data,
    na.color = "transparent")
}

chi_c_pal <- c_pal(chi_data$c_cut)
sf_c_pal <- c_pal(sf_data$c_cut)
wa_c_pal <- c_pal(wa_king_covid$c_cut)

u_pal <- function(data){
  colorFactor(c(
    "#ffffb2", 
    "#fecc5c",
    "#fd8d3c",
    "#f03b20",
    "#bd0026"
    ),
    domain = data,
    na.color = "transparent")
}

chi_u_pal <- u_pal(chi_data$u_cut)
sf_u_pal <- u_pal(sf_data$u_cut)
wa_u_pal <- u_pal(wa_data$u_cut)

#
# Map
# --------------------------------------------------------------------------

map <- leaflet(options = leafletOptions(title = "HPRM", zoomControl = TRUE)) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }", 
    "function(el, x) {
        L.control.zoom({ position: 'topleft' }).addTo(this)
    }") %>% 
  # addControl(title, position = "bottomleft") %>% 
  addLayersControl(
    position = 'topright', 
    baseGroups = c(
      'Net Migration VLI', 
      'Net Migration LI', 
      'Eviction Risk',      
      'Avg Unemp Rate',
      'COVID Risk'), 
    options = layersControlOptions(collapsed = FALSE)) %>% 
    addMapPane(name = "polygons", zIndex = 410) %>% 
    addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>% # see: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    # htmlwidgets::onRender() %>% 
    addEasyButton(
      easyButton(
          icon="fa-crosshairs", 
          title="My Location",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  addSearchOSM() 

### Polygon function

  # poly_fun <- function(data, group, label1, label2, color, pal, cut, ...){
  # map %>% addPolygons(
  #       data = data, 
  #       group = group, 
  #       label = ~paste0(label1, round(label2, 2)), 
  #       labelOptions = labelOptions(textsize = "12px"), 
  #       fillOpacity = .5, 
  #       color = color, 
  #       stroke = TRUE, 
  #       weight = 1, 
  #       opacity = .3, 
  #       highlightOptions = 
  #         highlightOptions(
  #           color = "#ff4a4a", 
  #           weight = 5,
  #           bringToFront = TRUE), 
  #         # popup = ~popup, 
  #         popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  #       ) %>%   
  #     addLegend( 
  #       data = data, 
  #         position = 'topright',
  #         pal = pal, 
  #         values = ~cut, 
  #         group = group,
  #         title = group
  #     ) 
  # }


  # poly_fun <- function(map_input, data, group, label, color, pal, ...) {
  #   addPolygons(
  #       data = data, 
  #       group = group, 
  #       label = label, 
  #       labelOptions = labelOptions(textsize = "12px"), 
  #       fillOpacity = .5, 
  #       color = color, 
  #       stroke = TRUE, 
  #       weight = 1, 
  #       opacity = .3, 
  #       highlightOptions = 
  #         highlightOptions(
  #           color = "#ff4a4a", 
  #           weight = 5,
  #           bringToFront = TRUE), 
  #         # popup = ~popup, 
  #         popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  #       ) %>%   
  #     addLegend( 
  #       data = data, 
  #         position = 'topright',
  #         pal = pal, 
  #         values = ~di_vlinet_cut, 
  #         group = group,
  #         title = group
  #     ) 
  # }


  # map %>% 
  #   poly_fun(
  #     data = sf_data, 
  #     group = "Net Migration VLI", 
  #     label = ~paste0("Net Migration: ", round(net_very_low_adj, 2)), 
  #     color = ~d_vli_pal(di_vlinet_cut),
  #     pal = d_vli_pal, 
  #     cut = di_vlinet_cut
  #     ) ## maybe delete

map %>% 
# VLI Disp
    addPolygons(
      data = sf_data, 
      group = "Net Migration VLI", 
      label = ~paste0("Net Migration: ", round(net_very_low_adj, 2)), 
      labelOptions = labelOptions(textsize = "12px"), 
      fillOpacity = .5, 
      color = ~sf_d_vli_pal(di_vlinet_cut), 
      stroke = TRUE, 
      weight = 1, 
      opacity = .3, 
    highlightOptions = highlightOptions(
                            color = "#ff4a4a", 
                            weight = 5,
                            bringToFront = TRUE
                            ), 
        # popup = ~popup, 
        popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
      ) %>%   
    addLegend( 
      data = sf_data, 
        position = 'topright',
        pal = sf_d_vli_pal, 
        values = ~di_vlinet_cut, 
        group = "Net Migration VLI",
        title = "Net Migration VLI"
    ) %>%     
# LI Disp
    addPolygons(
      data = sf_data, 
      group = "Net Migration LI", 
      label = ~paste0("Net Migration: ", round(net_low_adj, 2)), 
      labelOptions = labelOptions(textsize = "12px"), 
      fillOpacity = .5, 
      color = ~sf_d_vli_pal(di_linet_cut), 
      stroke = TRUE, 
      weight = 1, 
      opacity = .3, 
    highlightOptions = highlightOptions(
                            color = "#ff4a4a", 
                            weight = 5,
                            bringToFront = TRUE
                            ), 
        # popup = ~popup, 
        popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
      ) %>%   
    addLegend( 
      data = sf_data, 
        position = 'topright',
        pal = sf_d_li_pal, 
        values = ~di_linet_cut, 
        group = "Net Migration LI",
        title = "Net Migration LI"
    ) %>% 
# Eviction risk
    addPolygons(
      data = sf_data, 
      group = "Eviction Risk", 
      label = ~paste0("E Risk: ", round(ev_rr, 2), " | E Rate: ", scales::percent(ev_rate, accuracy = .1)), 
      labelOptions = labelOptions(textsize = "12px"), 
      fillOpacity = .5, 
      color = ~sf_e_pal(ev_cut), 
      stroke = TRUE, 
      weight = 1, 
      opacity = .3, 
    highlightOptions = highlightOptions(
                            color = "#ff4a4a", 
                            weight = 5,
                            bringToFront = TRUE
                            ), 
        # popup = ~popup, 
        popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
      ) %>%   
    addLegend( 
      data = sf_data, 
        position = 'topright',
        pal = sf_e_pal, 
        values = ~ev_cut, 
        group = "Eviction Risk",
        title = "Eviction Risk"
    ) %>% 
# Avg Unemp rate
    addPolygons(
      data = sf_data, 
      group = "Avg Unemp Rate", 
      label = ~paste0("Unemp Rate: ", scales::percent(unemp_rate, accuracy = .1)), 
      labelOptions = labelOptions(textsize = "12px"), 
      fillOpacity = .5, 
      color = ~sf_u_pal(u_cut), 
      stroke = TRUE, 
      weight = 1, 
      opacity = .3, 
    highlightOptions = highlightOptions(
                            color = "#ff4a4a", 
                            weight = 5,
                            bringToFront = TRUE
                            ), 
        # popup = ~popup, 
        popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
      ) %>%   
    addLegend( 
      data = sf_data, 
        position = 'topright',
        pal = sf_u_pal, 
        values = ~u_cut, 
        group = "Avg Unemp Rate",
        title = "Avg Unemp Rate"
    ) %>%
  # Covid
  addPolygons(
    data = sf_covid, 
    group = "COVID Risk", 
    label = ~paste0("Covid Rate: ", scales::percent(rate, accuracy = .1)), 
    labelOptions = labelOptions(textsize = "12px"), 
    fillOpacity = .5, 
    color = ~sf_c_pal(c_cut), 
    stroke = TRUE, 
    weight = 1, 
    opacity = .3, 
    highlightOptions = highlightOptions(
      color = "#ff4a4a", 
      weight = 5,
      bringToFront = TRUE
    ), 
    # popup = ~popup, 
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%   
  addLegend( 
    data = sf_covid, 
    position = 'topright',
    pal = sf_c_pal, 
    values = ~c_cut, 
    group = "Covid Risk",
    title = "Covid Risk"
  )
}

sf_map <- mapping(sf_data, sf_covid)
chi_map <- mapping(chi_data, il_covid)

# ==========================================================================
# ==========================================================================
# ==========================================================================
# 
# ==========================================================================
# ==========================================================================
# ==========================================================================



#
# Functions
# --------------------------------------------------------------------------





#
# WA
# --------------------------------------------------------------------------

# TODO
#   * student pop

# Examine sd and ratio thread

# VLI spread
disp %>% group_by(vli_exit_sd, vli_exit) %>% count() %>% summarize(vli_exit_sd = round(vli_exit_sd, 2), vli_exit = round(vli_exit, 2)) %>% distinct() %>% data.frame()
# -1.3sd = 0
# -1sd = .2
# 0sd = .85
# 1sd = 1.5
# 2sd = 2.15
# 3sd = 2.8
# max = 30 = 21

# LI spread
disp %>% group_by(li_exit_sd, li_exit) %>% count() %>% summarize(li_exit_sd = round(li_exit_sd, 2), li_exit = round(li_exit, 2)) %>% distinct() %>% data.frame()
# -1sd = 0.04
# 0sd = 1.17
# 1sd = 2.32
# 2sd = 2.15
# 3sd = 2.8
# max = 30 = 21


disp %>% group_by(vli_exit, vli_exit_sd) %>% count() %>% summarize(vli_exit = round(vli_exit, 2), vli_exit_sd = round(vli_exit_sd, 2)) %>% distinct() %>% data.frame()

# Notes:
# vli_exit

# US_tracts_sf <- 
#   tracts()
created on a new gdal version.
popup = c("Tract FIPS" = 'GEOID', 'HH',
                  'p_vli',
                  'p_li',
                  'p_mi',
                  'p_hi',
                  "net_high",
                  "net_middle",
                  "net_low",
                  "net_very_low",
                  "net_livli",
                  "HH_very_low",
                  "MOVE_OUT_very_low",
                  "MOVE_IN_very_low",
                  "MOVE_WITHIN_very_low",
                  "vli_exit",
                  "p_vli_exit",
                  "vli_in",
                  "p_vli_in",
                  "HH_low",
                  "MOVE_OUT_low",
                  "MOVE_IN_low",
                  "MOVE_WITHIN_low",                  
                  "li_exit",
                  "p_li_exit",
                  "li_in",
                  "p_li_in",
                  "livli_exit",
                  "p_livli_exit",
                  "livli_in",
                  "p_livli_in",
                  "HH_middle",
                  "MOVE_OUT_middle",
                  "MOVE_IN_middle",
                  "MOVE_WITHIN_middle",
                  "mi_exit",
                  "p_mi_exit",
                  "mi_in",
                  "p_mi_in",
                  "HH_high",
                  "MOVE_OUT_high",
                  "MOVE_IN_high",
                  "MOVE_WITHIN_high", 
                  "hi_exit",
                  "p_hi_exit",
                  "hi_in",
                  "p_hi_in",
                  "pr_li_in_vli_exit",
                  "pr_mi_in_li_exit",
                  "pr_mi_in_livli_exit",
                  "pr_hi_in_mi_exit",
                  "vli_exit_sd",
                  "li_exit_sd",
                  "mi_exit_sd",
                  "hi_exit_sd",
                  "li_in_vli_exit_cratio",
                  "mi_in_li_exit_cratio",
                  "mi_in_livli_exit_cratio",
                  "hi_in_mi_exit_cratio",
                  "li_in_vli_exit_rratio",
                  "mi_in_li_exit_rratio",
                  "mi_in_livli_exit_rratio",
                  "hi_in_mi_exit_rratio",
                  "li_in_r_vli_out_r_or",
                  "mi_in_r_livli_out_r_or",
                  "mi_in_r_li_out_r_or",
                  "hi_in_r_mi_out_r_or")

tmap_mode("view")
mappy <- function(vli, li, mi, hi, min, max){
  tm_shape(disp_sf) +
    tm_facets(sync = TRUE, ncol = 2) +
    tm_fill(
      alpha = .5,
      c(vli, li, mi, hi),
      style = "fixed",
      breaks = c(0, .5, 1, 1.25, 1.5, 2, min, max),
      popup.vars= popup) +
    tm_borders(alpha = .2)
}

mappy("vli_exit_sd_adj", "li_exit_sd_adj", "mi_exit_sd_adj", "hi_exit_sd_adj", min = (-3), max = (15))
mappy("li_vli_cratio_sd_adj","mi_li_cratio_sd_adj", "mi_livli_cratio_sd_adj", "hi_mi_cratio_sd_adj", min = -3, max =6)

mappy("li_vli_rratio_sd_adj","mi_li_rratio_sd_adj", "mi_livli_rratio_sd_adj", "hi_mi_rratio_sd_adj", min = -1, max =22)

mappy_rat <- function(vli, li, mi, hi, max, popup = popup){
  tm_shape(disp_sf) +
    tm_facets(sync = TRUE, ncol = 2) +
    tm_fill(
      alpha = .5,
      c(vli, li, mi, hi),
      id = "GEOID",
      style = "fixed",
      breaks = c(0, .5, 1, 1.25, 1.5, 2, max),
      popup.vars= popup,
      midpoint = 1,
      palette = "-RdBu") +
    tm_borders(alpha = .2)
}

mappy_rat("vli_exit_adj", "li_exit_adj", "mi_exit_adj", "livli_exit_adj", max = (25))
mappy_rat("li_in_r_vli_out_r_or", "mi_in_r_livli_out_r_or", "mi_in_r_li_out_r_or", "hi_in_r_mi_out_r_or", max = (1900))

  tm_shape(disp_sf) +
    tm_fill(
      alpha = .5,
      title = c("Very low income rate", "Low income rate"),
      c("vli_exit_adj", "li_exit_adj"),
      id = "GEOID",
      style = "fixed",
      breaks = c(0, .5, 1, 1.25, 1.5, 2, 15),
      popup.vars= popup,
      midpoint = 1,
      textNA = "< 5% population",
      palette = "-RdBu") +
    tm_borders(alpha = .2) + 
    tm_view(
      # view.legend.position = c("left", "bottom"),
      set.view = c(11)
      ) + 
    tm_facets(sync = TRUE, ncol = 1, nrow = 2)

io_rate <- 
  tm_shape(disp_sf) +
    tm_fill(
      alpha = .5,
      title = c("VLI out/in rate", "LI out/in rate", "LI & VLI out/in rate", 
        "MI out/in rate"),
      c("vli_exit_adj", "li_exit_adj", "livli_exit_adj", "mi_exit_adj"),
      id = "GEOID",
      style = "fixed",
      breaks = c(0, .5, 1, 1.25, 1.5, 2, 15),
      popup.vars= popup,
      midpoint = 1,
      textNA = "< 5% population",
      palette = "-RdBu") +
    tm_borders(alpha = .2) + 
    tm_view(
      # view.legend.position = c("left", "bottom"),
      set.view = c(10)
      ) + 
    tm_facets(sync = TRUE, ncol = 2)

prate_map <- 
tm_shape(disp_sf) +
    tm_fill(
      alpha = .5,
      title = c("p_li_in/p_vli_exit", "p_mi_in/p_li_exit", "p_mi_in/p_livli_exit", "p_hi_in/p_mi_exit"),
      c("pr_li_in_vli_exit_adj", "pr_mi_in_li_exit_adj", "pr_mi_in_livli_exit_adj", "pr_hi_in_mi_exit_adj"
        ),
      id = "GEOID",
      style = "fixed",
      breaks = c(0, .5, 1, 1.25, 1.5, 2, 15),
      popup.vars= popup,
      midpoint = 1,
      textNA = "< 5% population",
      palette = "-RdBu") +
    tm_borders(alpha = .2) + 
    tm_view(
      # view.legend.position = c("left", "bottom"),
      set.view = c(11)
      ) + 
    tm_facets(sync = TRUE, ncol = 2)

net_map <- 
tm_shape(disp_sf) +
    tm_fill(
      alpha = .5,
      title = c("net_very_low", "net_low", "net_middle", "net_high"),
      c("net_very_low_asj", "net_low_asj", "net_middle_asj", "net_high_asj"),
      id = "GEOID",
      style = "fixed",
      breaks = c(-1001, -40, 0, 40, 35000),
      popup.vars= popup,
      midpoint = 0,
      textNA = "< 5% population",
      palette = "RdBu") +
    tm_borders(alpha = .2) + 
    tm_view(
      # view.legend.position = c("left", "bottom"),
      set.view = c(11)
      ) + 
    tm_facets(sync = TRUE, ncol = 2)

tmap_save(map1, "~/git/hprm/maps/displacement/dcrp_pres_map.html")
tmap_save(io_rate, "~/git/hprm/maps/displacement/io_rate_map.html")
tmap_save(prate_map, "~/git/hprm/maps/displacement/prate_map.html")
# rratio doesn't work because when dividing from 0.x and 0.y, you get greater than 1 values.

# ==========================================================================
# output
# ==========================================================================

disp <-
  # ig53 %>%
  ig06 %>%
  mutate(YEARS = YEAR,
         YEAR = as.numeric(str_sub(YEARS, -4, -1)),
  ) %>%
  filter(YEAR %in% c(2015, 2016, 2017, 2018, 2019)) %>%
  group_by(GEOID, CAT) %>%
  summarize(
    HH = sum(HH, na.rm = TRUE),
    MOVE_OUT = sum(MOVE_OUT, na.rm = TRUE),
    MOVE_IN = sum(MOVE_IN, na.rm = TRUE),
    MOVE_WITHIN = sum(MOVE_WITHIN, na.rm = TRUE),
    YEARS = "2015-2019"
  ) %>%
  pivot_wider(names_from = CAT, values_from = HH:MOVE_WITHIN) %>%
  mutate(
    MOVE_IN_livli = sum(MOVE_IN_low, MOVE_IN_very_low, na.rm = TRUE),
    MOVE_OUT_livli = sum(MOVE_OUT_low, MOVE_OUT_very_low, na.rm = TRUE),
    HH_livli = sum(HH_low, HH_very_low, na.rm = TRUE),
    HH = sum(HH_high, HH_low, HH_middle, HH_very_low, na.rm = TRUE),
    p_vli = HH_very_low/HH,
    p_li = HH_low/HH,
    p_livli = HH_livli/HH,
    p_mi = HH_middle/HH,
    p_hi = HH_high/HH,
    p_vli_exit = MOVE_OUT_very_low/HH_very_low,
    p_li_exit = MOVE_OUT_low/HH_low,
    p_livli_exit = MOVE_OUT_livli/HH_livli,
    p_mi_exit = MOVE_OUT_middle/HH_middle,
    p_hi_exit = MOVE_OUT_high/HH_high,
    p_vli_in = MOVE_IN_very_low/HH_very_low,
    p_li_in = MOVE_IN_low/HH_low,
    p_livli_in = MOVE_IN_livli/HH_livli,
    p_mi_in = MOVE_IN_middle/HH_middle,
    p_hi_in = MOVE_IN_high/HH_high,
    pr_li_in_vli_exit = p_li_in/p_vli_exit, 
    pr_mi_in_li_exit = p_mi_in/p_li_exit, 
    pr_mi_in_livli_exit = p_mi_in/p_livli_exit, 
    pr_hi_in_mi_exit = p_mi_in/p_livli_exit, 
    vli_exit =
      case_when(
        (MOVE_OUT_very_low == 0 & MOVE_IN_very_low == 0) ~ NA_real_,
        (MOVE_OUT_very_low > 0 & MOVE_IN_very_low == 0) ~
          (MOVE_OUT_very_low + 1)/(MOVE_IN_very_low + 1),
        TRUE ~ MOVE_OUT_very_low/MOVE_IN_very_low),
    li_exit =
      case_when(
        (MOVE_OUT_low == 0 & MOVE_IN_low == 0) ~ NA_real_,
        (MOVE_OUT_low > 0 & MOVE_IN_low == 0) ~
          (MOVE_OUT_low + 1)/(MOVE_IN_low + 1),
        TRUE ~ MOVE_OUT_low/MOVE_IN_low),
    livli_exit =
      case_when(
        (MOVE_OUT_livli == 0 & MOVE_IN_livli == 0) ~ NA_real_,
        (MOVE_OUT_livli > 0 & MOVE_IN_livli == 0) ~
          (MOVE_OUT_livli + 1)/(MOVE_IN_livli + 1),
        TRUE ~ MOVE_OUT_livli/MOVE_IN_livli),
    mi_exit =
      case_when(
        (MOVE_OUT_middle == 0 & MOVE_IN_middle == 0) ~ NA_real_,
        (MOVE_OUT_middle > 0 & MOVE_IN_middle == 0) ~
          (MOVE_OUT_middle + 1)/(MOVE_IN_middle + 1),
        TRUE ~ MOVE_OUT_middle/MOVE_IN_middle),
    hi_exit =
      case_when(
        (MOVE_OUT_high == 0 & MOVE_IN_high == 0) ~ NA_real_,
        (MOVE_OUT_high > 0 & MOVE_IN_high == 0) ~
          (MOVE_OUT_high + 1)/(MOVE_IN_high + 1),
        TRUE ~ MOVE_OUT_high/MOVE_IN_high),
    vli_in =
      case_when(
        (MOVE_IN_very_low == 0 & MOVE_OUT_very_low == 0) ~ NA_real_,
        (MOVE_IN_very_low > 0 & MOVE_OUT_very_low == 0) ~
          (MOVE_IN_very_low + 1)/(MOVE_OUT_very_low + 1),
        TRUE ~ MOVE_IN_very_low/MOVE_OUT_very_low),
    li_in =
      case_when(
        (MOVE_IN_low == 0 & MOVE_OUT_low == 0) ~ NA_real_,
        (MOVE_IN_low > 0 & MOVE_OUT_low == 0) ~
          (MOVE_IN_low + 1)/(MOVE_OUT_low + 1),
        TRUE ~ MOVE_IN_low/MOVE_OUT_low),
    livli_in =
      case_when(
        (MOVE_IN_livli == 0 & MOVE_OUT_livli == 0) ~ NA_real_,
        (MOVE_IN_livli > 0 & MOVE_OUT_livli == 0) ~
          (MOVE_IN_livli + 1)/(MOVE_OUT_livli + 1),
        TRUE ~ MOVE_IN_livli/MOVE_OUT_livli),
    mi_in =
      case_when(
        (MOVE_IN_middle == 0 & MOVE_OUT_middle == 0) ~ NA_real_,
        (MOVE_IN_middle > 0 & MOVE_OUT_middle == 0) ~
          (MOVE_IN_middle + 1)/(MOVE_OUT_middle + 1),
        TRUE ~ MOVE_IN_middle/MOVE_OUT_middle),
    hi_in =
      case_when(
        (MOVE_IN_high == 0 & MOVE_OUT_high == 0) ~ NA_real_,
        (MOVE_IN_high > 0 & MOVE_OUT_high == 0) ~
          (MOVE_IN_high + 1)/(MOVE_OUT_high + 1),
        TRUE ~ MOVE_IN_high/MOVE_OUT_high),
    li_in_vli_exit_cratio =
      case_when(
        (MOVE_IN_low == 0 & MOVE_OUT_very_low == 0) ~ NA_real_,
        (MOVE_IN_low > 0 & MOVE_OUT_very_low == 0) ~
          (MOVE_IN_low + 1)/(MOVE_OUT_very_low + 1),
        TRUE ~ MOVE_IN_low/MOVE_OUT_very_low),
    mi_in_li_exit_cratio =
      case_when(
        (MOVE_IN_middle == 0 & MOVE_OUT_low == 0) ~ NA_real_,
        (MOVE_IN_middle > 0 & MOVE_OUT_low == 0) ~
          (MOVE_IN_middle + 1)/(MOVE_OUT_low + 1),
        TRUE ~ MOVE_IN_middle/MOVE_OUT_low),
    mi_in_livli_exit_cratio =
      case_when(
        (MOVE_IN_middle == 0 & MOVE_OUT_livli == 0) ~ NA_real_,
        (MOVE_IN_middle > 0 & MOVE_OUT_livli == 0) ~
          (MOVE_IN_middle + 1)/(MOVE_OUT_livli + 1),
        TRUE ~ MOVE_IN_middle/MOVE_OUT_livli),
    hi_in_mi_exit_cratio =
      case_when(
        (MOVE_IN_high == 0 & MOVE_OUT_middle == 0) ~ NA_real_,
        (MOVE_IN_high > 0 & MOVE_OUT_middle == 0) ~
          (MOVE_IN_high + 1)/(MOVE_OUT_middle + 1),
        TRUE ~ MOVE_IN_high/MOVE_OUT_middle),
    li_in_vli_exit_rratio =
      case_when(
        (is.na(li_in) | is.na(vli_exit)) ~ NA_real_,
        (li_in > 0 & vli_exit == 0) ~
          (li_in + 1)/(vli_exit + 1),
        TRUE ~ li_in/vli_exit
      ),
    mi_in_livli_exit_rratio =
      case_when(
        (is.na(mi_in) | is.na(livli_exit)) ~ NA_real_,
        (mi_in > 0 & livli_exit == 0) ~
          (mi_in + 1)/(livli_exit + 1),
        TRUE ~ mi_in/livli_exit
      ),
    mi_in_li_exit_rratio =
      case_when(
        (is.na(mi_in) | is.na(li_exit)) ~ NA_real_,
        (mi_in > 0 & li_exit == 0) ~
          (mi_in + 1)/(li_exit + 1),
        TRUE ~ mi_in/li_exit
      ),
    hi_in_mi_exit_rratio =
      case_when(
        (is.na(hi_in) | is.na(mi_exit)) ~ NA_real_,
        (hi_in > 0 & mi_exit == 0) ~
          (hi_in + 1)/(mi_exit + 1),
        TRUE ~ hi_in/mi_exit
      )
    ) %>%
  ungroup() %>%
  mutate(
    vli_exit_sd = scale_this(vli_exit),
    li_exit_sd = scale_this(li_exit),
    mi_exit_sd = scale_this(mi_exit),
    hi_exit_sd = scale_this(hi_exit),
    li_vli_cratio_sd = scale_this(li_in_vli_exit_cratio),
    mi_li_cratio_sd = scale_this(mi_in_li_exit_cratio),
    mi_livli_cratio_sd = scale_this(mi_in_livli_exit_cratio),
    hi_mi_cratio_sd = scale_this(hi_in_mi_exit_cratio),
    li_vli_rratio_sd = scale_this(li_in_vli_exit_rratio),
    mi_li_rratio_sd = scale_this(mi_in_li_exit_rratio),
    mi_livli_rratio_sd = scale_this(mi_in_livli_exit_rratio),
    hi_mi_rratio_sd = scale_this(hi_in_mi_exit_rratio),
    vli_exit_adj = case_when((p_vli < .05 | sum(MOVE_OUT_very_low, MOVE_IN_very_low, na.rm = TRUE)/HH_very_low < 0.5 ~ NA_real_), TRUE ~ vli_exit),
    li_exit_adj = case_when((p_li < .05 | sum(MOVE_OUT_low, MOVE_IN_low, na.rm = TRUE)/HH_low < 0.5~ NA_real_), TRUE ~ li_exit),
    livli_exit_adj = case_when((p_livli < .05 | sum(MOVE_OUT_livli, MOVE_IN_livli, na.rm = TRUE)/HH_middle < 0.5 ~ NA_real_), TRUE ~ livli_exit),    
    mi_exit_adj = case_when((p_mi < .05 | sum(MOVE_OUT_middle, MOVE_IN_middle, na.rm = TRUE)/HH_middle < 0.5 ~ NA_real_), TRUE ~ mi_exit),
    hi_exit_adj = case_when((p_hi < .05 | sum(MOVE_OUT_high, MOVE_IN_high, na.rm = TRUE)/HH_high < .05 ~ NA_real_), TRUE ~ hi_exit),
    vli_exit_sd_adj = case_when(p_vli < .05 ~ NA_real_, TRUE ~ vli_exit_sd),
    li_exit_sd_adj = case_when(p_li < .05 ~ NA_real_, TRUE ~ li_exit_sd),
    mi_exit_sd_adj = case_when(p_mi < .05 ~ NA_real_, TRUE ~ mi_exit_sd),
    hi_exit_sd_adj = case_when(p_hi < .05 ~ NA_real_, TRUE ~ hi_exit_sd),
    li_vli_cratio_sd_adj =
      case_when(
        (p_li < .05 | p_vli < .05) ~ NA_real_,
         TRUE ~ li_vli_cratio_sd),
    mi_li_cratio_sd_adj =
      case_when(
        (p_mi < .05 | p_li < .05) ~ NA_real_,
         TRUE ~ mi_li_cratio_sd),
    mi_livli_cratio_sd_adj =
      case_when(
        (p_mi < .05 | p_livli < .05) ~ NA_real_,
         TRUE ~ mi_livli_cratio_sd),
    hi_mi_cratio_sd_adj =
      case_when(
        (p_hi < .05 | p_mi < .05) ~ NA_real_,
         TRUE ~ hi_mi_cratio_sd),
    li_vli_rratio_sd_adj =
      case_when(
        (p_li < .05 | p_vli < .05) ~ NA_real_,
         TRUE ~ li_vli_rratio_sd),
    mi_li_rratio_sd_adj =
      case_when(
        (p_mi < .05 | p_li < .05) ~ NA_real_,
         TRUE ~ mi_li_rratio_sd),
    mi_livli_rratio_sd_adj =
      case_when(
        (p_mi < .05 | p_livli < .05) ~ NA_real_,
         TRUE ~ mi_livli_rratio_sd),
    hi_mi_rratio_sd_adj =
      case_when(
        (p_hi < .05 | p_mi < .05) ~ NA_real_,
         TRUE ~ hi_mi_rratio_sd),
    li_in_r_vli_out_r_or =
      case_when(
        (MOVE_OUT_very_low*MOVE_OUT_low) == 0 ~
          (MOVE_IN_low*MOVE_IN_very_low)/1,
        TRUE ~
        (MOVE_IN_low*MOVE_IN_very_low)/
        (MOVE_OUT_very_low*MOVE_OUT_low)
      ),
    mi_in_r_livli_out_r_or =
      case_when(
        (MOVE_OUT_livli*MOVE_OUT_middle) == 0 ~
          (MOVE_IN_middle*MOVE_IN_livli)/1,
        TRUE ~
        (MOVE_IN_middle*MOVE_IN_livli)/
        (MOVE_OUT_livli*MOVE_OUT_middle)
      ),
    mi_in_r_li_out_r_or =
      case_when(
        (MOVE_OUT_low*MOVE_OUT_middle) == 0 ~
          (MOVE_IN_middle*MOVE_IN_low)/1,
        TRUE ~
        (MOVE_IN_middle*MOVE_IN_low)/
        (MOVE_OUT_low*MOVE_OUT_middle)
      ),
    hi_in_r_mi_out_r_or =
      case_when(
        (MOVE_OUT_middle*MOVE_OUT_high) == 0 ~
          (MOVE_IN_high*MOVE_IN_middle)/1,
        TRUE ~
          (MOVE_IN_high*MOVE_IN_middle)/
          (MOVE_OUT_middle*MOVE_OUT_high)), 
    pr_li_in_vli_exit_adj = 
      case_when(p_vli < 0.05 | p_li < 0.05 ~ NA_real_, TRUE ~ pr_li_in_vli_exit),
    pr_mi_in_li_exit_adj = 
      case_when(p_li < 0.05 | p_mi < 0.05 ~ NA_real_, TRUE ~ pr_mi_in_li_exit),
    pr_mi_in_livli_exit_adj = 
      case_when(p_livli < 0.05 | p_mi < 0.05 ~ NA_real_, TRUE ~ pr_mi_in_livli_exit),
    pr_hi_in_mi_exit_adj = 
      case_when(p_mi < 0.05 | p_hi < 0.05 ~ NA_real_, TRUE ~ pr_hi_in_mi_exit),
    net_high = ((MOVE_IN_high - MOVE_OUT_high)/HH_high)*1000, 
    net_middle = ((MOVE_IN_middle - MOVE_OUT_middle)/HH_middle)*1000, 
    net_low = ((MOVE_IN_low - MOVE_OUT_low)/HH_low)*1000, 
    net_very_low = ((MOVE_IN_very_low - MOVE_OUT_very_low)/HH_very_low)*1000, 
    net_livli = ((MOVE_IN_livli - MOVE_OUT_livli)/HH_livli)*1000
    )


# ==========================================================================
# Map
# ==========================================================================

 %>%

map <-
  leaflet(
    data = df_tr,
    options = leafletOptions(zoomControl = FALSE, title = "Affordable Neighborhoods")
  ) %>% # hide legend when using baseGroups
  addLayersControl(
    position = 'topright',
    baseGroups = c(
      # overlayGroups = c(
      "covid",
      "evictions",
      "unemployment"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addEasyButton(
    easyButton(
      position = "topright",
      icon="fa-crosshairs",
      title="My Location",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>%
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  ## evictions
  addPolygons(
    data = df_tr %>% filter(!is.na(e_cut)),
    group = "evictions",
    label = ~str_c("ID: ", GEOID, " eviction RR: ", Evictions_RR),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~e_pal(e_cut),
    stroke = TRUE,
    weight = 1,
    # opacity = .8,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    )) %>%
  addLegend(
    # position = 'bottomright',
    data = df_tr %>% filter(!is.na(e_cut)),
    pal = e_pal,
    values = ~e_cut,
    group = "evictions",
    title = "evictions") %>%
  ## covid
  addPolygons(
    data = df_tr %>% filter(!is.na(c_cut)),
    group = "covid",
    label = ~str_c("ID: ", GEOID, " covid RR: ", Overall_COVID_RR),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~c_pal(c_cut),
    stroke = TRUE,
    weight = 1,
    # opacity = .8,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    )) %>%
  addLegend(
    # position = 'bottomright',
    data = df_tr %>% filter(!is.na(c_cut)),
    pal = c_pal,
    values = ~c_cut,
    group = "covid",
    title = "covid") %>%
  ## unemployment
  addPolygons(
    data = df_tr %>% filter(!is.na(u_cut)),
    group = "unemployment",
    label = ~str_c("ID: ", GEOID, " unemployment rate: ", scales::percent(unemp_rate)),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .8,
    color = ~u_pal(u_cut),
    stroke = TRUE,
    weight = 1,
    # opacity = .8,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    )#,
    # popup = ~popup
  ) %>%
  addLegend(
    # position = 'bottomright',
    data = df_tr %>% filter(!is.na(u_cut)),
    pal = u_pal,
    values = ~u_cut,
    group = "covid",
    title = "covid")
map

htmlwidgets::saveWidget(seattle, file="/Users/timthomas/git/hprm/maps/phase1.html")

#### END FIX ####
li_vli_cratio =
  case_when(vli_exit == 0 ~
              (li_exit + 1)/(vli_exit + 1),
            TRUE ~ li_exit/vli_exit),
mi_li_cratio =
  case_when(li_exit == 0 ~
              (mi_exit + 1)/(li_exit + 1),
            TRUE ~ mi_exit/li_exit),
hi_mi_cratio =
  case_when(mi_exit == 0 ~
              (hi_exit + 1)/(mi_exit + 1),
            TRUE ~ hi_exit/mi_exit),
vli_o = MOVE_OUT_very_low,
vli_p = sum(MOVE_OUT_very_low, MOVE_IN_very_low, na.rm = TRUE),
li_o = MOVE_OUT_low,
li_p = sum(MOVE_OUT_low, MOVE_IN_low, na.rm = TRUE),
mi_o = MOVE_OUT_middle,
mi_p = sum(MOVE_OUT_middle, MOVE_IN_middle, na.rm = TRUE),
hi_o = MOVE_OUT_high,
hi_p = sum(MOVE_OUT_high, MOVE_IN_high, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    vli_r = sum(vli_o, na.rm = TRUE)/sum(vli_p, na.rm = TRUE),
    li_r = sum(li_o, na.rm = TRUE)/sum(li_p, na.rm = TRUE),
    mi_r = sum(mi_o, na.rm = TRUE)/sum(mi_p, na.rm = TRUE),
    hi_r = sum(hi_o, na.rm = TRUE)/sum(hi_p, na.rm = TRUE),
    vli_e = vli_p * vli_r,
    li_e = li_p * li_r,
    mi_e = mi_p * mi_r,
    hi_e = hi_p * hi_r,
    vli_smr = vli_o/vli_e,
    li_smr = li_o/li_e,
    mi_smr = mi_o/mi_e,
    hi_smr = hi_o/hi_e) %>%
  # replace(is.na(.), 0)
  group_by(GEOID) %>%
  mutate(
    tot_hh = sum(HH_high, HH_middle, HH_low, HH_very_low),
    p_very_low = HH_very_low/tot_hh,
    p_low = HH_low/tot_hh,
    p_middle = HH_middle/tot_hh,
    p_high = HH_high/tot_hh) %>%
  ungroup()

pois.daly(disp$vli_o, disp$vli_p) %>% summary()
pois.exact(disp$vli_o, disp$vli_p) %>% glimpse()
glimpse(disp)

# LEFT OFF
# No lower bound CI is above 1. Should consider more standard means.
# Compare to exit vars in DT again.
# Try MI Displacement over LI Displacement

#      %>%
# left_join(read_csv("/Users/timthomas/git/exit-typologies/data/downloads_for_public/sanfrancisco.csv"))

disp2 <-
  disp %>%
  filter(tot_hh > quantile(.$tot_hh, .05))

summary(disp)
summary(disp2)
glimpse(disp2)

ggplot(disp2, aes(hi_exit)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = 1))

ggplot(disp2 %>% filter(li_exit < 4.5, mi_exit < 4.5,
                        hi_exit < 4.5), aes(li_exit)) +
  geom_histogram(bins = 100) +
  geom_histogram(aes(mi_exit), bins = 100, color = "red", alpha = .5) +
  geom_histogram(aes(hi_exit), bins = 100, color = "blue", alpha = .5) +
  geom_histogram(aes(vli_exit), bins = 100, color = "orange", alpha = .5) +
  geom_vline(aes(xintercept = 1), color = "red")

tracts <- readRDS("data/census/US_tracts_sf.rds")
disp_sf <-
  right_join(tracts, disp2)
st_crs(disp_sf) <- 4269 # require transform becuase gdal may be old and was created on a new gdal version.

tmap_mode("plot")
tm_shape(disp_sf) +
  tm_fill(
    c("vli_exit", "li_exit", "mi_exit", "hi_exit"),
    # n = 6,
    style = "jenks",
    # breaks = c(-.9, -.5, -.25, -.05, .05, .25, .5, .9)
  ) +
  tm_borders(alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"))

# interactive
tmap_mode("view")
tm_shape(disp_sf) +
  tm_facets(sync = TRUE, ncol = 2) +
  tm_fill(
    c("vli_exit", "li_exit", "mi_exit", "hi_exit"),
    # n = 6,
    style = "fixed",
    breaks = c(0,.5,1,1.1, 1.5,11),
    palette = "RdBu", # - reverses palette,
    alpha = .65,
    popup.vars=
      c(
        "vli_exit",
        "li_exit",
        "mi_exit",
        "hi_exit",
        "HH_very_low",
        "MOVE_OUT_very_low",
        "MOVE_IN_very_low",
        "MOVE_WITHIN_very_low",

        "HH_low",
        "MOVE_OUT_low",
        "MOVE_IN_low",
        "MOVE_WITHIN_low",

        "HH_middle",
        "MOVE_OUT_middle",
        "MOVE_IN_middle",
        "MOVE_WITHIN_middle",

        "HH_high",
        "MOVE_OUT_high",
        "MOVE_IN_high",
        "MOVE_WITHIN_high")) +
  tm_borders(alpha = .2)


tm_shape(disp_sf) +
  tm_facets(sync = TRUE, ncol = 2) +
  tm_fill(
    c("vli_exit", "li_exit", "mi_exit", "hi_exit"),
    # n = 6,
    style = "fixed",
    breaks = c(0,.5,1,1.1, 1.5,11),
    palette = "-RdBu", # - reverses palette,
    alpha = .65,
    popup.vars=
      c("vli_smr", "li_smr", "mi_smr", "hi_smr",
        "vli_exit",
        "li_exit",
        "mi_exit",
        "hi_exit",
        "HH_very_low",
        "MOVE_OUT_very_low",
        "MOVE_IN_very_low",
        "MOVE_WITHIN_very_low",

        "HH_low",
        "MOVE_OUT_low",
        "MOVE_IN_low",
        "MOVE_WITHIN_low",

        "HH_middle",
        "MOVE_OUT_middle",
        "MOVE_IN_middle",
        "MOVE_WITHIN_middle",

        "HH_high",
        "MOVE_OUT_high",
        "MOVE_IN_high",
        "MOVE_WITHIN_high")) +
  tm_borders(alpha = .2)

tm_shape(disp_sf) +
  tm_facets(sync = TRUE, ncol = 2) +
  tm_fill(
    c("li_vli_cratio", "mi_li_cratio", "hi_mi_cratio"),
    # n = 6,
    style = "fixed",
    breaks = c(0,.5,1,1.1, 1.5,11),
    palette = "-RdBu", # - reverses palette,
    alpha = .65) +
  tm_borders(alpha = .2)

tmap_tr <- tm_shape(disp_sf %>% filter(GEOID == "06075061500")) + tm_polygons()
tmap_mode("view")
tmap_tr



### left off
disp2 %>%
  filter(Typology == "Ongoing Displacement") %>%
  glimpse()
%>%
  filter(li_exit < 1) %>% arrange(li_replacement) %>%
  # filter(HH_high > 10000) %>% glimpse()
  filter(mi_li_cratio == Inf) %>%
  summary()

#
# Tracts
#

# evictions %>%
# summarize(states <- substr(GEOID, 1, 2)) %>%
# distinct() %>%
# pull()
# ==========================================================================
# Begin Download
# ==========================================================================
# us_states <- states() %>% pull(STATEFP) %>% unique()

# us_tracts <-
# 	map_df(us_states, function(state){
# 		tracts(state = state, cb = TRUE)
# 		}) %>%
#     ms_simplify(keep = 0.5)
# #
# saveRDS(tracts, "/Volumes/GoogleDrive/My Drive/CCI Docs/Current Projects/HPRM/Data/raw/census/US_tracts_sf.rds")
# ==========================================================================
# End Download
# ==========================================================================
tracts <- readRDS("/Users/timthomas/Google Drive/CCI Docs/Current Projects/HPRM/Data/raw/census/US_tracts_sf.rds")

# ==========================================================================
# Displacement iterations
# ==========================================================================

#
# i1 - ratios of li and mi and hi change over time
# --------------------------------------------------------------------------

# Looking at 5 year interval
# left off
year5 <-

  ig %>%
  filter(YEAR %in% c(2015, 2016, 2017, 2018, 2019)) %>% arrange(CAT) %>% data.frame()
group_by(GEOID, CAT) %>%
  summarize(
    TOT_HH = mean(HH, na.rm = TRUE),
    MO_19 = sum(MOVED_OUT, na.rm = TRUE),
    MI_19 = sum(MOVED_IN, na.rm = TRUE),
    MW_19 = sum(MOVED_WITHIN, na.rm = TRUE),
    mo_mi_cratio = MO_19/MI_19) %>%
  glimpse()

tmap_tr <- tm_shape(tracts %>% filter(GEOID == "06075061500")) + tm_polygons()
tmap_mode("view")
tmap_tr


summarize(
  MO_19 =
    case_when(YEAR %in% c(2015, 2016, 2017, 2018, 2019) ~ sum(MOVED_OUT, na.rm = TRUE)),
  MI_19 =
    case_when(YEAR %in% c(2015, 2016, 2017, 2018, 2019) ~ sum(MOVED_IN, na.rm = TRUE)),
  MW_19 =
    case_when(YEAR %in% c(2015, 2016, 2017, 2018, 2019) ~ sum(MOVED_WITHIN, na.rm = TRUE)),
  HH = case_when(YEAR %in% c(2015, 2016, 2017, 2018, 2019) ~ sum(HH, na.rm = TRUE)))

%>%
  group_by(GEOID) %>%
  mutate(HH_p = case_when(YEAR %in% c(2015, 2016, 2017, 2018, 2019) ~ HH/sum(HH, na.rm = TRUE)))

glimpse(year5)

year5 %>% select(GEOID, CAT, MO_19:MW_19, HH, HH_p) %>% distinct() %>% na.omit() %>% arrange(GEOID)

#
# Things to consider:
# 1. High counts within each category - which category is higher to define type, maybe the number of moves?
#
5yr <-
  exit_hud %>%
  filter(YEAR == 1519) %>%
  group_by(GEOID10) %>%
  mutate(moved = )

#
# Replacement idea
#   * Calculate where the rate of replacement is lower for li and higher fo mi
#

## Left off

exit_hud %>%
  # pivot_wider(names_from = CAT, values_from = HH:MOVED_WITHIN_TOT) %>%
  # mutate(
  #     li_replacement = MOVED_OUT_low/MOVED_IN_low,
  #     mi_replacement = MOVED_OUT_middle/MOVED_IN_middle,
  #     hi_replacement = MOVED_OUT_high/MOVED_IN_high,
  #     li_mi_cratio = li_replacement/mi_replacement
  #     ) %>%
  filter(
    GEOID == "12057011504", CAT == "low", YEAR > 2000) %>%
  glimpse()

#
# Create final dataset
#

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

RR <- function(y, n){
  local = y/n
  regional = sum(y, na.rm = TRUE)/sum(n, na.rm = TRUE)
  RR = local/regional
  RR
}

df_tr <-
  tracts %>%
  # left_join()
  left_join(covid) %>% group_by(GEOID) %>% count() %>% arrange(desc(n)) %>% filter(n > 1)

left_join(evictions) %>%
  left_join(unemployment %>%
              mutate(GEOID = str_pad(fips, 11, "left", "0")) %>%
              filter(month == "08")) %>% # could consider looking at the first spike in unemployment.
  mutate(
    e_cut = factor(
      case_when(
        Evictions_RR > 2 ~ '2+',
        Evictions_RR > 1.1 & Evictions_RR <= 2 ~ '1.1 - 2',
        Evictions_RR > .9 & Evictions_RR <= 1.1 ~ '0.9 - 1.1',
        Evictions_RR > 0.5 & Evictions_RR <= .9 ~ '0.5 - 0.9',
        Evictions_RR >= 0 & Evictions_RR <= .5 ~ '0 - 0.5'),
      levels = c(
        '2+',
        '1.1 - 2',
        '0.9 - 1.1',
        '0.5 - 0.9',
        '0 - 0.5')),
    c_cut = factor(
      case_when(
        Overall_COVID_RR > 2  ~ '2+',
        Overall_COVID_RR > 1.1 & Overall_COVID_RR <= 2 ~ '1.1 - 2',
        Overall_COVID_RR > .9 & Overall_COVID_RR <= 1.1 ~ '0.9 - 1.1',
        Overall_COVID_RR > 0.5 & Overall_COVID_RR <= .9 ~ '0.5 - 0.9',
        Overall_COVID_RR >= 0 & Overall_COVID_RR <= .5 ~ '0 - 0.5'),
      levels = c(
        '2+',
        '1.1 - 2',
        '0.9 - 1.1',
        '0.5 - 0.9',
        '0 - 0.5')),
    u_cut = factor(
      case_when(
        unemp_rate > .2 ~ '+20%',
        unemp_rate > .14 & unemp_rate <= .2 ~ '14% - 20%',
        unemp_rate > .08 & unemp_rate <= .14 ~ '8% - 14%',
        unemp_rate > .05 & unemp_rate <= .08 ~ '5% - 8%',
        unemp_rate >= 0 & unemp_rate <= .05 ~ '0% - 5%'),
      levels = c(
        '+20%',
        '14% - 20%',
        '8% - 14%',
        '5% - 8%',
        '0% - 5%')),
    na.case = case_when(is.na(e_cut) & is.na(c_cut) & is.na(u_cut) ~ "Yes", TRUE ~ "No")) %>%
  filter(na.case == "No")


glimpse(df_tr)
# %>%
# filter(!is.na(e_cut) & !is.na(c_cut) & !is.na(u_cut))

# glimpse(df_tr %>% filter(is.na(u_cut)))

# df_tr %>% filter(sc_unemp_rate >= 2) %>% summary()

### Unemployment rate cutoffs
# These are based on .035 to .045 being good for economy, .05
# .05
# .08
# .14
# .20
####



# ggplot(df_tr %>% select(GEOID, unemp_rate) %>% drop_na(unemp_rate)) +
# geom_point(aes(x = reorder(GEOID, unemp_rate), y = unemp_rate))



### Waiting on new exit ###

# c_tr <- left_join(tracts, covid)

# #
# # Full dataset
# # --------------------------------------------------------------------------

# df <-
# 	tracts %>%
# 	# left_join(evictions) %>% glimpse()
# 	 %>%
# 		glimpse()
# 	# left_join(exit_mhi %>% rename(GEOID = exit_mhi_adj.csv)) %>%
# 	left_join(unemployment %>% mutate(GEOID = as.character(fips))) %>%
# 	left_join(covid)

# ==========================================================================
# Map
# ==========================================================================

#
# Color palletes
# --------------------------------------------------------------------------

# pal <-
# 	colorFactor(c(
# 		"#ca0020",
# 		"#f4a582",
# 		"#f7f7f7",
# 		"#92c5de",
# 		"#0571b0"
# 		),
# 		domain = d_tr$TOT_RR_METRO,
#         na.color = "transparent")

e_pal <-
  colorFactor(
    c("#ca0020",
      "#f4a582",
      "#f7f7f7",
      "#92c5de",
      "#0571b0"),
    domain = df_tr$e_cut,
    na.color = "transparent")

c_pal <-
  colorFactor(
    c("#ca0020",
      "#f4a582",
      "#f7f7f7",
      "#92c5de",
      "#0571b0"),
    domain = df_tr$c_cut,
    na.color = "transparent")

u_pal <-
  colorFactor(c(
    "#bd0026",
    "#f03b20",
    "#fd8d3c",
    "#fecc5c",
    "#ffffb2"),
    domain = df_tr$u_cut,
    na.color = "transparent")

#
# Map
# --------------------------------------------------------------------------

map <-
  leaflet(
    data = df_tr,
    options = leafletOptions(zoomControl = FALSE, title = "Affordable Neighborhoods")
  ) %>% # hide legend when using baseGroups
  addLayersControl(
    position = 'topright',
    baseGroups = c(
      # overlayGroups = c(
      "covid",
      "evictions",
      "unemployment"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addEasyButton(
    easyButton(
      position = "topright",
      icon="fa-crosshairs",
      title="My Location",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addMapPane(name = "polygons", zIndex = 410) %>%
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  ## evictions
  addPolygons(
    data = df_tr %>% filter(!is.na(e_cut)),
    group = "evictions",
    label = ~str_c("ID: ", GEOID, " eviction RR: ", Evictions_RR),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~e_pal(e_cut),
    stroke = TRUE,
    weight = 1,
    # opacity = .8,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    )) %>%
  addLegend(
    # position = 'bottomright',
    data = df_tr %>% filter(!is.na(e_cut)),
    pal = e_pal,
    values = ~e_cut,
    group = "evictions",
    title = "evictions") %>%
  ## covid
  addPolygons(
    data = df_tr %>% filter(!is.na(c_cut)),
    group = "covid",
    label = ~str_c("ID: ", GEOID, " covid RR: ", Overall_COVID_RR),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~c_pal(c_cut),
    stroke = TRUE,
    weight = 1,
    # opacity = .8,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    )) %>%
  addLegend(
    # position = 'bottomright',
    data = df_tr %>% filter(!is.na(c_cut)),
    pal = c_pal,
    values = ~c_cut,
    group = "covid",
    title = "covid") %>%
  ## unemployment
  addPolygons(
    data = df_tr %>% filter(!is.na(u_cut)),
    group = "unemployment",
    label = ~str_c("ID: ", GEOID, " unemployment rate: ", scales::percent(unemp_rate)),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .8,
    color = ~u_pal(u_cut),
    stroke = TRUE,
    weight = 1,
    # opacity = .8,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    )#,
    # popup = ~popup
  ) %>%
  addLegend(
    # position = 'bottomright',
    data = df_tr %>% filter(!is.na(u_cut)),
    pal = u_pal,
    values = ~u_cut,
    group = "covid",
    title = "covid")
map

htmlwidgets::saveWidget(seattle, file="/Users/timthomas/git/hprm/maps/phase1.html")
# map %>%
# pfun(data = d_tr %>%
# 		filter(CAT == "FPL"),
# 	group = "exit",
# 	val = "Evictions_RR")

#
# fwrite(tr_dem_acs, '~/git/exit-typologies/data/outputs/downloads/tr_dem_acs.csv.gz')

hud_geoids <- exit_hud %>% rename(GEOID = exit_hud_adj.csv)
mhi_geoids <- exit_mhi %>% rename(GEOID = exit_mhi_adj.csv)

notin_hud <- hud_geoids %>% filter(!GEOID %in% mhi_geoids$GEOID)
notin_mhi <- mhi_geoids %>% filter(!GEOID %in% hud_geoids$GEOID)

write_csv(notin_hud, "/Volumes/GoogleDrive/My Drive/CCI Docs/Current Projects/HPRM/Data/output/national/notin_hud.csv.bz2")
write_csv(notin_mhi, "/Volumes/GoogleDrive/My Drive/CCI Docs/Current Projects/HPRM/Data/output/national/notin_mhi.csv.bz2")

mhi_geoids %>%
  filter(!GEOID %in% hud_geoids$GEOID) %>%
  glimpse()

notin <- which(!hud_geoids %in% mhi_geoids)
glimpse(mhi_geoids)
