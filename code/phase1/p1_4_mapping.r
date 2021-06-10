# ==========================================================================
# Phase 1: Mapping current trajectories
# Author: timthomas@berkeley.edu
# Plan: 
# Core Layers
# 	[*] Eviction Notice Rate (pre-pandemic)
# 	[*] unemployment risk
# 	[*] COVID-19 Risk
# 	[*] Displacement typologies
# Additional Layers
# 	[df] Typologize Change ACS Unemployment v. DEEP Maps unemployment
# 	[df] Segregation
# 	[ ] Outline High BIPOC
# Popup
# 	[ ] Income (poverty, rent burden)
# 	[ ] housing market
# 	[ ] Demographics
# 	[ ] Other risks
# Fixes
#   [*] Change phase 1 away from risk, these are descriptives
#   [ ] Reconsider levels of risk, maybe just go with rates
# ==========================================================================

# ==========================================================================
# Libraries
# ==========================================================================
 
rm(list = ls()) # remove objects in environment
pacman::p_load(scales, sf, colorout, leaflet, leaflet.extras, htmlwidgets, tidyverse)
options(width = Sys.getenv('COLUMNS'), tigris_use_cache = TRUE, gargle_oob_default = TRUE)
# ==========================================================================
# Pull in data
# ==========================================================================
sf_df <- 
  read_rds("~/git/hprm/data/output/sf_df.rds") %>% 
  group_by(GEOID) %>% 
  mutate(
    ppov = sum(povfamh, povnonfamh, na.rm = TRUE)/totpov, 
    popup = 
    str_c('<b>Tract: ', GEOID, '</b><br><br>',
# hprm
      '<b><i><u>Housing Precarity Risk Score:</u></i></b> ', hprm_scale, ' out of 9<br>',
      'Displacement: ', typology, ' = ', hprm_dis, ' points<br>',
      'Eviction: ', percent(ev_rate, accuracy = .1), ' = ', hprm_ev, ' points<br>',
      'Unemployment: ', percent(unemp_rate, accuracy = 1), ' = ', hprm_unemp, ' points<br>',
      'Unemployment Change: ', percent(unemp_ch, accuracy = 1), ' = ', hprm_ch_unemp, ' points<br><br>', 
# evictions
      '<b><i><u>2019 Eviction Notice Rate</u></i></b><br>',
      case_when(
        is.na(ev_count) ~ "No data<br><br>", 
        TRUE ~ paste0(
          comma(ev_count, accuracy = 1), ' notices<br>', 
          percent(ev_rate, accuracy = .1), ' eviction notice rate<br><br>')
        ),       
# unemployment
      '<b><i><u>Unemployment</u></i></b><br>',
      '2020 average unemployment rate: ', percent(unemp_rate, accuracy = .1), '<br>', 
      '2019 average unemployment rate: ', percent(unemp_rate_2019, , accuracy = .1), '<br>', 
      case_when(
        unemp_ch >= 0 ~ paste0(percent(unemp_ch, , accuracy = .1), ' increase'), 
        unemp_ch < 0 ~ paste0(percent(unemp_ch, , accuracy = .1), ' decrease')), '<br>', 

      '2020 labor force: ', comma(laborforce, accuracy = 1), '<br>', 
      '2020 unemployed: ', comma(unemployed, accuracy = 1), '<br><br>',
# covid
      '<b><i><u>COVID-19 (March 2020 to March 2021)</u></i></b><br>',
      'Case rate: ', 
        case_when(is.na(c_rate) ~ 'No Data', TRUE ~ paste0(percent(c_rate, accuracy = .1))), '<br>', 
      case_when(is.na(rate) ~ '<br>', TRUE ~ paste0(comma(rate, accuracy = 1), ' per 10,000 people')), 
      case_when(is.na(deaths) ~ '<br>', TRUE ~ paste0(comma(deaths, accuracy = 1),' Deaths<br>')), '<br>',      
# displacement
      '<b><i><u>Households by income (2019)</u></i></b><br>',
      comma(tr_totinc_count_2019, accuracy = 1), ' households (', 
      percent(Rent.x/Total, accuracy = 1), ' renters)<br>', 
      case_when(
        is.na(MedRent) ~ '', TRUE ~ paste0(dollar(MedRent, accuracy = 1), ' median rent<br><br>')),
      'High income: ', comma(tr_HI_count_2019, accuracy = 1), ' (', percent(tr_HI_prop_2019, accuracy = 1), ')<br>',
      'Middle income: ', comma(tr_MI_count_2019, accuracy = 1), ' (', percent(tr_MI_prop_2019, accuracy = 1), ')<br>',                    
      'Low income: ', comma(tr_LI_count_2019, accuracy = 1), ' (', percent(tr_LI_prop_2019, accuracy = 1), ')<br>', 
      case_when(
        tr_LI_pchange < 0 ~ paste0(percent(tr_LI_pchange, accuracy = 1), ' decrease'), 
        tr_LI_pchange >= 0 ~ paste0(percent(tr_LI_pchange, accuracy = 1), ' increase')), '  in low-income HH since 2000<br><br>', 
        
# demographics
      '<b><i><u>Demographics (2019)</u></i></b><br>',
      NeighType, ' neighborhood<br>', 
      percent(pAsian, accuracy = 1), ' Asian', '<br>', 
      percent(pBlack, accuracy = 1), ' Black', '<br>', 
      percent(pLatinx, accuracy = 1), ' Latinx', '<br>', 
      percent(pOther, accuracy = 1), ' Other', '<br>', 
      percent(pWhite, accuracy = 1), ' White', '<br><br>',
# income
      '<b><i><u>Income (2019)</u></i></b><br>',
      'County area median income: ', dollar(co_ami_2019, accuracy = 1), '<br>', 
      'Tract median household income: ', 
        case_when(
          is.na(MHHInc) ~ 'No Data<br>', 
          TRUE ~ paste0(dollar(MHHInc, accuracy = 1), '<br>')
          ), 
      'Percent on welfare: ', percent(welf/totwelf, accuracy = .1), '<br>', 
      'Percent in poverty: ', percent(ppov, accuracy = 1), '<br><br>'      
      )
    )

#
# Palletes 
# --------------------------------------------------------------------------
source("~/git/hprm/code/phase1/p1_1_functions.r")
sf_e_pal <- e_pal(sf_df$rr_cut)
sf_er_pal <- er_cut_pal(sf_df$er_cut)
# sf_d_vli_pal <- d_pal(sf_df$di_vlinet_cut)
# sf_d_li_pal <- d_pal(sf_df$di_linet_cut)
# sf_d_livli_pal <- d_pal(sf_df$di_livlinet_cut)
# sf_d_mid_pal <- d_pal(sf_df$di_midnet_cut)
# sf_d_high_pal <- d_pal(sf_df$di_high_cut)
sf_c_pal <- c_pal(sf_df$c_cut)
sf_u_pal <- u_pal(sf_df$u_cut)
sf_ui_pal <- u_increase_pal(sf_df$unemp_cat)
sf_dt_pal <- dt_pal(sf_df$typology)
sf_nt_pal <- nt_pal(sf_df$nt_conc)
sf_hprm_pal <- hprm_cut_pal(sf_df$hprm_cut)

#
# Map
# --------------------------------------------------------------------------

map <- 
  leaflet(options = leafletOptions(title = "HPRM", zoomControl = TRUE)) %>%
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
      # 'Net Migration VLI',
      # 'Net Migration LI',
      'Housing Precarity Risk',
      '2019 Displacement',
      '2019 Eviction Notice Rate',
      '2020 Avg Unemployment Rate',
      'Unemployment Change 2019 - 2020',
      'COVID-19 Case Rate', 
      'Segregation'),
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
  
sf_map <- 
  map %>% 
# HPRM scale
  addPolygons(
    data = sf_df,
    group = "Housing Precarity Risk",
    label = ~paste0(hprm_cut, ": ", hprm_scale, " out of 9 points"),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~sf_hprm_pal(sf_df$hprm_cut),
    stroke = TRUE,
    weight = 1,
    opacity = .3,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    ),
  popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_hprm_pal,
    values = ~hprm_cut,
    group = "Housing Precarity Risk",
    title = "Housing Precarity Risk"
  ) %>%
# Displacement
  addPolygons(
    data = sf_df,
    group = "2019 Displacement",
    label = ~typology,
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~sf_dt_pal(typology),
    stroke = TRUE,
    weight = 1,
    opacity = .3,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    ),
  popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_dt_pal,
    values = ~typology,
    group = "2019 Displacement",
    title = "2019 Displacement"
  ) %>%
# Eviction Notice Rate
  addPolygons(
    data = sf_df,
    group = '2019 Eviction Notice Rate',
    label = ~case_when(is.na(ev_count) ~ "No Data", 
      TRUE ~ paste0(scales::percent(ev_rate, accuracy = .1), ' ', ev_rate_cut, ' eviction notice rate')),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~sf_er_pal(er_cut),
    stroke = TRUE,
    weight = 1,
    opacity = .3,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    ),
    popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_er_pal,
    values = ~er_cut,
    group = '2019 Eviction Notice Rate',
    title = '2019 Eviction Notice Rate'
  ) %>%
# 2020 Avg Unemployment rate
  addPolygons(
    data = sf_df,
    group = "2020 Avg Unemployment Rate",
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
    popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_u_pal,
    values = ~u_cut,
    group = "2020 Avg Unemployment Rate",
    title = "2020 Avg Unemployment Rate"
  ) %>%
# Unemp Change
  addPolygons(
    data = sf_df,
    group = 'Unemployment Change 2019 - 2020',
    label = ~paste0(
      unemp_cat, 
      " Change: ", 
      case_when(
        unemp_ch >= 0 ~ paste0(
          scales::percent(unemp_ch, accuracy = 2), 
          " increase (", 
          scales::percent(unemp_rate_2019, accuracy = 2), 
          " to ", 
          scales::percent(unemp_rate, accuracy = 2), 
          ")"
          ), 
        unemp_ch < 0 ~ paste0(
          scales::percent(unemp_ch, accuracy = 2), 
          " decrease (", 
          scales::percent(unemp_rate_2019, accuracy = 2), 
          " to ", 
          scales::percent(unemp_rate, accuracy = 2), 
          ")"
        ))),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~sf_ui_pal(unemp_cat),
    stroke = TRUE,
    weight = 1,
    opacity = .3,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    ),
    popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_ui_pal,
    values = ~unemp_cat,
    group = 'Unemployment Change 2019 - 2020',
    title = 'Unemployment Change 2019 - 2020'
  ) %>%
# Covid
  addPolygons(
    data = sf_df,
    group = "COVID-19 Case Rate",
    label = ~paste0("Covid Rate: ", scales::percent(c_rate, accuracy = .1)),
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
    popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_c_pal,
    values = ~c_cut,
    group = "COVID-19 Case Rate",
    title = "COVID-19 Case Rate"
  ) %>% 
# Segregation
  addPolygons(
    data = sf_df,
    group = "Segregation",
    label = ~nt_conc,
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~sf_nt_pal(nt_conc),
    stroke = TRUE,
    weight = 1,
    opacity = .3,
    highlightOptions = highlightOptions(
      color = "#ff4a4a",
      weight = 5,
      bringToFront = TRUE
    ),
    popup = ~popup,
    popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
  ) %>%
  addLegend(
    data = sf_df,
    position = 'topright',
    pal = sf_nt_pal,
    values = ~nt_conc,
    group = "Segregation",
    title = "Segregation"
  ) %>% 
  setView(lng = -122.45, lat = 37.78, zoom = 12)

sf_map

saveWidget(sf_map, file="~/git/hprm/maps/p1sf.html")