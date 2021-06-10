# ==========================================================================
# Libraries
# ==========================================================================

rm(list = ls()) # remove objects in environment
pacman::p_load(leaflet, leaflet.extras, htmlwidgets)

# ==========================================================================
# Pull in data
# ==========================================================================

ps_df <- readRDS("~/data/output/ps_df.rds")
source("~/git/hprm/code/phase1/p1_2_functions.r")
ps_e_pal <- e_pal(ps_df$rr_cut)
ps_d_vli_pal <- d_pal(ps_df$di_vlinet_cut)
ps_d_li_pal <- d_pal(ps_df$di_linet_cut)
ps_d_livli_pal <- d_pal(ps_df$di_livlinet_cut)
ps_d_mid_pal <- d_pal(ps_df$di_midnet_cut)
ps_d_high_pal <- d_pal(ps_df$di_high_cut)
ps_c_pal <- c_pal(ps_df$c_cut)
ps_u_pal <- u_pal(ps_df$u_cut)


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
      # 'Net Migration VLI',
      # 'Net Migration LI',
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

  # Eviction risk
ps_map <- map %>% addPolygons(
    data = ps_df,
    group = "Eviction Risk",
    label = ~paste0("E Risk: ", round(ev_rr, 2), " | E Rate: ", scales::percent(ev_rate, accuracy = .1)),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~ps_e_pal(rr_cut),
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
    data = ps_df,
    position = 'topright',
    pal = ps_e_pal,
    values = ~rr_cut,
    group = "Eviction Risk",
    title = "Eviction Risk"
  ) %>%
  # Avg Unemp rate
  addPolygons(
    data = ps_df,
    group = "Avg Unemp Rate",
    label = ~paste0("Unemp Rate: ", scales::percent(unemp_rate, accuracy = .1)),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~ps_u_pal(u_cut),
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
    data = ps_df,
    position = 'topright',
    pal = ps_u_pal,
    values = ~u_cut,
    group = "Avg Unemp Rate",
    title = "Avg Unemp Rate"
  ) %>%
  # Covid
  addPolygons(
    data = ps_df,
    group = "COVID Risk",
    label = ~paste0("Covid Rate: ", scales::percent(c_rate, accuracy = .1)),
    labelOptions = labelOptions(textsize = "12px"),
    fillOpacity = .5,
    color = ~ps_c_pal(c_cut),
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
    data = ps_df,
    position = 'topright',
    pal = ps_c_pal,
    values = ~c_cut,
    group = "COVID Risk",
    title = "COVID Risk"
  )
saveWidget(ps_map, file="~/git/hprm/maps/p1v1ps.html")
# }
#
# ps_map <- mapping(ps_df, ps_covid)
# ps_map <- mapping(ps_data, il_covid)

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
#     data = ps_df,
#     group = "Net Migration VLI",
#     label = ~paste0("Net Migration: ", round(net_very_low_adj, 2)),
#     color = ~d_vli_pal(di_vlinet_cut),
#     pal = d_vli_pal,
#     cut = di_vlinet_cut
#     ) ## maybe delete

map %>%
  # VLI Disp
  # addPolygons(
  #   data = ps_df,
  #   group = "Net Migration VLI",
  #   label = ~paste0("Net Migration: ", round(net_very_low_adj, 2)),
  #   labelOptions = labelOptions(textsize = "12px"),
  #   fillOpacity = .5,
  #   color = ~ps_d_vli_pal(di_vlinet_cut),
  #   stroke = TRUE,
  #   weight = 1,
  #   opacity = .3,
#   highlightOptions = highlightOptions(
#     color = "#ff4a4a",
#     weight = 5,
#     bringToFront = TRUE
#   ),
#   # popup = ~popup,
#   popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
# ) %>%
# addLegend(
#   data = ps_df,
#   position = 'topright',
#   pal = ps_d_vli_pal,
#   values = ~di_vlinet_cut,
#   group = "Net Migration VLI",
#   title = "Net Migration VLI"
# ) %>%
# # LI Disp
# addPolygons(
#   data = ps_df,
#   group = "Net Migration LI",
#   label = ~paste0("Net Migration: ", round(net_low_adj, 2)),
#   labelOptions = labelOptions(textsize = "12px"),
#   fillOpacity = .5,
#   color = ~ps_d_vli_pal(di_linet_cut),
#   stroke = TRUE,
#   weight = 1,
#   opacity = .3,
#   highlightOptions = highlightOptions(
#     color = "#ff4a4a",
#     weight = 5,
#     bringToFront = TRUE
#   ),
#   # popup = ~popup,
#   popupOptions = popupOptions(maxHeight = 215, closeOnClick = TRUE)
# ) %>%
# addLegend(
#   data = ps_df,
#   position = 'topright',
#   pal = ps_d_li_pal,
#   values = ~di_linet_cut,
#   group = "Net Migration LI",
#   title = "Net Migration LI"
# ) %>%