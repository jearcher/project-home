# ==========================================================================
# Phase 1 Functions
# ==========================================================================

RR <- function(evicted, renters){
  o = evicted
  r = sum(evicted, na.rm = TRUE)/sum(renters, na.rm = TRUE)
  e = renters*r
  rr = o/e
  rr
}

er_cut <- function(x){
  cut(x,
    breaks = c(0, .01, .02, .04, 0.08, 1),
    labels = c("< 1%", "1% to 2%", "2% to 4%", "4% to 8%", "> 8%"),
    ordered_result = TRUE)
}

rr_cut <- function(x){
  cut(x,
    breaks = c(0, .25, .5,1, 2, 3, 30),
    # labels = c("< .25", ".25 to .5", ".5 to 1", "1 to 2", "2 to 3", "> 3"),
    labels = c("Extremely Low", "Low", "Below Average", "Above Average", "High", "Extremely High"),
    ordered_result = TRUE)
    }

count_cut <- function(x){
  cut(x,
    breaks = c(0, 10, 20, 40, 300),
    labels = c("< 10", "10 to 20", "20 to 40", "> 40"),
    ordered_result = TRUE)
    }

unemp_cut <- function(x){
  factor(cut(x,
      breaks = c(0, .05, .08, .14, .2, 1),
      labels = c("< 5%", "5% to 8%", "8% to 14%", "14% to 20%", "> 20%"),
      ordered_result = TRUE), 
  levels = c("< 5%", "5% to 8%", "8% to 14%", "14% to 20%", "> 20%"))
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


disp <- function(data, years = c(2015, 2016, 2017, 2018, 2019)){
  data %>%
  mutate(YEARS = YEAR,
         YEAR = as.numeric(str_sub(YEARS, -4, -1)),
  ) %>%
  filter(YEAR %in% years) %>%
  group_by(GEOID, CAT) %>%
  summarize(
    HH = sum(HH, na.rm = TRUE),
    MOVE_OUT = sum(MOVE_OUT, na.rm = TRUE),
    MOVE_IN = sum(MOVE_IN, na.rm = TRUE),
    MOVE_WITHIN = sum(MOVE_WITHIN, na.rm = TRUE),
    YEARS = paste0(min(years), "-", max(years))
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
    net_livli = ((MOVE_IN_livli - MOVE_OUT_livli)/HH_livli)*1000,
    net_high_adj = case_when(p_hi < 0.05 ~ NA_real_, TRUE ~ net_high),
    net_middle_adj = case_when(p_mi < 0.05 ~ NA_real_, TRUE ~ net_middle),
    net_low_adj = case_when(p_li < 0.05 ~ NA_real_, TRUE ~ net_low),
    net_very_low_adj = case_when(p_vli < 0.05 ~ NA_real_, TRUE ~ net_very_low),
    net_livli_adj = case_when(p_livli < 0.05 ~ NA_real_, TRUE ~ net_livli),
    GEOID = as.character(GEOID)
    )
}

# Mapping functions

#
# palettes
# --------------------------------------------------------------------------

dt_pal <- function(data){
  colorFactor(
    # c('#87CEFA', # old color
    #   '#6495ED'), 
    c('#e0f3f8',
      '#fee090',
      # '#fc8d59',
      '#d73027'#, 
      # "#8f8f8f" # high student population
      ), 
      na.color = 'transparent',
      domain = data)
  }
nt_pal <- function(data){
    colorFactor(c(
        '#33a02c', # 'Mostly Asian', green
        '#1f78b4', # 'Mostly Black', blue
        '#e31a1c', # 'Mostly Latinx', red
        '#9b66b0', # 'Mostly Other', purple
        '#C95123', # 'Mostly White',
        '#1fc2ba', # 'Asian-Black',
        '#d6ae5c', # 'Asian-Latinx',
        '#91c7b9', # 'Asian-Other',
        '#b2df8a', # 'Asian-White',
        '#de4e4b', # 'Black-Latinx',
        '#71a1f5', # 'Black-Other',
        '#a6cee3', # 'Black-White',
        '#f0739b', # 'Latinx-Other',
        '#fb9a99', # 'Latinx-White',
        '#c28a86', # 'Other-White',
        '#fdbf6f', # '3 Group Mixed',
        '#cab2d6', # '4 Group Mixed',
        '#1d5fd1', # 'Diverse',
        '#FFFFFF'),  # 'Unpopulated Tract'
      domain = data,
      na.color = 'transparent'
        )
  }

er_cut_pal <- function(data){
  colorFactor(
    c(
    '#e0f3f8',
    '#91bfdb',
    '#fee090',
    '#fc8d59',
    '#d73027'
    ),
    domain = data,
    na.color = "transparent")
}

hprm_cut_pal <- function(data){
  colorFactor(
    c(
    '#e0f3f8',
    '#fee090',
    # '#fc8d59',
    '#d73027'#, 
    # "#8f8f8f" # high student population
    ),
    domain = data,
    na.color = "transparent")
}

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

# chi_e_pal <- e_pal(chi_data$ev_cut)
#
# wa_e_pal <- e_pal(wa_data$ev_cut)

d_pal <- function(data){
  colorFactor(
    c("#b2182b",
      "#ef8a62",
      "#fddbc7",
      "#f7f7f7",
      "#d1e5f0",
      "#67a9cf",
      "#2166ac"#, 
      # "#8f8f8f" # high student population
    ),
    domain = data,
    na.color = "transparent")
}

# chi_d_vli_pal <- d_pal(chi_data$di_vlinet_cut)
# chi_d_li_pal <- d_pal(chi_data$di_linet_cut)
# chi_d_livli_pal <- d_pal(chi_data$di_livlinet_cut)
# chi_d_mid_pal <- d_pal(chi_data$di_midnet_cut)
# chi_d_high_pal <- d_pal(chi_data$di_high_cut)
#
#
# wa_d_vli_pal <- d_pal(wa_data$di_vlinet_cut)
# wa_d_li_pal <- d_pal(wa_data$di_linet_cut)
# wa_d_livli_pal <- d_pal(wa_data$di_livlinet_cut)
# wa_d_mid_pal <- d_pal(wa_data$di_midnet_cut)
# wa_d_high_pal <- d_pal(wa_data$di_high_cut)

c_pal <- function(data){
  colorFactor(
    c("#0571b0",
      "#92c5de",
      #"#f7f7f7",
      "#f4a582",
      "#ca0020"),
    domain = data,
    na.color = "transparent")
}

# chi_c_pal <- c_pal(chi_data$c_cut)
# wa_c_pal <- c_pal(wa_king_covid$c_cut)

u_pal <- function(data){
  colorFactor(c(
    "#ffffb2",
    "#fecc5c",
    "#fd8d3c",
    "#f03b20",
    "#bd0026"#, 
    # "#8f8f8f" # high student population
  ),
  domain = data,
  na.color = "transparent")
}

u_increase_pal <- function(data){
  colorFactor(c( 
    "#99d594", 
    "#ffffbf",
    "#fc8d59"
  ),
  domain = data,
  na.color = "transparent")
}

# chi_u_pal <- u_pal(chi_data$u_cut)
# wa_u_pal <- u_pal(wa_data$u_cut)

#
# Decennial Census
# --------------------------------------------------------------------------

dec_inc_vars <- c(
'HHInc_tot' = 'P052001',
'HHInc_10' = 'P052002', # Total:  Less than $10,000 
'HHInc_15' = 'P052003', # Total:  $10,000 to $14,999 
'HHInc_20' = 'P052004', # Total:  $15,000 to $19,999 
'HHInc_25' = 'P052005', # Total:  $20,000 to $24,999 
'HHInc_30' = 'P052006', # Total:  $25,000 to $29,999 
'HHInc_35' = 'P052007', # Total:  $30,000 to $34,999 
'HHInc_40' = 'P052008', # Total:  $35,000 to $39,999 
'HHInc_45' = 'P052009', # Total:  $40,000 to $44,999 
'HHInc_50' = 'P052010', # Total:  $45,000 to $49,999 
'HHInc_60' = 'P052011', # Total:  $50,000 to $59,999 
'HHInc_75' = 'P052012', # Total:  $60,000 to $74,999 
'HHInc_100' = 'P052013', # Total:  $75,000 to $99,999 
'HHInc_125' = 'P052014', # Total:  $100,000 to $124,999 
'HHInc_150' = 'P052015', # Total:  $125,000 to $149,999 
'HHInc_200' = 'P052016', # Total:  $150,000 to $199,999 
'HHInc_250' = 'P052017', # Total:  $200,000 or more 
'MHHInc' = 'P053001')

inc_names <- c(
'HHInc_10' = 9999, # Total:  Less than $10,000
'HHInc_15' = 14999, # Total:  $10,000 to $14,999
'HHInc_20' = 19999, # Total:  $15,000 to $19,999
'HHInc_25' = 24999, # Total:  $20,000 to $24,999
'HHInc_30' = 29999, # Total:  $25,000 to $29,999
'HHInc_35' = 34999, # Total:  $30,000 to $34,999
'HHInc_40' = 39999, # Total:  $35,000 to $39,999
'HHInc_45' = 44999, # Total:  $40,000 to $44,999
'HHInc_50' = 49999, # Total:  $45,000 to $49,999
'HHInc_60' = 59999, # Total:  $50,000 to $59,999
'HHInc_75' = 74999, # Total:  $60,000 to $74,999
'HHInc_100' = 99999, # Total:  $75,000 to $99,999
'HHInc_125' = 124999, # Total:  $100,000 to $124,999
'HHInc_150' = 149999, # Total:  $125,000 to $149,999
'HHInc_200' = 199999, # Total:  $150,000 to $199,999
'HHInc_250' = 249999 # Total:  $200,000 or more
)

#
# Download decenial data
# --------------------------------------------------------------------------
dl_dec <- function(state)
    get_decennial(
      geography = "tract",
      variables = dec_inc_vars,
      state = state,
      county = NULL,
      geometry = FALSE,
      cache_table = TRUE,
      output = "wide",
      year = 2000,
      sumfile = "sf3",
      keep_geo_vars = TRUE
    ) %>% 
  mutate(year = 2000) 

### test 
# ca_dec <- dl_dec("CA")

# tmap_mode("view")
# tm_shape(check) + 
# tm_fill("tr_totinc_count_2000")

  # tm_fill(
  #   "HHInc_tot",
  #   # n = 6,
  #   style = "fixed", 
  #   # id = c("pwht_dif", "pblk_dif", "pasi_dif", "plat_dif"),
  #   # breaks = c(-1, -.5, -.25, -.05, .05, .25, .5, 1),
  #     palette = "-RdBu", 
  #     alpha = .5, 
  #     legend.reverse = TRUE) +
  #   tm_borders(alpha = 0, lwd = 0) + 
  #   tm_view(
  #     view.legend.position = c("left", "bottom")
  #     ) + 
  #   tm_facets(sync = TRUE, ncol = 2)

#
# ACS census
# --------------------------------------------------------------------------

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
  'totunemp' = 'B23025_001',
  'unemp' = 'B23025_005'
  )

# years_acs <- rep(2011:2019)
years_acs <- 2019

#
# Download ACS data
# --------------------------------------------------------------------------
dl_acs <- function(state, county = NULL){
  map_df(years_acs, function(year){
    get_acs(
    state = state,
    county = county,
    geography = "tract",
    year = year,
    variables = acs_vars,
    geometry = FALSE,
    cache_table = TRUE,
    output = "wide"
      ) %>%
    mutate(year = year) %>% 
    select(!ends_with("M")) %>% 
    rename_at(vars(ends_with("E")), ~ str_remove(., "E$"))
  }
  )}

#
# Define LI, MI, and HI households
# --------------------------------------------------------------------------

df_li <- function(df){
  df %>% 
  mutate(COUNTY = substr(GEOID, 1, 5)) %>% 
  select(
    GEOID,
    COUNTY,
    year,
    MHHInc,
    HHInc_10:HHInc_250) %>% 
  group_by(COUNTY, year) %>%
  mutate(
    co_ami = median(MHHInc, na.rm = TRUE),
    co_MI_val = 1.2*co_ami,
    co_LI_val = .8*co_ami) %>% 
  ungroup() %>%
  gather(medinc_cat, medinc_count, HHInc_10:HHInc_250) %>% 
  mutate(medinc_cat = recode(medinc_cat, !!!inc_names)) %>% 
  mutate(bottom_inccat = # for imputation - these are the incriments of change
         case_when(medinc_cat == 9999 ~ medinc_cat - 9999,
                   medinc_cat > 9999 & medinc_cat <= 49999 ~ medinc_cat - 4999,
                   medinc_cat == 59999 ~ medinc_cat - 9999,
                   medinc_cat > 59999 & medinc_cat <= 149999 ~ medinc_cat - 24999,
                   medinc_cat >= 199999 ~ medinc_cat - 49999,
                   TRUE ~ NA_real_),
    top_inccat = medinc_cat,
    HI = case_when( # dummy
      bottom_inccat >= co_MI_val ~ 1, 
      top_inccat >= co_MI_val & bottom_inccat <= co_MI_val ~ 
        (top_inccat - co_MI_val)/(top_inccat - bottom_inccat),
      TRUE ~ 0
      ),
    MI = case_when( 
      top_inccat >= co_MI_val & bottom_inccat <= co_MI_val & bottom_inccat >= co_LI_val ~ 
        (co_MI_val - bottom_inccat)/(top_inccat - bottom_inccat),  # top part
      top_inccat >= co_LI_val & top_inccat <= co_MI_val & bottom_inccat >= co_LI_val & bottom_inccat <= co_MI_val ~ 1, # fits in the middle
      top_inccat <= co_MI_val & top_inccat >= co_LI_val & bottom_inccat <= co_LI_val ~ # bottom part
        (top_inccat - co_LI_val)/(top_inccat - bottom_inccat), 
      TRUE ~ 0
      ), 
    LI = case_when(
      top_inccat >= co_LI_val & bottom_inccat <= co_LI_val ~ 
        (co_LI_val - bottom_inccat)/(top_inccat - bottom_inccat), 
      top_inccat <= co_LI_val ~ 1, 
      TRUE ~ 0
      )) %>% 
  group_by(GEOID, year) %>%
  mutate(
       tr_totinc_count = sum(medinc_count, na.rm = TRUE),
       tr_HI_count = sum(HI*medinc_count, na.rm = TRUE),
       tr_MI_count = sum(MI*medinc_count, na.rm = TRUE),
       tr_LI_count = sum(LI*medinc_count, na.rm = TRUE)) %>% #,
       # tr_HI_prop = tr_HI_count/tr_totinc_count,
       # tr_MI_prop = tr_MI_count/tr_totinc_count,
       # tr_LI_prop = tr_LI_count/tr_totinc_count) %>% 
  select(GEOID:co_LI_val, tr_totinc_count:tr_LI_count, year) %>%
  distinct()
}

# df_li(ca_acs) %>% glimpse()

#
# Create income categories
# --------------------------------------------------------------------------

dec_li <- function(df2000)
  left_join(
    df_li(df2000), 
    read_csv("~/data/census/crosswalk_2000_2010.csv"),
    by = c("GEOID" = "trtid00")
    ) %>% 
  mutate_at(vars(starts_with("tr_")), ~.*weight) %>% 
  rename_at(vars(MHHInc:tr_LI_count), ~ paste0(., "_", 2000)) %>% 
  select(GEOID = trtid10, year, tr_totinc_count_2000:tr_LI_count_2000) %>% 
  group_by(GEOID, year) %>% 
  summarize_at(vars(tr_totinc_count_2000:tr_LI_count_2000), ~sum(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-year)

# dec_li(ca_2000) %>% glimpse()
#
# Merge dec and acs and calc change in LI groups
# --------------------------------------------------------------------------
area_li_19 <- function(acs_df, dec_df, yr = 2019){
  left_join(
    df_li(
      acs_df %>% 
      filter(year == yr) %>% 
      select(GEOID, year, MHHInc, HHInc_Total:HHInc_250)) %>%
    rename_at(vars(MHHInc:tr_LI_count), ~ paste0(., "_", 2019)), 
    dec_df) %>% 
  select(GEOID, MHHInc_2019:tr_LI_count_2000) %>% 
  ungroup() %>%
  mutate(
    tr_HI_prop_2019 = tr_HI_count_2019/tr_totinc_count_2019,
    tr_MI_prop_2019 = tr_MI_count_2019/tr_totinc_count_2019,
    tr_LI_prop_2019 = tr_LI_count_2019/tr_totinc_count_2019,
    tr_HI_prop_2000 = tr_HI_count_2000/tr_totinc_count_2000,
    tr_MI_prop_2000 = tr_MI_count_2000/tr_totinc_count_2000,
    tr_LI_prop_2000 = tr_LI_count_2000/tr_totinc_count_2000,
    tr_HI_change = tr_HI_count_2019 - tr_HI_count_2000,
    tr_MI_change = tr_MI_count_2019 - tr_MI_count_2000,
    tr_LI_change = tr_LI_count_2019 - tr_LI_count_2000, 
    tr_HI_pchange = (tr_HI_count_2019 - tr_HI_count_2000)/tr_HI_count_2000,
    tr_MI_pchange = (tr_MI_count_2019 - tr_MI_count_2000)/tr_MI_count_2000,
    tr_LI_pchange = (tr_LI_count_2019 - tr_LI_count_2000)/tr_LI_count_2000) %>% 
  left_join(ca_acs %>% select(GEOID, year, pop = totrace)) %>% 
  mutate( 
    low_pdmt_medhhinc = 
      case_when(
        tr_LI_prop_2019 >= 0.55 & 
        tr_MI_prop_2019 < 0.45 & 
        tr_HI_prop_2019 < 0.45 ~ 1, 
        TRUE ~ 0), 
    high_pdmt_medhhinc = 
      case_when(
        tr_LI_prop_2019 < 0.45 &
        tr_MI_prop_2019 < 0.45 & 
        tr_HI_prop_2019 >= 0.55 ~ 1,
        TRUE ~ 0), 
    mod_pdmt_medhhinc = 
      case_when(
        tr_LI_prop_2019 < 0.45 & 
        tr_MI_prop_2019 >= 0.55 & 
        tr_HI_prop_2019 < 0.45 ~ 1, 
        TRUE ~ 0)) %>% 
  # group_by(GEOID) %>%  
  # mutate(p_students = sum(proenroll, colenroll, na.rm = TRUE)/totenroll
  #   ) %>%
  # ungroup() %>%  
  mutate(
    mix_low_medhhinc = 
      case_when(
        low_pdmt_medhhinc == 0 &
        mod_pdmt_medhhinc == 0 &
        high_pdmt_medhhinc == 0 & 
        MHHInc_2019 < co_LI_val_2019 ~ 1, 
        TRUE ~ 0), 
    # , 
    # mix_mod_medhhinc_18 = 
    #   case_when(
    #     low_pdmt_medhhinc == 0 & 
    #     mod_pdmt_medhhinc == 0 & 
    #     high_pdmt_medhhinc == 0 & 
    #     MHHInc_2019
    #     ), 
    # mix_high_medhhinc_18 = 
    lostli = 
      case_when(
        tr_LI_change < 0 ~ 1, 
        TRUE ~ 0), 
    typology = # displacement typology for li and susceptible to displacement
      factor(case_when(
        pop >= 500 &
        (low_pdmt_medhhinc == 1 | mix_low_medhhinc == 1) & 
        lostli == 1 ~ 'Ongoing Displacement', 
        pop >= 500 &
        (low_pdmt_medhhinc == 1 | mix_low_medhhinc == 1) & 
        lostli == 0 ~ 'Low-income & Susceptible to Displacement',
        TRUE ~ 'Low Displacement Risk'), 
      levels = c('Low Displacement Risk', 'Low-income & Susceptible to Displacement', 'Ongoing Displacement')))
}

#
# Erase water
# --------------------------------------------------------------------------

# counties <- function(state, df)
#   counties(state) %>% 
#   st_transform(st_crs(df)) %>% 
#   .[df, ]  %>% 
#   arrange(STATEFP, COUNTYFP) %>% 

# st_geometry(counties) <- NULL

# state_water <- counties %>% pull(STATEFP)
# county_water <- counties %>% pull(COUNTYFP)

# water <- 
# map2_dfr(state_water, county_water, 
#   function(states = state_water, counties = county_water){
#     area_water(
#       state = states,
#       county = counties, 
#       class = 'sf') %>% 
#     filter(AWATER > 500000)
#     }) %>% 
# st_transform(st_crs(bay5))

# #
# # Remove water & non-urban areas & simplify spatial features
# # --------------------------------------------------------------------------

# st_erase <- function(x, y) {
#   st_difference(x, st_union(y))
# }

# ###
# # Note: This takes a very long time to run. 
# ###
# bay5 <- 
#   bay5 %>% 
#     st_erase(water)

###
# TESTBED - at risk of gentrification - below is what's needed 

# aff <- (rm_hinc_18*.3)/12

# ## Mixed-Low income
# pums['mixed_low'] = np.where((pums['predominantly_LI']==0)&
#                               (pums['predominantly_MI']==0)&
#                               (pums['predominantly_HI']==0)&
#                               (pums['mmhcosts_18']<aff_18*0.6),1,0)


# ### ***** 1990 *****
# ### 3/4 Criteria that needs to be met
# data['vul_gent_90'] = np.where(((data['aboverm_real_mrent_90']==0)|(data['aboverm_real_mhval_90']==0))&
#                                  ((data['aboverm_per_all_li_90']+
#                                    data['aboverm_per_nonwhite_90']+
#                                    data['aboverm_per_rent_90']+
#                                    (1-data['aboverm_per_col_90']))>2), 1, 0)

# # ### ***** 2000 *****
# # ### 3/4 Criteria that needs to be met
# data['vul_gent_00'] = np.where(((data['aboverm_real_mrent_00']==0)|(data['aboverm_real_mhval_00']==0))&
#                                  ((data['aboverm_per_all_li_00']+
#                                    data['aboverm_per_nonwhite_00']+
#                                    data['aboverm_per_rent_00']+
#                                    (1-data['aboverm_per_col_00']))>2), 1, 0)

# data['gent_90_00'] = np.where((data['vul_gent_90']==1)&
#                                 (data['aboverm_ch_per_col_90_00']==1)&
#                                 (data['aboverm_pctch_real_hinc_90_00']==1)&
#                                 (data['lostli_00']==1)&
#                                 (data['hotmarket_00']==1), 1, 0)

# census.loc[(census['tot_decrease']==1)|(census['tot_marginal']==1), 'change_flag_encoded'] = 1

# ## Change over 90th percentile change
# zillow['ab_90percentile_ch'] = np.where(zillow['per_ch_zillow_12_18']>percentile_90, 1, 0)

# census_zillow['rent_90percentile_ch'] = np.where(census_zillow['pctch_real_mrent_12_18']>=0.9, 1, 0)

# ### ***** 2018 *****
# ### 3/4 Criteria that needs to be met
# data['vul_gent_18'] = np.where(((data['aboverm_real_mrent_18']==0)|(data['aboverm_real_mhval_18']==0))&
#                                  ((data['aboverm_per_all_li_18']+
#                                    data['aboverm_per_nonwhite_18']+
#                                    data['aboverm_per_rent_18']+
#                                    (1-data['aboverm_per_col_18']))>2), 1, 0)

# # # 2000 - 2018
# data['gent_00_18'] = np.where((data['vul_gent_00']==1)&
#                                 (data['aboverm_ch_per_col_00_18']==1)&
#                                 (data['aboverm_pctch_real_hinc_00_18']==1)&
#                                 (data['lostli_18']==1)&
#                                 # (data['ch_per_limove_12_18']<0)&
#                                 (data['hotmarket_18']==1), 1, 0)

# data['gent_00_18_urban'] = np.where((data['vul_gent_00']==1)&
#                                 (data['aboverm_ch_per_col_00_18']==1)&
#                                 (data['aboverm_pctch_real_hinc_00_18']==1)&
#                                 # (data['lostli_18']==1)&
#                                 # (data['ch_per_limove_12_18']<0)&
#                                 (data['hotmarket_18']==1), 1, 0)

#   [*] ((df['pop00flag']==1)&
#   [*] ((df['low_pdmt_medhhinc_18']==1)|(df['mix_low_medhhinc_18']==1))&
#   [*] ((df['lmh_flag_encoded']==1)|(df['lmh_flag_encoded']==4))&
#     # predominantly_LI  and mixed_low
#   [ ] ((df['change_flag_encoded'] == 1)|(df['ab_90percentile_ch']==1)|(df['rent_90percentile_ch']==1))&
#   [ ] (df['gent_90_00']==0)&
#   [ ] ((df['dp_PChRent'] == 1)|(df['dp_RentGap'] == 1)) & 
#     # see ~/git/displacement-typologies-og/code/3_create_lag_vars.r
#   [ ] (df['vul_gent_18']==1)&
#   [ ] (df['gent_00_18']==0), 1, 0)

# End testbed
###

# check <- area_li_19(ca_acs, ca_dec_li) 
# glimpse(check)
# check %>% filter(mix_low_medhhinc == 1) %>% glimpse()
# check %>% filter(!is.na(typology)) %>% glimpse()
# check %>% filter(GEOID == "06067008905") %>% glimpse()

# Hot market (rapid increase 2013-2018)
# 1. Above regaional median % ∆ home and rent value

# Rent gap and nearby pct change in rent 

# ongoing dispalcement

