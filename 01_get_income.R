library(tidyverse)
library(tidycensus)
library(alarmdata)
library(geomander)
library(sf)

county_df <- function(st) {
  
  # elections
  st_map <- alarm_50state_map(st)
  # census
  st_inc <- get_acs(
    geography = "county", 
    variables = c(medincome = "B19013_001"), 
    summary_var = "B01001_001",
    state = st,
    geometry = TRUE,
    year = 2020)
  # project up to county
  st_inc$pre_20_rep <- geo_estimate_up(from = st_map, to = st_inc, value = st_map$pre_20_rep_tru)
  st_inc$pre_20_dem <- geo_estimate_up(from = st_map, to = st_inc, value = st_map$pre_20_dem_bid)
  
  st_df <- st_inc |> 
    mutate(biden_vshare = pre_20_dem / (pre_20_dem + pre_20_rep)) |> 
    mutate(state = st, 
           name = str_remove_all(NAME, " County.*"),
           .before = 1) |> 
    rename(medincome = summary_est) |> 
    select(-c(variable, moe, summary_moe, NAME))
  
  st_df |> 
    st_drop_geometry()
}

al_df <- county_df("AL")
pa_df <- county_df("PA")
ct_df <- county_df("CT")


states_df <- bind_rows(al_df, pa_df, ct_df) |> 
  as_tibble()

write_csv(states_df, "data/counties.csv")
