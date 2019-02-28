# Export Data for D3
library(readr)
library(tidyverse)
library(jsonlite)
library(tidyverse)
library(here)
library(sf)
library(mapdata)
library(readxl)

urban_bb = read_csv2(here("Datasets", "US_Urban_Rate_Broadband_Survey.csv"))

d3_test = tibble(dload_speed = as.numeric(urban_bb$`Download Bandwidth Mbps`), 
                 total_charge = as.numeric(urban_bb$`Total Charge`),
                 tech = urban_bb$Technology)
d3_test %>% toJSON() %>% write_lines(here("D3", "urban_bb_sub.json"))

d3_test[1:200,] %>% toJSON() %>% write_lines(here("D3", "urban_bb_small.json"))


us_sf = st_as_sf(map("county", plot = FALSE, fill = TRUE)) %>% separate(ID, c("state", "subregion"), sep = ",")

land_area = read_excel(here("Datasets", "LND01.xls")) %>% select(STCOU, LND110210D)
land_area[land_area$STCOU == 46113,]$STCOU = 46102
acs_17 = land_area %>% rename(GEOID = STCOU, land_area = LND110210D) %>% right_join(readRDS(here("Datasets", "acs_17_vals.rds")))

%>% inner_join(as_tibble(us_sf)[,c(4,16,17)], by = "GEOID")

acs_17_mod = acs_17[1:length(acs_17[[1]]) ,c(1,3,21,22,23,31,34,35,36)] %>%
  bind_cols(data.frame(pop_dens = (acs_17$total_pop / acs_17[,2]))) %>% 
  bind_cols(as_tibble(acs_17[, -c(1,2,3,21,22,23,31,34,35,36)] / acs_17$total_pop)) %>% 
  rename("pop_dens" = land_area, "avg_fam_inc" = aggr_fam_inc) %>%
  separate(NAME, c("county", "state"), sep = ", ")