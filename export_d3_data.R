# Export Data for D3
library(readr)
library(jsonlite)
library(tidyverse)
library(here)
urban_bb = read_csv2(here("Datasets", "US_Urban_Rate_Broadband_Survey.csv"))

d3_test = tibble(dload_speed = as.numeric(urban_bb$`Download Bandwidth Mbps`), 
                 total_charge = as.numeric(urban_bb$`Total Charge`),
                 tech = urban_bb$Technology)
d3_test %>% toJSON() %>% write_lines(here("D3", "urban_bb_sub.json"))

d3_test[1:200,] %>% toJSON() %>% write_lines(here("D3", "urban_bb_small.json"))
