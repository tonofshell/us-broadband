library(tidyverse)
library(ggplot2)
library(here)
library(ggmap)
library(maps)
library(mapdata)

wifi = read_csv(here("Datasets", "Boulder_CO_Public_Wifi_Usage.csv"))
ny_bb = read_csv(here("Datasets", "NY_Broadband_Availability.csv")) 
gam_job_bb = read_csv(here("Datasets", "US_Gaming_ Jobs_Broadband.csv")) 
people_net = read_csv(here("Datasets", "US_People_Without_Internet.csv"))
urban_bb = read_csv2(here("Datasets", "US_Urban_Rate_Broadband_Survey.csv"))

people_net$percent_below_poverty = (people_net$P_below_poverty / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_below_poverty)
people_net$percent_white = (people_net$P_white / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_white, use = "complete.obs")

ggplot(data = people_net, aes(x = percent_no_internet, y = percent_below_poverty)) + geom_point() + geom_smooth(method='lm',formula=y~x)

states = map_data("state")
new_york = subset(states, region %in% "new york") 
ny_map = ggplot(data = new_york, mapping = aes(x = long, y = lat, group = group)) + geom_polygon() + coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")

counties <- map_data("county")
ny_county <- subset(counties, region == "new york")
ny_county_bb = subset(ny_bb, ny_bb$`Municipality Type` == "County") %>% rename("subregion" = "Municipality Name") 
ny_county_bb$subregion = tolower(ny_county_bb$subregion)
ny_county_bb = inner_join(ny_county, ny_county_bb)
ny_county_bb$pop_dens = ny_county_bb$`2010 Muni Population` / ny_county_bb$`Muni Area (sq mi)`
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
ny_map + geom_polygon(data = ny_county_bb, aes(fill = ny_county_bb$pop_dens), color = "white") + geom_polygon(color = "black", fill = NA) + 
  scale_fill_distiller(palette = "Spectral", direction = -1, trans = "log10") + theme_light() + ditch_the_axes 

ggplot(data = ny_county_bb, aes(y = pop_dens, x = as.factor(ny_county_bb$`# Cable Providers`))) + geom_boxplot() + scale_y_continuous(trans = "log10")

ny_map + geom_polygon(data = ny_county_bb, aes(fill = ny_county_bb$`# Cable Providers`), color = "white") + geom_polygon(color = "black", fill = NA) + 
  scale_fill_distiller(palette = "Spectral", direction = -1) + theme_light() + ditch_the_axes

ny_map + geom_polygon(data = ny_county_bb, aes(fill = ny_county_bb$`% Hse Units Cable`), color = "white") + geom_polygon(color = "black", fill = NA) + 
  scale_fill_distiller(palette = "Spectral", direction = -1) + theme_light() + ditch_the_axes

ggplot(data = ny_county_bb, aes(y = pop_dens, x = as.factor(ny_county_bb$`# Fiber Providers`))) + geom_boxplot() + scale_y_continuous(trans = "log10")

                       