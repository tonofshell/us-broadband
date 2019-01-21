library(tidyverse)
library(ggplot2)
library(here)
library(ggmap)
library(maps)
library(mapdata)

#import the data
wifi = read_csv(here("Datasets", "Boulder_CO_Public_Wifi_Usage.csv"))
ny_bb = read_csv(here("Datasets", "NY_Broadband_Availability.csv")) 
gam_job_bb = read_csv(here("Datasets", "US_Gaming_ Jobs_Broadband.csv")) 
people_net = read_csv(here("Datasets", "US_People_Without_Internet.csv"))
urban_bb = read_csv2(here("Datasets", "US_Urban_Rate_Broadband_Survey.csv"))

w = 7
h = 5

#people without internet
people_net$percent_below_poverty = (people_net$P_below_poverty / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_below_poverty)
people_net$percent_white = (people_net$P_white / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_white, use = "complete.obs")

ggplot(data = people_net, aes(x = percent_no_internet, y = percent_below_poverty)) + geom_point() + geom_smooth(method='lm',formula=y~x) + labs(title = "Relationship Between Poverty and Internet Access", subtitle = "By US County", x = "Percent with no Internet", y = "Percent Below Poverty Line", caption="US Census 2016 American Community Survey")
ggsave("Charts/internet vs poverty.pdf", width = w, height = h)

#urban broadband survey
urban_bb$`Total Charge` = round(as.numeric(urban_bb$`Total Charge`))
urban_bb$`Download Bandwidth Mbps` = round(as.numeric(urban_bb$`Download Bandwidth Mbps`))
hist(urban_bb$`Total Charge`)
hist(urban_bb$`Download Bandwidth Mbps`)
cor(urban_bb$`Total Charge`, urban_bb$`Download Bandwidth Mbps`)
# urban_bb = subset(urban_bb, `Download Bandwidth Mbps` <= 1000)
# urban_bb = subset(urban_bb, `Total Charge` <= 750)
hist(urban_bb$`Total Charge`)
hist(urban_bb$`Download Bandwidth Mbps`)
cor(urban_bb$`Total Charge`, urban_bb$`Download Bandwidth Mbps`)

ggplot(data = urban_bb, aes(x = `Total Charge`, y = `Download Bandwidth Mbps`)) + geom_point(color = "grey") + 
  geom_smooth(method='lm', formula=y~x, color = "grey50") + 
  geom_point(aes(color = Technology)) + 
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Relationship Between Download Speed and Price", 
       subtitle = "By Internet Service Provider",
       x = "Total Monthly Internet Cost (US)",
       y = "Download Speed (mbps)", caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/download vs price by tech.pdf", width = w, height = h)

ggplot(data = urban_bb, aes(x = `Total Charge`, y = `Download Bandwidth Mbps`)) + geom_point(color = "grey") + 
  geom_smooth(method='lm', formula=y~x, color = "grey50") + 
  geom_point(data = subset(urban_bb, State == "New York"), 
             aes(color = State)) + 
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Relationship Between Download Speed and Price", 
       subtitle = "By Internet Service Provider",
       x = "Total Monthly Internet Cost (US)",
       y = "Download Speed (mbps)", caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/download vs price.pdf", width = w, height = h)

#NY municipal data (as county)
states = map_data("state")
new_york = subset(states, region %in% "new york") 
ny_map = ggplot(data = new_york, mapping = aes(x = long, y = lat, group = group)) + geom_polygon() + coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")

counties <- map_data("county")
ny_county <- subset(counties, region == "new york")
ny_county_bb = subset(ny_bb, ny_bb$`Municipality Type` == "County") %>% rename("subregion" = "Municipality Name") 
ny_county_bb$subregion = tolower(ny_county_bb$subregion)
ny_county_bb = inner_join(ny_county, ny_county_bb)
ny_county_bb$pop_dens = ny_county_bb$`2010 Muni Population` / ny_county_bb$`Muni Area (sq mi)`

remove_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ggplot(data = ny_county_bb, 
       aes(y = pop_dens, x = as.factor(ny_county_bb$`# Fiber Providers`))) + 
  geom_boxplot() + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "Relationship Between Fiber Providers and Population Density",
       subtitle = "by New York County",
       x = "Number of Fiber Providers per County", 
       y = "Population Density of County (ppl/sq mi)",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny fiber prov boxplot county.pdf", width = w, height = h)

ggplot(data = ny_county_bb, 
       aes(y = pop_dens, 
           x = as.factor(ny_county_bb$`# Cable Providers`))) + 
  geom_boxplot() + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "Relationship Between Cable Providers and Population Density",
       subtitle = "by New York County",
       x = "Number of Cable Providers per County",
       y = "Population Density of County (ppl/sq mi)",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny cable prov boxplot.pdf", width = w, height = h)

w = 11
h = 8

ny_map + geom_polygon(data = ny_county_bb, 
                      aes(fill = ny_county_bb$pop_dens), 
                      color = "white") + 
  geom_polygon(color = "black", fill = NA) + 
  scale_fill_distiller(palette = "Spectral", direction = -1, trans = "log10") +
  theme_light() + 
  remove_axes + 
  labs(title = "Population Density",
       subtitle = "by New York County",
       fill = "People Per\nSquare Mile",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny pop dens county.pdf", width = w, height = h)


ny_map + geom_polygon(data = ny_county_bb, 
                      aes(fill = ny_county_bb$`# Cable Providers`), 
                      color = "white") + 
  geom_polygon(color = "black", fill = NA) + 
  scale_fill_distiller(palette = "Spectral", direction = -1) + 
  theme_light() + 
  remove_axes + 
  labs(title = "Number of Cable Providers",
       subtitle = "by New York County",
       fill = "Number of \nCable Providers",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny cable prov county.pdf", width = w, height = h)

ny_map + geom_polygon(data = ny_county_bb, 
                      aes(fill = ny_county_bb$`% Hse Units Cable`), 
                      color = "white") + 
  geom_polygon(color = "black", fill = NA) + 
  scale_fill_distiller(palette = "Spectral", direction = -1) + 
  theme_light() + 
  remove_axes + 
  labs(title = "Cable Coverage",
       subtitle = "by New York County",
       fill = "Percent of County \nWith Cable",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny perc cable county.pdf", width = w, height = h)

#boulder wifi data
wifi_vendor_table = as_tibble(as.data.frame(table(wifi$Vendor)))

                       