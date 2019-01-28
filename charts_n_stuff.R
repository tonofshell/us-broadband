library(tidyverse)
library(ggplot2)
library(here)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(scales)
#devtools::install_github("eliocamp/ggnewscale")
library(ggnewscale)

#import the data
#wifi = read_csv(here("Datasets", "Boulder_CO_Public_Wifi_Usage.csv"))
ny_bb = read_csv(here("Datasets", "NY_Broadband_Availability.csv")) 
gam_job_bb = read_csv(here("Datasets", "US_Gaming_ Jobs_Broadband.csv")) 
people_net = read_csv(here("Datasets", "US_People_Without_Internet.csv"))
urban_bb = read_csv2(here("Datasets", "US_Urban_Rate_Broadband_Survey.csv"))
consum_ins = read.csv(here("Datasets", "cbg_patterns.csv"))
#acs_17 = readRDS(here("Datasets", "acs_17_vals.rds"))

state_regions = bind_rows(filter(state.fips, !str_detect(polyname, ":")), filter(state.fips, str_detect(polyname, ":main")))[4:5] %>% bind_rows(tibble(division = c(9, 9), abb = c("AK", "HI")))
state_regions$division = state_regions$division  %>% ordered(levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"))

people_net = merge(people_net, tibble(region = state_regions$division, state = state_regions$abb))

w = 7
h = 5

#people without internet
people_net$percent_below_poverty = (people_net$P_below_poverty / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_below_poverty)
people_net$percent_white = (people_net$P_white / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_white, use = "complete.obs")

ggplot(data = people_net, aes(x = percent_no_internet, y = percent_below_poverty)) + 
  geom_point(aes(size = P_total, color = region), alpha = 0.6) + 
  geom_smooth(method='lm',formula=y~x, color = "dark grey") + 
  labs(title = "Communities with Higher Poverty Rates Have Less Internet Access", 
       subtitle = "US Counties that have a higher percentage of people with no Internet also have a \nhigher percentage of residents below the poverty line", 
       x = "Percent with no Internet", 
       y = "Percent Below Poverty Line", 
       caption="US Census 2016 American Community Survey", 
       color = "Region", size = "Total Population") + 
  scale_x_log10(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) + 
  scale_y_log10(breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  theme_light()
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
urban_bb$isp_size = -log(as.numeric(urban_bb$Weight))
urban_bb$isp_size = (urban_bb$isp_size - min(urban_bb$isp_size) + 1)^3


perc_not_bb = sum(urban_bb$`Download Bandwidth Mbps` < 25) / length(urban_bb$`Download Bandwidth Mbps`) * 100

chart_labels = tibble(text = c('FCC Broadband \nCutoff', 'Trendline'), x = c(8.5, 700), y = c(25, 2000))

ggplot(data = urban_bb, aes(x = `Total Charge`, y = `Download Bandwidth Mbps`)) + 
  geom_smooth(method='lm', formula=y~x, color = "brown") + 
  geom_point(color = "red", alpha = 0.5) + 
  geom_hline(yintercept = 25, linetype = 2) + 
  geom_label(data = chart_labels, aes(x = x, y = y, label = text), alpha = 0.95) + 
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Faster Download Speeds Mean Costlier Internet", 
       subtitle = paste("Even in urban areas, ", round(perc_not_bb, 0), "% of survey respondents had internet slower \nthan the 25 mbps minimum download speed for broadband as defined by the FCC", sep = ""),
       x = "Total Monthly Internet Cost (USD)",
       y = "Download Speed (mbps)", caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/download vs price.pdf", width = w, height = h)

ggplot(data = filter(urban_bb, Technology != "Other"), aes(x = `Total Charge`, y = `Download Bandwidth Mbps`)) + 
  geom_smooth(method='lm', formula=y~x, color = "grey") + 
  geom_point(aes(color = Technology), alpha = 0.5) + 
  geom_hline(yintercept = 25, linetype = 2) + 
  facet_wrap(~ Technology) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Download Speeds and Costs Vary by Network Technology",
       x = "Total Monthly Internet Cost (USD)",
       y = "Download Speed (mbps)", 
       caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/download vs price.pdf", width = w, height = h)

ggplot(data = urban_bb, aes(x = `Total Charge`, y = `Download Bandwidth Mbps`)) + 
  geom_smooth(method='lm', formula=y~x, color = "grey") + 
  geom_point(aes(color = Year), alpha = 0.5) + 
  geom_hline(yintercept = 25, linetype = 2) + 
  facet_wrap(~ Year) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(title = "Download Speeds and Costs Vary by Year",
       subtitle = "Even as faster speeds have become more common over time, the relationship \nbetween price and download speed has changed very little.",
       x = "Total Monthly Internet Cost (USD)",
       y = "Download Speed (mbps)", 
       caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/download vs price by year.pdf", width = w, height = h)

ggplot(data = urban_bb, aes(x = factor(`Year`), y = `Total Charge`)) +
  geom_violin(aes(color = Year, fill = Year)) +
  scale_y_continuous(trans = "log10", breaks = c(10, 30, 50, 100, 150, 300, 1000)) +
  labs(title = "Monthly Internet Costs Have Increased Slightly",
       subtitle = "Most people paid around $50 for Internet access every month in 2015-2018. \nHowever, less people are paying less than $50 and more between $50-100 in 2018 than 2015.",
       x = "Year",
       y = "Total Monthly Internet Cost (USD)",
       caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/price by year.pdf", width = w, height = h)

chart_labels = tibble(text = c('FCC \nBroadband \nCutoff'), x = c(0.75), y = c(25))
ggplot(data = urban_bb, aes(x = factor(`Year`), y = `Download Bandwidth Mbps`)) +
  geom_violin(aes(color = Year, fill = Year)) +
  scale_y_continuous(trans = "log10", breaks = c(1, 5, 20, 50, 100, 300, 1000, 10000)) +
  geom_hline(yintercept = 25, linetype = 2, color = "grey55") + 
  geom_label(data = chart_labels[1,], aes(x = x, y = y, label = text), alpha = 0.95) + 
  labs(title = "Download Speeds Have Increased Significantly",
       subtitle = "High-speed Internet, especially Gigabit Internet subscribtions have increased noticeably between 2015 and 2018 \nStill, many people remained on Internet plans with speeds below the FCC minimum for broadband \nat less than 25 mbps in 2018",
       x = "Year",
       y = "Download Speed (mbps)",
       caption = "Urban Rate Broadband Survey") +
  theme_light()
ggsave("Charts/price by year.pdf", width = w, height = h)

urban_bb[c(2,5)] %>% table() %>% as_tibble() %>%
  ggplot(aes(x = Year, fill = Technology, y = n)) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "More People are Choosing Fiber to the Home and Fixed Wireless Internet",
       subtitle = "The amount of survey respondents who had Cable or DSL Internet has decreased between 2015 and 2018",
       x = "Year", 
       y = "Percentage of Respondents") +
  scale_y_continuous(labels = percent_format()) +
  theme_light()
ggsave("Charts/tech by year.pdf", width = w, height = h)


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
  geom_boxplot(aes(color = as.factor(ny_county_bb$`# Fiber Providers`))) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "Population Density and Number of Fiber Providers Increase Together",
       subtitle = "The more dense a New York county is, the more likely it is to have several fiber providers",
       x = "Number of Fiber Providers per County", 
       y = "Population Density of County (ppl/sq mi)",
       caption = "New York State Broadband Availability By Municipality") + 
  theme(legend.position="none")
ggsave("Charts/ny fiber prov boxplot county.pdf", width = w, height = h)

ggplot(data = ny_county_bb, 
       aes(y = pop_dens, 
           x = as.factor(ny_county_bb$`# Cable Providers`))) + 
  geom_boxplot(aes(fill = as.factor(ny_county_bb$`# Cable Providers`))) + 
  scale_y_continuous(trans = "log10") + 
  labs(title = "Population Density and Number of Cable Providers Increase Together",
       subtitle = "The more dense a New York county is, the more likely it is to have several cable providers",
       x = "Number of Cable Providers per County",
       y = "Population Density of County (ppl/sq mi)",
       caption = "New York State Broadband Availability By Municipality") + 
  theme_light() +
  theme(legend.position="none") 
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
  theme_light() + 
  remove_axes + 
  labs(title = "Number of Cable Providers",
       subtitle = "by New York County",
       fill = "Number of \nCable Providers",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny cable prov county.pdf", width = w, height = h)

ny_map + geom_polygon(data = ny_county_bb, 
                 aes(fill = ny_county_bb$`% Hse Units Cable`)) + 
  scale_fill_gradient(low = "yellow", 
                       high = "red", "Percent of Units with Cable" ) +
  new_scale_fill() +
  geom_polygon(data = ny_county_bb, 
               aes(fill = ny_county_bb$pop_dens), 
               alpha = 0.5) + 
  scale_fill_gradient(low = "white",
                      high = "blue",
                      trans = "log10") +
  geom_polygon(color = "black", fill = NA) + 
  theme_light() + 
  remove_axes + 
  labs(title = "Cable Coverage",
       subtitle = "by New York County",
       fill = "Population Density (ppl/sq mi)",
       fill2 = "Percent of County /n with Cable",
       caption = "New York State Broadband Availability By Municipality")
ggsave("Charts/ny perc cable county.pdf", width = w, height = h)

#boulder wifi data
#wifi_vendor_table = as_tibble(as.data.frame(table(wifi$Vendor)))

#ACS 17 Data


                       