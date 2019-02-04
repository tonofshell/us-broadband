library(tidyverse)
library(ggplot2)
library(here)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(scales)
library(ggnewscale)
library(extrafont)

# font_import()
# loadfonts(device = "win")

##People Without Internet (American Community Survey 2016)

people_net = read_csv(here("Datasets", "US_People_Without_Internet.csv"))
state_regions = bind_rows(filter(state.fips, !str_detect(polyname, ":")), filter(state.fips, str_detect(polyname, ":main")))[4:5] %>% bind_rows(tibble(division = c(9, 9), abb = c("AK", "HI")))
state_regions$division = state_regions$division  %>% ordered(levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"))

people_net = merge(people_net, tibble(region = state_regions$division, state = state_regions$abb))

people_net$percent_below_poverty = (people_net$P_below_poverty / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_below_poverty)
people_net$percent_white = (people_net$P_white / people_net$P_total) * 100
cor(people_net$percent_no_internet, people_net$percent_white, use = "complete.obs")

theme_master = function (base_size = 15, base_line_size = base_size/22, base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = "Pragati Narrow") +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = base_size - 2),
      plot.caption = element_text(color = "grey50"),
      axis.title = element_text(face = "bold"),
      legend.text = element_text(color = "grey75")
    )
}

color_pal = c("#FF501E", "#FFA73D", "#FFED8F", "#D4D4D4", "#B8FF8C", "#30FF9F", "#0FA8D1")

ggplot(data = people_net, aes(x = percent_no_internet, y = percent_below_poverty)) + 
  geom_point(aes(size = P_total, color = region), alpha = 0.6) +
  geom_smooth(method='lm',formula=y~x, color = "dark grey") + 
  labs(title = "Communities with Higher Poverty Rates Have Less Internet Access", 
       subtitle = "US Counties that have a higher percentage of people with no Internet also \nhave a higher percentage of residents below the poverty line", 
       x = "Percent with no Internet", 
       y = "Percent Below Poverty Line", 
       caption="US Census 2016 American Community Survey", 
       color = "Region", size = "Total Population") + 
  scale_x_log10(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) + 
  scale_y_log10(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_master()
