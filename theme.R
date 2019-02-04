library(extrafont)

#font_import()
loadfonts(device = "win")

theme_master = function (base_size = 15, base_line_size = base_size/22, base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = "Pragati Narrow") +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = base_size - 2),
      plot.caption = element_text(color = "grey50"),
      axis.title = element_text(face = "bold"),
      legend.text = element_text(color = "grey25"),
      axis.text.x = element_text(color = "grey50"), 
      axis.text.y = element_text(color = "grey50"),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_line(colour = "grey75")
    )
}

hide_legend = theme(
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank()
)

color_pal = c("#FF501E", "#FFA73D", "#FFED8F", "#C2D19F", "#B8FF8C", "#30FF9F", "#0FA8D1")

color_pal2 = color_pal[c(1,7)]

color_pal3 = color_pal[c(1,4,7)]

color_pal4 = color_pal[c(1,2,6,7)]

color_pal5 = color_pal[c(1,2,4,6,7)]

color_pal_warm = color_pal[c(4,3,2,1)]

color_pal_cool = color_pal[c(4,5,6,7)]