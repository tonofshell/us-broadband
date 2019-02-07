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

theme_map = function(base_size = 15, base_line_size = base_size/22, base_rect_size = base_size/22) {
  theme_master() + 
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
}

hide_legend = theme(
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank()
)

color_pal = function(n, type = "discrete", reverse = FALSE) {
  order_main = c("extra_light", "light", "normal", "dark")
  colors_warm = c("#FFD747", "#FFAF3C", "#EB5028", "#D73219")
  colors_cool = c("#64E687", "#1ED7AA", "#0FA0F5", "#0A55CD")
  
  order_mid = c("neutral", "spectral")
  colors_mid = c("#C8DCC8", "#E6E673")
  
  final = switch (as.character(n),
          "1" = switch (type,
            "discrete" = colors_warm[3],
            "continuous" = colors_cool[3],
            "warm" = colors_warm[3],
            "cool" = colors_cool[3]
          ),
          "2" = switch (type, 
                     "discrete" = c(colors_warm[3], colors_cool[3]),
                     "continuous" = c(colors_warm[3], colors_cool[3]),
                     "warm" = c(colors_warm[2], colors_warm[3]),
                     "cool" = c(colors_cool[2], colors_cool[3])
          ), 
          "3" = switch (type,
            "discrete" = c(colors_warm[3], colors_mid[1], colors_cool[3]),
            "continuous" = c(colors_warm[3], colors_mid[2], colors_cool[3]),
            "warm" = c(colors_warm[], colors_warm[2], colors_warm[3]),
            "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3])
          ),
          "4" = switch (type, 
                      "discrete" = c(colors_warm[3], colors_warm[1], colors_cool[1], colors_cool[3]),
                      "continuous" = c(colors_warm[4], colors_warm[3], colors_cool[3], colors_cool[4]),
                      "warm" = c(colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                      "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4])
          ),
          "5" = switch (type, 
                      "discrete" = c(colors_warm[3], colors_warm[1], colors_mid[1], colors_cool[1], colors_cool[3]),
                      "continuous" = c(colors_warm[3], colors_warm[2], colors_mid[2], colors_cool[2], colors_cool[3]),
                      "warm" = c(colors_middle[2], colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                      "cool" = c(colors_middle[2], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4])
          ),
          "6" = switch (type, 
                      "discrete" = c(colors_warm[4], colors_warm[3], colors_warm[1], colors_cool[1], colors_cool[3], colors_cool[4]),
                      "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_cool[1], colors_cool[2], colors_cool[3]),
                      "warm" = warning("No palette for this type and number"),
                      "cool" = warning("No palette for this type and number")
          ),
          "7" = switch (type, 
                      "discrete" = c(colors_warm[4], colors_warm[3], colors_warm[1], colors_mid[1], colors_cool[1], colors_cool[3], colors_cool[4]),
                      "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3]),
                      "warm" = warning("No palette for this type and number"),
                      "cool" = warning("No palette for this type and number")
          ),
          "8" = switch (type, 
                      "discrete" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[3], colors_cool[3], colors_cool[4]),
                      "continuous" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                      "warm" = warning("No palette for this type and number"),
                      "cool" = warning("No palette for this type and number")
          ),
          "9" = switch (type, 
                      "discrete" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[1], colors_mid[2], colors_cool[3], colors_cool[3], colors_cool[4]),
                      "continuous" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                      "warm" = warning("No palette for this type and number"),
                      "cool" = warning("No palette for this type and number")
          ) 
  )
  
  if (reverse) {
    return(rev(final))
  }
  return(final)
}


