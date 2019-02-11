library(extrafont)

#font_import()
loadfonts(device = "win")
b_size = 18
d_bg_color = "#323232"

get_d_bg_color = function() {
  return(d_bg_color)
}

theme_master = function (base_size = b_size, base_line_size = base_size/22, base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = "Pragati Narrow") +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = base_size - 2, face = "italic"),
      plot.caption = element_text(color = "grey50"),
      axis.title = element_text(face = "bold"),
      legend.text = element_text(color = "grey25"),
      axis.text.x = element_text(color = "grey50"), 
      axis.text.y = element_text(color = "grey50"),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_line(colour = "grey75")
    )
}

theme_master_dark = function (base_size = b_size, base_line_size = base_size/22, base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = "Pragati Narrow") +
    theme(
      plot.background = element_rect(color = d_bg_color, fill = d_bg_color),
      panel.background = element_rect(color = d_bg_color, fill = d_bg_color),
      plot.title = element_text(face = "bold", color = "white"),
      plot.subtitle = element_text(size = base_size - 2, face = "italic", color = "white"),
      plot.caption = element_text(color = "grey50"),
      axis.title = element_text(face = "bold", color = "white"),
      legend.title = element_text(color = "white"),
      legend.background = element_rect(fill = d_bg_color),
      legend.key = element_rect(fill = d_bg_color),
      legend.text = element_text(color = "grey75"),
      axis.text.x = element_text(color = "grey50"), 
      axis.text.y = element_text(color = "grey50"),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_line(colour = "grey25")
    )
}

theme_map = function(base_size = b_size, base_line_size = base_size/22, base_rect_size = base_size/22, dark_theme = FALSE) {
  if (dark_theme) {
    bg_color = d_bg_color
    theme_master_dark() + 
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(color = bg_color), 
        panel.grid.minor = element_line(color = bg_color),
        panel.grid = element_line(color = bg_color),
        axis.text = element_text(color = bg_color),
        legend.position = "bottom",
        legend.key.width = unit(1, units = "cm")
      )
  } else {
    bg_color = 'white'
    theme_master() + 
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_line(color = bg_color), 
        panel.grid.minor = element_line(color = bg_color),
        panel.grid = element_line(color = bg_color),
        axis.text = element_text(color = bg_color),
        legend.position = "bottom",
        legend.key.width = unit(1, units = "cm")
      )
  }
 
}

hide_legend = theme(
  legend.position = "none",
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank()
)

color_pal = function(n, type = "discrete", reverse = FALSE, spectral = FALSE) {
  #Discrete - distinct categories
  #Segmented - two sided continuous with neutral center
  #Continuous - like segmented but w/o neutral center
  order_mid = c("grey", "pale_yellow", "purple", "dark grey")
  colors_mid = c("#C8DCC8", "#E6E673", "#9146F0", "#5A5A5A")
  
  
  if (spectral) {
    order_spect = c("extra_light", "light", "normal", "dark")
    colors_warm = c("#FFD747", "#FFAF3C", "#EB5028", "#D73219")
    colors_cool = c("#64E687", "#1ED7AA", "#0FA0F5", "#0A55CD")
   
  } else {
    order_main = c("mid_offset", "light", "normal", "dark")
    colors_warm = c("#FFD747", "#FF7837", "#EB5028", "#D73219")
    colors_cool = c("#B1E651", "#1ED2D7", "#0FA0F5", "#0A55CD")
  }
  
  final = switch (as.character(n),
                  "1" = switch (type,
                                "discrete" = colors_warm[3],
                                "segmented" = colors_mid[4],
                                "continuous" = colors_mid[3],
                                "warm" = colors_warm[1],
                                "cool" = colors_cool[3]
                  ),
                  "2" = switch (type, 
                                "discrete" = c(colors_warm[3], colors_cool[3]),
                                "segmented" = c(colors_warm[3], colors_cool[3]),
                                "continuous" = c(colors_warm[3], colors_cool[3]),
                                "warm" = c(colors_warm[2], colors_warm[3]),
                                "cool" = c(colors_cool[2], colors_cool[3])
                  ), 
                  "3" = switch (type,
                                "discrete" = c(colors_warm[3], colors_warm[1], colors_cool[3]),
                                "segmented" = c(colors_warm[3], colors_mid[1], colors_cool[3]),
                                "continuous" = c(colors_warm[3], colors_mid[2], colors_cool[3]),
                                "warm" = c(colors_warm[], colors_warm[2], colors_warm[3]),
                                "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3])
                  ),
                  "4" = switch (type, 
                                "discrete" = c(colors_mid[4], colors_warm[3], colors_warm[1], colors_cool[3]),
                                "segmented" = c(colors_warm[4], colors_warm[3], colors_mid[1], colors_cool[4]),
                                "continuous" = c(colors_warm[4], colors_warm[3], colors_cool[3], colors_cool[4]),
                                "warm" = c(colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                                "cool" = c(colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4])
                  ),
                  "5" = switch (type, 
                                "discrete" = c(colors_mid[4], colors_warm[3], colors_warm[1], colors_cool[3], colors_mid[3]),
                                "segmented" = c(colors_warm[4], colors_warm[3], colors_mid[1], colors_cool[3], colors_cool[4]),
                                "continuous" = c(colors_warm[3], colors_warm[2], colors_mid[2], colors_cool[2], colors_cool[3]),
                                "warm" = c(colors_mid[2], colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                                "cool" = c(colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4])
                  ),
                  "6" = switch (type, 
                                "discrete" = c(colors_mid[4], colors_warm[3], colors_warm[1], colors_cool[3], colors_mid[3], colors_cool[1]),
                                "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[3], colors_cool[4]),
                                "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_cool[1], colors_cool[2], colors_cool[3]),
                                "warm" = c(colors_cool[1], colors_mid[2], colors_warm[1], colors_warm[2], colors_warm[3], colors_warm[4]),
                                "cool" = c(colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4])
                  ),
                  "7" = switch (type, 
                                "discrete" =warning("No palette for this type and number"), 
                                "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_mid[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                                "continuous" = c(colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[2], colors_cool[1], colors_cool[2], colors_cool[3]),
                                "warm" = warning("No palette for this type and number"),
                                "cool" = warning("No palette for this type and number")
                  ),
                  "8" = switch (type, 
                                "discrete" = warning("No palette for this type and number"),
                                "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                                "continuous" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4]),
                                "warm" = warning("No palette for this type and number"),
                                "cool" = warning("No palette for this type and number")
                  ),
                  "9" = switch (type, 
                                "discrete" = warning("No palette for this type and number"),
                                "segmented" = c(colors_warm[4], colors_warm[3], colors_warm[2], colors_warm[1], colors_mid[1], colors_cool[1], colors_cool[2], colors_cool[3], colors_cool[4]),
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


