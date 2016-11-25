theme_axon <- function (base_size = 12, base_family = "Open Sans") 
{
  require(ggplot2)
  require(ggthemes)
  
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(line = element_line(colour = "black"), rect = element_rect(fill = "white", 
                                                                      linetype = 0, colour = NA), text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]), 
           axis.text = element_text(), 
           axis.ticks = element_blank(), axis.line = element_blank(), 
           legend.background = element_rect(), legend.position = "bottom", 
           legend.direction = "horizontal", legend.box = "vertical", 
           panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]), 
           panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0, 
                                                                         size = rel(1.5), face = "bold"), plot.margin = unit(c(1, 
                                                                                                                               1, 1, 1), "lines"), strip.background = element_rect()))
}