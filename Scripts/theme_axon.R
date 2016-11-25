

theme_axon <- function (base_family = "Open Sans",
                        base_size = 11)
{
  require(ggplot2)
  palette <- c(
    "#FFFFFF",
    "#F0F0F0",
    "#D9D9D9",
    "#BDBDBD",
    "#969696",
    "#737373",
    "#525252",
    "#252525",
    "#000000"
  )
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  color.axis = palette[5]
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.border = element_rect(color = color.background),
      panel.grid.major = element_line(
        color = color.grid.major,
        size = 0.25,
        linetype = 2
      ),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = color.axis),
      axis.line.y = element_line(color = color.axis),
      axis.ticks = element_line(color = color.axis),
      legend.key = element_rect(fill = NA,
                                color = NA),
      legend.text = element_text(size = rel(0.8),
                                 color = color.axis.title),
      plot.title = element_text(
        color = color.axis.title,
        size = rel(1.2),
        vjust = 1.25
      ),
      axis.text.x = element_text(size = rel(0.9),
                                 color = color.axis.text),
      axis.text.y = element_text(size = rel(0.9),
                                 color = color.axis.text),
      axis.title.x = element_text(
        size = rel(1),
        color = color.axis.title,
        vjust = 0
      ),
      axis.title.y = element_text(
        size = rel(1),
        color = color.axis.title,
        vjust = 1.25
      ),
      plot.subtitle = element_text(color = color.axis.title),
      plot.caption = element_text(color = color.axis.title)
    )
}