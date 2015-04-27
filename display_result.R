display_result <- function (result, colour = "grey", 
                            low = "white", high = "steelblue",
                            Titel = "Endplatzierung",
                            labeling = FALSE, Teams = 18)
  
  # Displays results from SimWrapper in a heatmap
  # result : results to display
  # colour : background colour for tiles
  # low : colour for lower end of scale
  # high : colour for higher end of scale
  # Titel : text of the title line of the chart
  # labeling : boolean, if true the tiles of the heatmap
  #            are labeled with the values in percent
  
{
  require(reshape2)
  require(ggplot2)
  
  if (labeling) 
  {
    result <- round(result*100,0)
  }
  
  result.m <- melt (result)
  plot <- ggplot (result.m) + 
    aes (Var1, Var2) + 
    geom_tile(aes (fill=value), 
              colour = colour) + 
    scale_fill_gradient (low = low, high = high,
                         name = "p") +
    labs (x = "Verein", y = "Platz") +
    ggtitle (Titel) +
    theme_grey()
  plot <- plot + 
    theme (axis.text.x = element_text (size = rel (0.8), angle = 330,
                                       hjust = 0, colour = "grey50"))
  plot <- plot +
    theme (axis.ticks = element_line (linetype = 0)) +
    scale_y_reverse(breaks = 1:Teams)
  
  if (labeling) 
  {
    plot <- plot + geom_text (aes (label = value))
  }
  
  
  return (plot)  
}