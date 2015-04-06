display_winProbs <- function (winProbData)
  
{
  require (reshape2)
  require (ggplot2)
  
  # Create match names
  
  winProbData <- cbind(
    paste (winProbData$TeamHeim, winProbData$TeamGast, sep=":"),
    winProbData [,3:5])
  
  # reorder the data frame from highest win probability to lowest
  winProbData <- winProbData [order(winProbData$win, decreasing = FALSE),]
  
  # Melt so that 
  winProbData <- melt (winProbData, id.vars=1)
  names (winProbData) <- c("match", "outcome", "probability")
  
  # Let ggplot know that data is already ordered
  winProbData$match <- factor(winProbData$match, levels=winProbData$match,
                              ordered = TRUE)
  
  # Create the plot
  
  plot <- ggplot (data = winProbData, aes (x=match, y=probability, 
                                           fill = outcome)) +
    geom_bar(stat="identity") + 
    coord_flip() + xlab("Spiel") + ylab("Wahrscheinlichkeit") +
    scale_fill_manual(values = c("#51b4ac", "#f5f5f5", "#d8b365"),
                      name = "",
                      labels = c("Heimsieg", "Unentschieden", "Auswärtssieg"))
  
  return(plot)
}