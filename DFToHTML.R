DFToHTML <- function (probDF)
  
  # converts a data.frame with probabilities into html code
  
{
  require (xtable)
  
  # Convert to strings
  probDF <- apply(probDF, FUN=probToString, MARGIN=c(1,2))
  
  alignment <- c("l", rep("r", ncol(probDF)))
  
  HTML_table <- print(xtable(probDF, align=alignment), type="html")
  
  return(HTML_table)
}