probToString <- function (probability)
  
  # converts a probability to a string percentage,
  # with probabilities between .0 and .01 as "<1",
  # and probabilities between .99 and 1 as ">99"
  
{
  if (is.character(probability))
  {
    return(probability)
  }
  else if (probability==0) 
  {
    return("0")
  }
  else if (probability==1) 
  {
    return("100")
  }
  else if (probability<.01)
  {
    return("<1")
  }
  else if (probability>.99)
  {
    return(">99")
  }
  else
  {
    return(as.character(round(100*probability), 0))
  }
}