calculate_excitement <- function (outcomes)
  
  # calculate excitement levels based on possible outcomes
  # outcomes : data frame containing probabilities for 
  #            relevant outcomes for each team
  
  
{
  require (vegan)
  
  excitement <- diversity(outcomes, index="invsimpson")
  
}
  