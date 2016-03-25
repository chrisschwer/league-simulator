matchValue <- function (matchProbs, teamExcitement) {
  
  # calculate the value of a match defined by the product of the 
  # breadth of possible outcomes and the relevance to the participating
  # teams
  #
  # matchProbs : Win Draw Loss probabilities, e.g. as provided by calculateELO[[2]]
  # teamExcitement : relevance, as provided by calculate_excitement
  
  require (plyr)
  
  value <- cbind(matchProbs[,1:2], SpannungMatch = calculate_excitement(matchProbs[,3:5]))
  teamExcitement <- data.frame (TeamHeim = names(teamExcitement), SpannungHeim = teamExcitement)
  
  colnames(teamExcitement) <- c ("TeamHeim", "SpannnungHeim")
  value <- join (x = value, y = teamExcitement, type = "left")
  
  colnames(teamExcitement) <- c ("TeamGast", "SpannnungGast")
  value <- join (value, teamExcitement, type = "left")
  
  value <- cbind (value, Spannung = apply (value[,3:5], 1, prod))
  
  return (value)
  
}