groupResultsDF <- function (results,
                            labels = c("Meister", "Champions League", "CL Quali", "Europa League",
                                       "Platz 7", "Mittelfeld", "Relegation", "Abstieg"),
                            groups = cbind(c(1,1), c(2,3), c(4,4), c(5,6),
                                       c(7,7), c(8,15), c(16,16), c(17,18))) {
  
  # groups results into a data frame of labeled groups
  # results : data frame with n probabilities for n teams
  # labels : vector of strings, labels for the groups
  # groups : 2xn matrix of integers, lower and upper bounds for groups
  
  outputDF <- data.frame (matrix(ncol=length(labels), nrow=dim(results)[1]))
  colnames (outputDF) <- labels
  rownames (outputDF) <- rownames (results)
  
  for (i in 1:length(labels)) {
    lower <- groups [1,i]
    upper <- groups [2,i]
    if (lower == upper) {
      newcol <- results[,lower]
    } else {
      range <- c(lower:upper)
      newcol <- rowSums(results[, range])
    }
    outputDF[,i] <- newcol
    
  }
  
  return (outputDF)
}