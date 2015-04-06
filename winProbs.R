winProbs <- function (ELOHome, ELOAway,
                      Heimvorteil = 65,
                      GoalSlope = 0.0017854953143549,
                      GoalIntercept = 1.3218390804597700,
                      completeResults = FALSE)
  
  # Calculate Win-Draw-Loss-Probabilities based on ELO Values

{
  goalMatrix <- matrix (nrow = 10, ncol = 10)
  ELODelta <- ELOHome + Heimvorteil - ELOAway

  win <- 0
  draw <- 0
  loss <- 0
  
  for (i in 1:10) 
  {
    for (j in 1:10)
    {
      goalMatrix[i,j] <- dpois(i-1, (ELODelta*GoalSlope + GoalIntercept)) *
        dpois(j-1, (-ELODelta*GoalSlope + GoalIntercept))
    }
  }
  
  for (i in 1:10)
  {
    for (j in 1:10)
    {
      if (i>j) 
      {
        win <- win + goalMatrix[i,j]
      }
      else if (i<j)
      {
        loss <- loss + goalMatrix[i,j]
      }  
      else 
      {
        draw <- draw + goalMatrix[i,j]
      }
    }
  }
  if (completeResults)
  {
    return (list (goalMatrix, data.frame(win=win, draw=draw, loss=loss)))
  }
  
  return (data.frame(win=win, draw=draw, loss=loss))
}
