NeuesErgebnis <- function (Spielplan, TeamHeim, TeamGast,
                           ToreHeim, ToreGast)
  
  # Ersetzt im Spielplan das Ergebnis der Partie zwischen
  # TeamHeim und TeamGast mit dem Ergebnis ToreHeim:ToreGast
  
{
  for (i in 1:dim(Spielplan)[1])
  {
    if ((Spielplan$TeamHeim[i] == TeamHeim) & 
          (Spielplan$TeamGast[i] == TeamGast)) 
    {
      Spielplan$ToreHeim[i] <- ToreHeim
      Spielplan$ToreGast[i] <- ToreGast
      return (Spielplan)
    }
  }
  
  return (Spielplan)

}