TabellenRaenge <- function (Tabelle) 
{
  AnzahlTeams <- dim(Tabelle)[2]
  RangX <- order (Tabelle$Punkte, Tabelle$TD, Tabelle$T,
                  decreasing = TRUE)
  Tabelle <- Tabelle[RangX, ]
  
  Rang <- 1:AnzahlTeams
  Rang <- data.frame(Rang)
  
  Tabelle <- cbind(Tabelle, Rang)
  
  return (Tabelle)
}