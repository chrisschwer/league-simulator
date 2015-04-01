SiegNiederlage <- function (Spielplan, TeamHeim, TeamGast, n=1000)
  
  # Simuliert für das Spiel zunächst ein 1:0, dann ein 0:1.
  # Gibt die Differenz der Wahrscheinlichkeiten je Team und Platz
  # zurück.
  
{
  Spielplan2 <- NeuesErgebnis (Spielplan, TeamHeim, TeamGast, 1, 0)
  Sieg <- SimWrapper (Spielplan2, n)
  Spielplan2 <- NeuesErgebnis (Spielplan, TeamHeim, TeamGast, 0, 1)
  Niederlage <- SimWrapper (Spielplan2, n)
  
  Teams <- dimnames(Sieg)[[1]]
  Delta <- Sieg
  
  for (t in Teams)
  {
    Delta[t,] <- Sieg[t,] - Niederlage [t,]
  }
  
  return (Delta)
}