SimWrapper <- function (Spielplan, n=10000,
                        ModFaktor = 20, Heimvorteil = 65)
  
  # Führt die Simulationen durch und erstellt dann
  # eine Tabelle der Wahrscheinlichkeiten für den
  # jeweiligen Rang nach Team, sortiert nach 
  # der erwarteten Durchschnittsplazierung je Team
  
{
  Ergebnis <- Simulationen (Spielplan = Spielplan, n = n,
                            ModFaktor = ModFaktor,
                            Heimvorteil = Heimvorteil)
  TableErgebnis <- table (Ergebnis$Team, Ergebnis$Rang) / n
  
  Rangschnitt <- tapply (Ergebnis$Rang, Ergebnis$Team, sum) / 1000
  RangX <- order (Rangschnitt, decreasing = FALSE)
  
  TableErgebnis <- TableErgebnis [RangX, ]
  
  return (TableErgebnis)
}