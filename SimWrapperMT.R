SimWrapperMT <- function (Spielplan, n=10000,
                          ModFaktor = 20, Heimvorteil = 65)
  
  # Führt die Simulationen durch und erstellt dann
  # eine Tabelle der Wahrscheinlichkeiten für den
  # jeweiligen Rang nach Team, sortiert nach 
  # der erwarteten Durchschnittsplazierung je Team
  
{
  require(parallel)
  
  n <- n - (n %% 8)
  nBit <- n / 8
  
  Ergebnis1 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis2 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis3 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis4 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis5 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis6 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis7 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  Ergebnis8 <- mcparallel(Simulationen (Spielplan = Spielplan, n = nBit,
                                        ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil))
  
  ErgebnisList <- mccollect(
    list (Ergebnis1, Ergebnis2, Ergebnis3, Ergebnis4,
          Ergebnis5, Ergebnis6, Ergebnis7, Ergebnis8))
  
  Ergebnis <- rbind (ErgebnisList[[1]], ErgebnisList[[2]], 
                     ErgebnisList[[3]], ErgebnisList[[4]], 
                     ErgebnisList[[5]], ErgebnisList[[6]], 
                     ErgebnisList[[7]], ErgebnisList[[8]])
  
  TableErgebnis <- table (Ergebnis$Team, Ergebnis$Rang) / n
  
  Rangschnitt <- tapply (Ergebnis$Rang, Ergebnis$Team, sum) / 1000
  RangX <- order (Rangschnitt, decreasing = FALSE)
  
  TableErgebnis <- TableErgebnis [RangX, ]
  
  return (TableErgebnis)
}