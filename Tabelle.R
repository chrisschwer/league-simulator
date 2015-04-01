Tabelle <- function (Saison, 
                     AnzahlTeams, AnzahlSpiele)

  # Berechnet aus einer Saison eine Tabelle
  
{
  # Teamnamen und Anzahl extrahieren
  
  Ergebnis <- data.frame()

  
  TeamHeim <- Saison$TeamHeim
  TeamGast <- Saison$TeamGast
  ToreHeim <- Saison$ToreHeim
  ToreGast <- Saison$ToreGast
    
  ToreDelta <- ToreHeim - ToreGast
  
  Resultat <- (sign(ToreDelta) + 1) / 2
  
  PunkteHeim <- floor ((Resultat * 2) + Resultat)
  PunkteGast <- floor (((1 - Resultat) * 2) + 1 - Resultat)
  
    
  HeimErgebnis <- data.frame (Team = TeamHeim, Punkte = PunkteHeim, 
                              TD = ToreDelta,
                              T = ToreHeim, GT = ToreGast)
  GastErgebnis <- data.frame (Team = TeamGast, Punkte = PunkteGast,
                              TD = -ToreDelta,
                              T = ToreGast, GT = ToreHeim)
  
  Ergebnis <- rbind(Ergebnis, HeimErgebnis, GastErgebnis)
  
  
  Punkte <- tapply(Ergebnis$Punkte, Ergebnis$Team, sum)
  TD <- tapply(Ergebnis$TD, Ergebnis$Team, sum)
  T <- tapply(Ergebnis$T, Ergebnis$Team, sum)
  GT <- tapply(Ergebnis$GT, Ergebnis$Team, sum)
  Team <- names(Punkte)

  # Hilfsweise Berechnung der Rangpunktzahl
  Rang <- 10000 * Punkte + 100 * TD + T
  
  Ergebnis <- data.frame (Team, Punkte, TD, T, GT, Rang)
  
  # Sortieren
  RangX <- order (Ergebnis$Rang, decreasing = TRUE)
  Ergebnis <- Ergebnis[RangX, ]
  
  # Tabellenplätze eintragen
  Ergebnis$Rang <- 1:AnzahlTeams
  
  return (Ergebnis)
}