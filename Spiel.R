Spiel <- function (ELOHeim, ELOGast, ToreHeim, ToreGast, 
                ZufallHeim, ZufallGast, 
                ModFaktor = 20, Heimvorteil = 65, 
                Simulieren = FALSE, 
                ToreSlope = 0.0017854953143549, ToreIntercept = 1.3218390804597700)

# Berechnet aus einem (simulierten) Spielergebnis die neuen ELO-Stärken
# 
# ELOHeim, ELOGast : ELO Werte der beiden Teams
# ToreHeim, ToreGast : Erzielte Tore je Team
# ZufallHeim, ZufallGast : Normalverteilte Zufallsvariablen, falls simuliert wird
# ModFaktor : Modifikator für die ELO-Anpassung
# Simulation : Soll das Spiel simuliert werden?

{
  # Berechnung des Delta der ELO-Stärke
  ELODelta <- ELOHeim + Heimvorteil - ELOGast
  
  if (Simulieren) 
    # Gegebenenfalls Zufallsergebnis berechnen
  {
    ToreHeimDurchschnitt <- max (ELODelta * ToreSlope + ToreIntercept,
                                 0.001)
    ToreGastDurchschnitt <- max ((-ELODelta) * ToreSlope 
                                 + ToreIntercept,
                                 0.001)
    ToreHeim <- qpois (p = ZufallHeim, lambda = ToreHeimDurchschnitt)
    ToreGast <- qpois (p = ZufallGast, lambda = ToreGastDurchschnitt)
  }
  
  # Absolutes ELODelta auf 400 kappen
  ELODelta <- min (max (ELODelta, -400), 400)
  
  # ELO-Prognose errechnen
  ELOProb <- 1 / (1 + 10 ^ (-ELODelta / 400))
  
  # Tor-Modifikator errechnen
  TD <- ToreHeim - ToreGast
  Vorzeichen <- sign (TD)
  Ergebnis <- (Vorzeichen + 1) / 2
  TorModifikator <- sqrt ( max (Vorzeichen * TD, 1) )
  
  
  # ELO-Modifikator errechnen
  ELOModifikator <- (Ergebnis - ELOProb) * TorModifikator * ModFaktor
  
  # Neue ELOWerte berechnen
  ELOHeim <- ELOHeim + ELOModifikator
  ELOGast <- ELOGast - ELOModifikator
  
  returnDF <- data.frame(ELOHeim = ELOHeim, ELOGast = ELOGast, 
                         ToreHeim, ToreGast, 
                         ELOProb = ELOProb)
  names(returnDF) <- c("ELOHeim", "ELOGast", "ToreHeim", "ToreGast",
                       "ELOProb")
  return (returnDF)
  
}