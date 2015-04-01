SaisonSimulieren <- function (Spielplan,
                              ModFaktor = 20, Heimvorteil = 65,
                              TeamNamen, AnzahlTeams,
                              AnzahlSpiele)
  
  # Simuliert eine Saison auf Basis eines Spielplans
  #
  # Spielplan : DF mit den einzelnen Spielen und
  #             initialen ELO-Werten in der ersten Zeile
  # ModFaktor : Geschwindigkeit der Anpassung der ELO-Werte
  # Heimvorteil : ELO-Heimvorteil
  
{
  for (spielnr in 1:AnzahlSpiele) 
  {
    # Spiel auswählen
    spieldaten <- Spielplan [spielnr, ]
    
    # Heim und Gastmannschaft ermitteln
    TeamHeim <- as.character(spieldaten$TeamHeim)
    TeamGast <- as.character(spieldaten$TeamGast)
    
    # Ergebnis ermitteln
    ToreHeim <- spieldaten$ToreHeim
    ToreGast <- spieldaten$ToreGast
    
    # Klären, ob Simulieren notwendig
    Simulieren <- is.na (ToreHeim)
    
    #ELO-Werte ermitteln
    ELOHeim <- as.numeric(spieldaten[TeamHeim])
    ELOGast <- as.numeric(spieldaten[TeamGast])
    
    # ELO-Werte nach Spieltag und ggf. Ergebnis ermitteln
    if (Simulieren) 
    {
      Zufall1 <- runif(1)
      Zufall2 <- runif(1)
      Ergebnis <- Spiel (ELOHeim = ELOHeim, ELOGast = ELOGast,
                         ZufallHeim = Zufall1, ZufallGast = Zufall2,
                         ModFaktor = ModFaktor,
                         Heimvorteil = Heimvorteil,
                         Simulieren = TRUE)
    }
    
    else
    {
      Ergebnis <- Spiel (ELOHeim = ELOHeim, ELOGast = ELOGast,
                         ToreHeim = ToreHeim, ToreGast = ToreGast,
                         ModFaktor = ModFaktor,
                         Heimvorteil = Heimvorteil)
    }
    
    # Tore übertragen
    if (Simulieren)
    {
      Spielplan[spielnr, ]$ToreHeim <- Ergebnis$ToreHeim
      Spielplan[spielnr, ]$ToreGast <- Ergebnis$ToreGast
    }
    
    # Rückgabe, wenn letztes Spiel
    if (spielnr == AnzahlSpiele)
    {
      return (Spielplan)
    }
    
    # Auslesen, Anpassen ELO und Rückschreiben des nächsten Spieles
    spieldaten_naechstes <- Spielplan [spielnr + 1, ]
    
    spieldaten_temp <- spieldaten
    spieldaten_temp$TeamHeim <- spieldaten_naechstes$TeamHeim
    spieldaten_temp$TeamGast <- spieldaten_naechstes$TeamGast
    spieldaten_temp$ToreHeim <- spieldaten_naechstes$ToreHeim
    spieldaten_temp$ToreGast <- spieldaten_naechstes$ToreGast
    
    spieldaten_temp[TeamHeim] <- Ergebnis$ELOHeim
    spieldaten_temp[TeamGast] <- Ergebnis$ELOGast
    Spielplan [spielnr + 1, ] <- spieldaten_temp
  }
  
  return (Spielplan)
  
}