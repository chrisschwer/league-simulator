calculateELO <- function (Spielplan,
                          ModFaktor = 20, Heimvorteil = 65,
                          ToreSlope = 0.0017854953143549, 
                          ToreIntercept = 1.3218390804597700)
  
  # Calculates the current ELOs of all teams based on the games already played
  
{
  
  # split into games not played and games played
  
  played <- !is.na(Spielplan$ToreHeim)
  played_games <- Spielplan [played,]
  unplayed_games <- Spielplan [!played,]
  
  # Calculate ELO based on played games
  
  TeamNamen <- names(played_games)
  AnzahlTeams <- length(TeamNamen) - 4
  TeamNamen <- TeamNamen[5:length(TeamNamen)]
  
  # Anzahl der Spiele ermitteln
  AnzahlSpiele = length (played_games$TeamHeim)
  
  
  played_games <- SaisonSimulieren (played_games, 
                                    ModFaktor = 20, Heimvorteil = 65,
                                    TeamNamen = TeamNamen,
                                    AnzahlTeams = AnzahlTeams,
                                    AnzahlSpiele = AnzahlSpiele)
  
  # Vorläufige ELO-Werte übertragen
  
  ELO <- played_games[AnzahlSpiele, 5:(AnzahlTeams+4)]
  
  # ELO Werte für das letzte Spiel errechnen
  
  last_game <- played_games[AnzahlSpiele,]
  
  TeamHeim <- as.character(last_game$TeamHeim)
  TeamGast <- as.character(last_game$TeamGast)
  ToreHeim <- as.numeric(last_game$ToreHeim)
  ToreGast <- as.numeric(last_game$ToreGast)
  
  ELOHeim <- as.numeric(last_game[TeamHeim])
  ELOGast <- as.numeric(last_game[TeamGast])
  
  Ergebnis <- Spiel (ELOHeim = ELOHeim, ELOGast = ELOGast,
                     ToreHeim = ToreHeim, ToreGast = ToreGast,
                     ModFaktor = ModFaktor,
                     Heimvorteil = Heimvorteil)
  
  # Letzte ELO-Änderung übertragen
  
  ELO[TeamHeim] <- as.numeric(Ergebnis$ELOHeim)
  ELO[TeamGast] <- as.numeric(Ergebnis$ELOGast)
  
  # Vorhersagen für ungespielte Spiele erstellen
  
  spielVorhersagen <- unplayed_games[,1:2]
  
  dummyVorhersagen <- data.frame()
  
  for (spielnr in 1:length(unplayed_games$TeamHeim))
  {
    current_game <- unplayed_games[spielnr,]
    TeamHeim <- as.character (current_game$TeamHeim)
    TeamGast <- as.character (current_game$TeamGast)
    
    ELOHeim <- getElement(ELO, TeamHeim)
    ELOGast <- getElement(ELO, TeamGast)
    
    dummyVorhersagen <- rbind (dummyVorhersagen,
                               winProbs (ELOHome = ELOHeim, ELOAway = ELOGast,
                                         Heimvorteil = Heimvorteil,
                                         GoalSlope = ToreSlope,
                                         GoalIntercept = ToreIntercept))
  }
  
  spielVorhersagen = cbind (spielVorhersagen,
                            dummyVorhersagen)
  
  # Rückgabe der Werte (Vorläufig, bisher nur ELO-Werte)
  
  return (list (ELO, spielVorhersagen))
  
  
  
  
  
}