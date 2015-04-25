aktuelleTabelle <- function (Spielplan)
  
  # Calculates the current table based on the matches already played
  
{
  # Filter to played games only
  gespielteSpiele <- Spielplan[!is.na(Spielplan$ToreHeim),]
  
  # Evaluate necessary miscellaneous variables
  AnzahlSpiele <- dim(gespielteSpiele)[1]
  AnzahlTeams <- dim(gespielteSpiele)[2]-4
  TeamNamen <- names(gespielteSpiele)[5:(AnzahlTeams+4)]
  
  return(Tabelle(Saison = SaisonSimulieren(Spielplan = gespielteSpiele,
                                           TeamNamen = TeamNamen,
                                           AnzahlSpiele = AnzahlSpiele,
                                           AnzahlTeams = AnzahlTeams), 
                 AnzahlTeams = AnzahlTeams, 
                 AnzahlSpiele = AnzahlSpiele))
}