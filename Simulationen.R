Simulationen <- function (Spielplan, n=10000,
                          ModFaktor = 20, Heimvorteil = 65)
  
{
  # Teamnamen und Anzahl extrahieren
  TeamNamen <- names(Spielplan)
  AnzahlTeams <- length(TeamNamen) - 4
  TeamNamen <- TeamNamen[5:length(TeamNamen)]
  
  # Anzahl der Spiele ermitteln
  AnzahlSpiele = length (Spielplan$TeamHeim)
  
  # DataFrame initialisieren
  Alle <- data.frame()
  for (i in 1:n)
  {
    IterationSaison <- SaisonSimulieren(Spielplan, ModFaktor = ModFaktor,
                                        Heimvorteil = Heimvorteil,
                                        TeamNamen = TeamNamen,
                                        AnzahlTeams = AnzahlTeams,
                                        AnzahlSpiele = AnzahlSpiele)
    IterationTabelle <- Tabelle(IterationSaison, 
                                AnzahlTeams = AnzahlTeams,
                                AnzahlSpiele = AnzahlSpiele)
    Alle <- rbind (Alle, IterationTabelle)
  }
  return (Alle)
}