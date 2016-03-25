display_excitement <- function (outcomes, colour = "grey",
                                bar_colour = "steelblue",
                                Title = "", xlabel = "Verein", ylabel = "Spannung",
                                team_names = NA
                                )
  
  # display excitement levels based on possible outcomes
  # outcomes : data frame containing probabilities for 
  #            relevant outcomes for each team
  # team_names : names of teams, defaults to names(outcomes)
  # colour : background color
  # bar_colour : colour of bars
  # Title : chart title
  # xlabel, ylabel : axis labels
  
  
{
  require (ggplot2)
  require (vegan)
  
  excitement <- diversity(outcomes, index="invsimpson")
  
  if (is.na (team_names)) {
    team_names <- row.names(outcomes)
  
  }
  
  excitement <- data.frame(excitement = excitement, team_names = team_names)
  excitement$team_names <- factor(excitement$team_names, 
                                  levels=excitement$team_names[length(excitement$team_names):1])
  
  ymax <- max(excitement[,1])
  
  ggplot (data = excitement, 
          aes (x=team_names, y = excitement)) + 
    geom_bar (stat = "identity", fill = bar_colour) +
    ylim (c(0, ymax + 0.2)) +
    xlab (xlabel) +
    ylab (ylabel) +
    ggtitle (Title) +
    coord_flip ()
 
  
}
  