# Ideas

# Important attributes:
# Tore, Team, Gegner, Heim, Quoten, Marktwert, Fifa.Platz, UEFA.Platz, UEFA.Punkte, CL.Spieler, 
# UEFA.Cup.Spieler, 

# attributes in question: GDP, Altersschnitt, FIFA.Punkte, Alter.Trainer

# Unused attributes:
# Max1, Max2, Grenzland, Legionaere, Population, Population_gegner

# To the teams: add some indicator of the class of the
# goal keeper, goal maker, trainer

# Detected tendencies:
# plot(playoffs$Tore, playoffs$UEFA.Platz) ---> Only teams with high UEFA scores actually 
# manage to score more goals in the UEFA tournament.


# Ideas:
# try to come up with a probabilistic intuition behind weighting the years of the presented championships
# If group games are to be used, consider the following strategy: if a strong team is to be paired
# with a weaker in the next round (finals) if it ranks on second place, then consider not playing 
# with top-performance in the group games.

# The additional info provided may be tricky: Olympic medals show the general sport fitness of the country 
# but is not (always) representative for football. For example Portugal is a strong team, but have
# won embarasingly few medals.