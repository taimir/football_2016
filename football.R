# Load the data into R
playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")

# format as factors
playoffs$host = as.factor(playoffs$host)
playoffs$host_opponent = as.factor(playoffs$host_opponent)
playoffs$nationality_coach = as.factor(playoffs$nationality_coach)
playoffs$nationality_coach_opponent = as.factor(playoffs$nationality_coach_opponent)


population = read.csv("data/additional_info_countries.txt", header=TRUE, sep="\t")

# Important attributes:
# Tore, Team, Gegner, Heim, Quoten, Marktwert, Fifa.Platz, UEFA.Platz, UEFA.Punkte, CL.Spieler, 
# UEFA.Cup.Spieler, 

# attributes in question: GDP, Altersschnitt, FIFA.Punkte, Alter.Trainer

# Unused attributes:
# Max1, Max2, Grenzland, Legionaere, Population, Population_gegner

# To the teams: add some indicator of the class of the
# goal keeper, goal maker, trainer

odds = read.csv("data/QuotenEM2016.txt", header=TRUE, sep="\t")


value = read.csv("data/Wert2016.txt", header=TRUE, sep="\t")
names(value) = c("team", "avg_age", "market_value")
value$avg_age = as.numeric(gsub(",", ".", as.character(value$avg_age)))
value$market_value = as.numeric(gsub(",", ".", as.character(value$market_value)))


fifa16 = read.csv("data/FIFA2016.txt", header=TRUE, sep="\t")

uefa16 = read.csv("data/UEFA2016.txt", header=TRUE, sep="\t")

neues16 = read.csv("data/Neues2016.txt", header=TRUE, sep="\t")
names(neues16) = c("team", "champ_league", "uefa_cup", "Grenzland", "trainer_age", "trainer_nation", "players_abroad", "max1", "max2")
View(neues16)


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


library(caret)
rf_model <- train(result ~ ., data=playoffs, 
                  method="rf", 
                  trControl=trainControl(method="cv", number=5), 
                  prox=TRUE, 
                  allowParallel=TRUE)

