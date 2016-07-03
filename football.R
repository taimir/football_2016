population = read.csv("data/additional_info_countries.txt", header=TRUE, sep="\t")
odds = read.csv("data/QuotenEM2016.txt", header=TRUE, sep="\t")
value = read.csv("data/Wert2016.txt", header=TRUE, sep="\t")
names(value) = c("team", "avg_age", "market_value")
value$avg_age = as.numeric(gsub(",", ".", as.character(value$avg_age)))
value$market_value = as.numeric(gsub(",", ".", as.character(value$market_value)))
fifa16 = read.csv("data/FIFA2016.txt", header=TRUE, sep="\t")
uefa16 = read.csv("data/UEFA2016.txt", header=TRUE, sep="\t")
neues16 = read.csv("data/Neues2016.txt", header=TRUE, sep="\t")
names(neues16) = c("team", "champ_league", "uefa_cup", "Grenzland", "trainer_age", "trainer_nation", "players_abroad", "max1", "max2")

# Load the data into R
playoffs = read.csv("data/playoffs.csv", header=TRUE, sep=",")

# format as factors
playoffs$host = as.factor(playoffs$host)
playoffs$host_opponent = as.factor(playoffs$host_opponent)
playoffs$vicinity = as.factor(playoffs$vicinity)
playoffs$vicinity_opponent = as.factor(playoffs$vicinity_opponent)
playoffs$nationality_coach = as.factor(playoffs$nationality_coach)
playoffs$nationality_coach_opponent = as.factor(playoffs$nationality_coach_opponent)


library(FSelector)
weights_info_gain = information.gain(result ~ ., data=playoffs)
weights_info_gain

weights_info_gain[order(-weights_info_gain$attr_importance), , drop = FALSE]
