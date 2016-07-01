
# Load the data into R
playoffs = read.csv("data/EM2004.2012.publ.txt", header=TRUE, sep=" ")



odds = read.csv("data/QuotenEM2016.txt", header=TRUE, sep="\t")


value = read.csv("data/Wert2016.txt", header=TRUE, sep="\t")
names(value) = c("team", "avg_age", "market_value")
value$avg_age = as.numeric(gsub(",", ".", as.character(value$avg_age)))
value$market_value = as.numeric(gsub(",", ".", as.character(value$market_value)))


fifa16 = read.csv("data/FIFA2016.txt", header=TRUE, sep="\t")

uefa16 = read.csv("data/UEFA2016.txt", header=TRUE, sep="\t")

uefa16$Startplätze = NULL

neues16 = read.csv("data/Neues2016.txt", header=TRUE, sep="\t")
names(neues16) = c("team", "champ_league", "uefa_cup", "Grenzland", "trainer_age", "trainer_nation", "players_abroad", "max1", "max2")
View(neues16)


