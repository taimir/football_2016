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

# load the library
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="cv", number=10, repeats=3)
# train the model
model <- train(result~., data=playoffs, method="lssvmRadial", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# dim reduct of the team-opponent structured data
library(tsne)
filtered_match_duplicates = playoffs#[-(seq(2,to=nrow(playoffs),by=2)),]
filtered_match_duplicates$color = rep(NA, length(filtered_match_duplicates$team))
for (i in 1:length(filtered_match_duplicates$team)) {
  if(as.character(filtered_match_duplicates[i,"result"]) == "win"){
    filtered_match_duplicates[i,"color"] = "green"
  }else if(as.character(filtered_match_duplicates[i,"result"]) == "draw"){
    filtered_match_duplicates[i,"color"] = "yellow"
  }else{
    filtered_match_duplicates[i,"color"] = "red"
  }
}

# =============================
# Data visualization & Feature selection
# =============================
# odds, odds_opponent are good
# CL_players opponent are good
# market_value is good as well
# UEFA_rank against UEFA_rank opponent / UEFA_points opponent is good
# GDP vs GDP_opponent seems good
# population seems sort of OK
# max2 seems OK

# max1 is messy
# past_scores vs past_scores opponent is somewhat messy
# FIFA_Points are somewhat messy
# age is very messy, very slight tendencies ...

no_names = playoffs2[c(10,28)] 
plot(as.matrix(no_names), t='n')
text(no_names, labels=paste(substr(filtered_match_duplicates$team, 1,2), substr(filtered_match_duplicates$opponent, 1,2), sep="-"), col=filtered_match_duplicates$color)

no_names =  filtered_match_duplicates[-c(1, 2, 17, 24)]
tsne_playoffs = tsne(as.matrix(no_names), 
                     k = 2, 
                     initial_dims = length(names(no_names)), 
                     perplexity = 3, 
                     max_iter = 2000)

library(zoom)
plot(tsne_playoffs, t='n')
text(tsne_playoffs, labels=paste(substr(filtered_match_duplicates$team, 1,2), substr(filtered_match_duplicates$opponent, 1,2), sep="-"), col=filtered_match_duplicates$color)
zm()


# attach t-sne columns to the data file
playoffs$t_sne_1 = tsne_playoffs[,1]
playoffs$t_sne_2 = tsne_playoffs[,2]
playoffs$result = factor(playoffs$result, levels=c("loss", "draw", "win"), ordered = TRUE)

model <- train(result~., data=playoffs, method="rf", trControl=control)
write.csv(playoffs, file="data/playoffs.csv", row.names = FALSE)

# dim reduct of the individual teams
team_data_only = playoffs[c(1,3,4,5,6,7,8,9,16,18,20)]
team_data_only_unique = team_data_only[!duplicated(team_data_only$team),]
#opp_data_only = playoffs[c(2,10,11,12,13,14,17,19)]

tsne_team = tsne(as.matrix(team_data_only_unique[-1]), 
                     k = 2, 
                     initial_dims = length(names(team_data_only_unique)), 
                     perplexity = 2, 
                     max_iter = 500)

plot(tsne_team, t='n')
text(tsne_team, labels=substr(team_data_only_unique$team, 1,2))

# library(FSelector)
# weights_info_gain = information.gain(result ~ ., data=playoffs)
# weights_info_gain
# 
# weights_info_gain[order(-weights_info_gain$attr_importance), , drop = FALSE]
