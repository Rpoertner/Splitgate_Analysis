library(tidyverse)
library(jsonlite)
library(httr)


#Send GET call with auth header to tracker.gg
split_json <- GET("https://public-api.tracker.gg/v2/splitgate/standard/matches/steam/76561198850994506", 
                  add_headers('TRN-Api-Key' = ""))

#Extract Json
alljson1<-fromJSON(content(split_json, "text", encoding = "UTF-8"))

#Extract match stat segments(Json)
match_segments <- alljson1$data$matches$segments

queue_type <- alljson1$data$matches$metadata$queue

#initialize isInt for pagination iteration
isInt <- 1

#do the above, while the data has "Next"
while(grepl(isInt, alljson1$data$metadata) == "TRUE" && isInt <= 9){
  
  
  split_json <- GET("https://public-api.tracker.gg/v2/splitgate/standard/matches/steam/76561198850994506", 
                    add_headers('TRN-Api-Key' = ""),
                    query = list('Next' = isInt))
  
  
  tJson <- fromJSON(content(split_json, "text", encodeing = "UTF-8"))
  
  match_segments <- c(match_segments, tJson$data$matches$segments)
  queue_type <-c (queue_type, tJson$data$matches$metadata$queue)
  
  alljson1$data$metadata <- tJson$data$metadata
  
  isInt <- isInt + 1
  
  Sys.sleep(1)
}

#get components of Data
split_big <- NULL
for(i in 1:length(match_segments)){
  this_row_i <- data.frame(
    Win = ifelse(match_segments[[i]]$metadata$result == 'victory', 1,0),
    Kills = match_segments[[i]]$stats$kills$value,
    Assists = match_segments[[i]]$stats$assists$value,
    Deaths = match_segments[[i]]$stats$deaths$value,
    headshotsLanded = match_segments[[i]]$stats$headshotsLanded$value,
    killsPerMinute = match_segments[[i]]$stats$killsPerMinute$value,
    distancePortaled = match_segments[[i]]$stats$distancePortaled$value,
    portalsSpawned = match_segments[[i]]$stats$portalsSpawned$value,
    damageDealt = match_segments[[i]]$stats$damageDealt$value
    
  )
  split_big <- bind_rows(split_big, this_row_i)
}


#get rid of outliers
split_big <- subset(split_big, headshotsLanded < 21 & distancePortaled < 1500)
split_big$queueType <- factor(split_big$queueType, levels = c("ranked", "casual"))

write.csv(split_big, "D:/STAT_330/Splitgate/split_big.csv")


split_ranked <- NULL
split_casual <- NULL

for(i in 1:length(split_big)){
  
  this_row_i <- data.frame(
    Win = split_big$Win[[i]],
    Kills = split_big$Kills[[i]],
    Assists = split_big$Assists[[i]],
    Deaths = split_big$Deaths[[i]],
    headshotsLanded = split_big$headshotsLanded[[i]],
    killsPerMinute = split_big$killsPerMinute[[i]],
    distancePortaled = split_big$distancePortaled[[i]],
    portalsSpawned = split_big$portalsSpawned[[i]],
    damageDealt = split_big$damageDealt[[i]]
  )
  
  if(split_big$queueType[[i]] == "ranked"){
    split_ranked <-bind_rows(split_ranked, this_row_i)
  }
  else{
    split_casual <- bind_rows(split_casual, this_row_i)
  }
}

summary(split_big)

write.csv(split_ranked, "D:/STAT_330/Splitgate/split_ranked.csv")
write.csv(split_casual, "D:/STAT_330/Splitgate/split_casual.csv")

#EDA

researchTest <- function(split_data){
#boxplot comparing each variable type to wins
par(mfrow = c(3,3))
boxplot(Kills~Win, data = split_data)
boxplot(Assists~Win, data = split_data)
boxplot(Deaths~Win, data = split_data)
boxplot(headshotsLanded~Win, data = split_data)
boxplot(killsPerMinute~Win, data = split_data)
boxplot(distancePortaled~Win, data = split_data)
boxplot(portalsSpawned~Win, data = split_data)
boxplot(damageDealt~Win, data = split_data)
par(mfrow = c(1,1))

#quick summary
summary(split_data)
#plot variables against other variables
plot(~Kills+Deaths+Assists+headshotsLanded+killsPerMinute+
       distancePortaled+portalsSpawned+damageDealt,data=split_data)

#split data between train and test
set.seed(121)
train_rows <- sample(length(split_data$Kills), length(split_data$Kills) - (length(split_data$Kills)/3))
split_train <- split_data[train_rows, ]
split_test <- split_data[-train_rows, ]
dim(split_data)

#check that both datasets are similar
summary(split_train)
summary(split_test)
#Datasets are Similar enough

#model:
#   logit(Win = 1) =beta0 + beta1 Kills + beta2 Deaths + 
#                       beta3 Assist + beta4 headshotsLanded +
#                         beta5 distancePortaled + beta6 portalsSpawned +
#                           beta7 damageDealt
split_out <- glm(Win~Kills +  
                   Deaths + 
                   Assists +
                   headshotsLanded + 
                   killsPerMinute + 
                   distancePortaled +
                   portalsSpawned +
                   damageDealt, 
                 data = split_train, family = "binomial")

#Table of Estimates and Std Errors
summary(split_out)$coefficients[-1,]

#Table of Coefficients in a probability sense
exp(coef(split_out))[-1]
exp(confint(split_out))[-1,]


#Ho: there is no statistically significant Kills effect
#Ha: there is a statistically significant Kills effect
#z-value: 2.095
#p-value: .036
#formal: we reject Ho in favor of Ha: There is a statistically significant Kills 
#   effect on odds of Winning at the .05 significance level

#informal: There is a statistically significant Kills effect on 
#   odds of winning(p-value = .036)

#conclusion: holding all else constant, for a 1 unit increase in Kills, 
# we estimate an expected increase in odds of winning by 28.9%(95% CI: 2.2, 65.4) 


#Ho: there is no statistically significant Deaths effect
#Ha: there is a statistically significant Deaths effect
#z-value: -3.807
#p-value: .000141
#formal: we reject Ho in favor of Ha: There is a statistically significant Deaths
#   effect on odds of winning at the .05 significance level

#informal: There is a statistically significant Deaths effect on 
#   odds of winning(p-value = .0001)

#conclusion: holding all else constant, for a 1 unit increase in Deaths, 
#   we estimate an expected decrease in odds of winning by 25.1%(95% CI: 13.9, 36.2)


#Ho: there is no statistically significant Assists effect
#Ha: there is a statistically significant Assists effect
#z-value: 3.006
#p-value: .002649
#formal: we reject Ho in favor of Ha: There is a statistically significant Assists
#   effect on odds of winning at the .05 significance level

#informal:There is a statistically significant Assists effect on 
#   odds of winning(p-value = .0026)

#conclusion: holding all else constant, for a 1 unit increase in Assists, 
#   we estimate an expected decrease in odds of winning by 37.1%(95% CI: 12.7, 70.4)


#Ho: there is no statistically significant headshotsLanded effect
#Ha: there is a statistically significant headshotsLanded effect
#z-value: 1.057
#p-value: .290
#formal: We fail to reject Ho: there is no statistically significant 
#   headshotsLanded effect on odds of winning at the .05 significance level

#informal: There is no statistically significant headshotsLanded effect on 
#   odds of winning(p-value =.290)

#conclusion: Do nothing


#Ho: there is no statistically significant killsPerMinute effect
#Ha: there is a statistically significant killsPerMinute effect
#z-value: -.245
#p-value: .806513
#formal: fail to reject
#informal: There is no effect
#conclusion: Do nothing


#Ho: there is no statistically significant distancePortaled effect
#Ha: there is a statistically significant distancePortaled effect
#z-value: .924
#p-value: .355741
#formal: fail to reject
#informal: There is no effect
#conclusion: Do nothing

#Ho: there is no statistically significant portalsSpawned effect
#Ha: there is a statistically significant portalsSpawned effect
#z-value: -1.477
#p-value: .139598
#formal: fail to reject
#informal: There is no effect
#conclusion: do Nothing

#Ho: there is no statistically significant damageDealt effect
#Ha: there is a statistically significant damageDealt effect
#z-value: -1.244
#p-value: .213393
#formal: fail to reject
#informal: There is no effect
#conclusion: do Nothing



#Ho: There is no effect on odds of winning from active play
#Ha: There is a statistically significant effect from active play
split_reduced <- glm(Win ~ Kills + killsPerMinute + damageDealt + Deaths, 
                     data = split_train, family = binomial)

anova(split_reduced, split_out, test = "Chisq")

#formal: we reject Ho in favor of Ha: There is a statistically significant 
#   effect from active play on odds of winning at the .05 significance level

#informal: There is a statistically significant 
#   effect from active play on odds of winning(p-value = .004692)

library(pROC)
roc(split_train$Win, predict(split_out, type = "response"),
    plot = TRUE,
    print.auc = TRUE)

#test
roc(split_test$Win, predict(split_out, newdata = split_test, type = "response"),
    plot = TRUE, add = TRUE, col = "green",
    print.auc = TRUE, print.auc.y = 0.45)

}

researchTest(split_big)
researchTest(split_ranked)
researchTest(split_casual)







