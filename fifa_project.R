library(tidyverse)
library(party) #for making a decision tree

#finding out the best liverpool player based on potential

potential <- fifa %>% select(Name, Nationality, Club, Overall, Potential) %>% filter(Club == "Liverpool") 


ggplot(potential, aes(Name, Potential)) + geom_point()

#Alisson is the best Liverpool player based on potential followed by Salah
# let us do some regression analysis. let us see what stats are contributing to significance of overall
# so let us make a dataframe only containg the variables we need

analysis1 <- fifa %>%  select(Overall, Crossing, Finishing, HeadingAccuracy, ShortPassing, Volleys, Dribbling, Curve, FKAccuracy, LongPassing, BallControl, Acceleration, SprintSpeed, Agility, Reactions, Balance, ShotPower, Jumping, Stamina, Strength, LongShots, Aggression, Interceptions, Positioning, Vision, Penalties, Composure, Marking, StandingTackle, SlidingTackle)
a <- lm(Overall ~ ., analysis1)

summary(a)

#looking for positive intercepts and highly significant 
# surprise that finishing is not really very significant with just one star (98.6% significant)


#checking the same for potential 
analysis2 <-  fifa %>%  select(Potential, Crossing, Finishing, HeadingAccuracy, ShortPassing, Volleys, Dribbling, Curve, FKAccuracy, LongPassing, BallControl, Acceleration, SprintSpeed, Agility, Reactions, Balance, ShotPower, Jumping, Stamina, Strength, LongShots, Aggression, Interceptions, Positioning, Vision, Penalties, Composure, Marking, StandingTackle, SlidingTackle)
b <- lm(Potential ~., analysis2)

summary(b)

c = broom::tidy(a) # tidy up the results we obtained.

#let us test an hypothesis... does age effect overall and potential

analysis3 <- fifa %>%  select(Age, Overall, Potential)
age <- lm(Age ~ Overall + Potential, analysis3)
ss
summary(age)

#provides really subtle and true analysis upon pondering... yes with age overall increases but potential not necessarily as we know some top OLD players do not really have potential to improve further

##ggplot for Age vs Overall and Potential 

ggplot(data = analysis3) +
  +   aes(x = Age, y = Overall, color = Potential) +
  +   geom_point() +
  +   scale_colour_viridis_c(option  = "plasma") +
  +   theme_minimal() +
  +   theme(legend.position = 'bottom')

#which top 10 club has the most players above 80 overall

number <- fifa %>%  select(Club, Overall) %>% filter(Overall >= 80) %>%  group_by(Club) %>% summarise(count = n()) %>% filter(count >= 16)

##ggplot

ggplot(data = number) +
  aes(x = Club, y = count) +
  geom_point(fill = "#ef562d", size = 3) +
  labs(title = "Count of players in top 10 teams with Overall 80 in FIFA 19",
       y = "Count of Players") +
  theme_minimal()


## top clubs with high potential players (>= 90)

number1 <- fifa %>%  select(Club, Potential) %>% filter(Potential >= 90) %>%  group_by(Club) %>% summarise(count = n()) %>% filter(count >= 5)

## ggplot

ggplot(data = number1) +
  aes(x = Club, y = count) +
  geom_point(fill = "#0c4c8a", size = 3) +
  labs(title = "FIFA 19: Clubs with highest potential players (>=90) ",
       y = "Count of Players") +
  theme_minimal()

######################################

#Some machine learning analysis on the data: we'll use supervised learning first... starting from random forest
number3 <- analysis3 %>%  mutate(category = if_else(Overall >= 80 & Potential >= 90, 1, 0))

#random forest library
library(randomForest)


number3$category <- as.factor(number3$category)

set.seed(1234)
ml <- sample(2, nrow(number3), replace=TRUE, prob = c(0.8,0.2))
ml_train <- number3[ml == 1,]
ml_test <- number3[ml == 2,]

set.seed(222)
rf <- randomForest(category ~., data = ml_train,
                   ntree = 100,
                   mtry = 8,
                   importance = T,
                   proximity = T)

print(rf)

####prediction and confusion matrix

library(caret)

pred <- predict(rf, ml_train)
confusionMatrix(pred, ml_train$category)

pred1 <- predict(rf, ml_test)
confusionMatrix(pred1, ml_test$category)

############################

plot(rf)


varImpPlot(rf, 
           sort = T,
           n.var = 3,
           main = "Important variables")


#######################################################################################


#PCA analysis on the FIFA dataset

data <- read.csv(file.choose(), header = T)

class <- ifelse(data$Overall >= 80 & data$Potential >= 90, 1, 0)

data <- data.frame(data, class)

##### data sampling

set.seed(123)
data1 <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[data1 == 1,]
test <- data[data1 == 2,]

###### since this is a fifa dataset it is expected that we will have a lot of variables which gives rise to collinerity because of their high variance
### the model will probably overfit, so let us perform PCA and extract only the important variables in principal components

#selecting required variables

train1 <- train %>% select(1,3,4,8,9,90) 

tp <- train1 %>%  group_by(class) %>%  summarise(count = n())

#### now lets see how correlated the variables are to one and another

library(psych)
pairs.panels(train1[], gap = 0) # we don't really have a lot of correlated variables, it is just overall and potential a bit related


##  trying PCA

pc <- prcomp(train1[,-2], center = T, scale. = T)

plot(pc)

attributes(pc)

pred <- predict(pc, train1)
new <- data.frame(pred, train1[6])

### trying prediction with multinominial logisitic regresson

library(nnet)


model <- multinom(class ~ PC1+PC2, data = new)

library(caret)

p <- predict(model, new)
tab <- table(p, new$class)

### brilliant.... just using two components we captured the results perfectly

