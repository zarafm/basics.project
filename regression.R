data <- read.csv(file.choose(), header = T)

###### now lets do some logistic regression and see which gives better prediction

### just using GRE variable 
## spilitting data into test and train


data1 <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[data1 == 1,]
test <- data[data1 == 2,]

train$admit <- as.factor(train$admit)


###### 



a <- glm(admit ~ gre, family = binomial, train)
b <- glm(admit ~ gre + gpa, family = binomial, train)

c <- 
  
  summary(a)


predict <- predict(b, train, type = 'response') # so this is important for logistic regression for conclusion stuff 

pre <- ifelse(predict > 0.5, 1, 0)
table(predicted = pre, actual =  train$admit)

### it is clear that from increasing the variables i.e. by adding gpa the model is performing better


############################################# using multiple linear regression 


d <- lm(gpa ~ gre, train)

predict(d, data.frame(gre = 500), interaval = 'confidence') ### useful for prediction when using general lm model
