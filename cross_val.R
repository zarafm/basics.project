#Cross validation using k-fold. 5 parts, 4 parts are used to train and one part is used to test and this carried out for all parts. Refer notes for detailed explanation

## checking in mpg dataset, which variables give least mean square value... is it hwy ~ displ or hwy ~ displ + drv 
##CV is used to find out which model is the best - a less complex but yields valuable information


library(tidyverse)
data(mpg)

library(modelr) #modelr package is used as a modelling function that work with the pipe. A bit different from tidyverse but both share some common packages. modelr does not have ggplot
cv  <- crossv_kfold(mpg, k = 5) #5 for 5 fold k-cross validation
cv

models1  <- map(cv$train, ~lm(hwy ~ displ, data = .))
models2  <- map(cv$train, ~lm(hwy ~ displ + drv, data = .)) #training two separate models

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}
pred1  <- map2_df(models1, cv$test, get_pred, .id = "Run")
pred2  <- map2_df(models2, cv$test, get_pred, .id = "Run") #making a function of two different models 

### now calculating the Mean Square Error (MSE):

MSE1  <- pred1 %>% group_by(Run) %>% 
  summarise(MSE = mean( (hwy - pred)^2))
MSE1

MSE2  <- pred2 %>% group_by(Run) %>% 
  summarise(MSE = mean( (hwy - pred)^2))
MSE2

mean(MSE1$MSE)
mean(MSE2$MSE)

## MSE gives us the the error, from the above the MSE is higher for the first model, therefore, 2nd model is better.n

