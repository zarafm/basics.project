##to avoid for loops we use nested dataframes. This is particularly useful for stratified datasets - with categorical variables 

gapminder.nested <- gapminder %>% 
  group_by(country, continent) %>% #group_by works in conjuction with other dplyr operators like filter, arrange, mutate and summarize
  nest() #group_by acts on the dataset group by group instead of acting on the entire dataset
gapminder.nested ## this will create a nested dataframe 

#################################################

country_model <- function(df){
  lm(lifeExp ~ year, data = df)
} ## this will create a function which carries out linear model (regression analysis) for all the countries

###############################################

gapminder.model <- gapminder.nested %>%
  mutate(model = map(data, country_model),
         coef = map(model, broom::tidy)) ## this will map the nested dataframe with the analysis and now we have analysis for all the countries.

###############################################

#test using

gapminder.model$coef[[1]] ###this is a way to test the function

##if you want to view data of a particular country then

gapminder.nested[1,]$data
