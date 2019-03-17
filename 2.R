################ Generalized linear models - where we can see that R squared is not equal to 1 and ofcourse the residuals. It is just not a Linear regression model, below is a logistic regression model. 
##CLASSIFICATION - ALL NUMERICALS NO CATEGORIES.
#################################### loading the dataset
library(glmx)
library(tidyverse)
data("BeetleMortality")
as_tibble(BeetleMortality) #remember that tibble shows you what kind of a variable we have in the dataset

#####################################################

BeetleMortality$prop_died <- BeetleMortality$died/BeetleMortality$n
ggplot(BeetleMortality,aes(dose,prop_died)) + geom_point() #ggplot clearly shows that there is no linear relationship - hence a GLM (logistic) problem

#################### 

### analysis using titanic data

install.packages("titanic")
library(titanic)
as_tibble(Titanic)

titanic.glm <- glm(Survived ~ Age + Sex, family = binomial(), titanic_train) #glm because it is a classification problem and family = binomial() because we have male and female
summary(titanic.glm)

#So what does our model tell us? The coefficient for Sexmale is negative (-2.465920) and highly significant (Pr(>|z|) < 2e-16 ***), meaning that men are significantly less likely to survive than women.
#However, the coefficient for Age (-0.005426) is not significantly different from zero (Pr(>|z|) = 0.39) so we can’t say the "children" part of "women and children first". Interesting!

#An example - lets see if class has an affect

titanic.glm1 <- glm(Survived ~ Pclass, family = poisson(),  titanic_train) #poisson was used since class has three types
summary(titanic.glm1) #from the analysis it is clear that lower the class the opportunity to survive was less. 


##LDA (linear discriminate analysis) has a similar syntax... starting with lda(....) but it is used when response variable has 2 or more levels (categories in text - example: alive, dead, alive, dead etc.).
