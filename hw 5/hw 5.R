install.packages("performance")
library(performance)
install.packages("see")
library(see)
install.packages("faraway")
library(faraway)
install.packages("caret")
library(caret)
install.packages("AICcmodavg")
library(AICcmodavg)
library(here)
polarbear <- read.csv("./hw 5/PB_data.csv", header=T)


polarbear$age <- as.factor(polarbear$age)


head(polarbear) 

survmod1 <- glm(survival ~ ice + age, data = polarbear, family = "binomial")
summary(survmod1)

exp(survmod1$coefficients["ice"])
exp(survmod1$coefficients["age2"])

plot(binned_residuals(survmod1))

halfnorm(cooks <- cooks.distance(survmod1))

#this line makes predictions from our survival model to our age and ice #data
prediction<-predict(survmod1, newdata=data.frame(age = polarbear$age, ice = polarbear$ice), type="response")

#we will say the model predicts survival if the predicted outcome is #>0.5 and predicts a death otherwise
binary_outcome <- ifelse(prediction > 0.5, 1, 0)

#this line creates a confusion matrix. “Reference” refers to our 
#original data, and “Prediction” refers to our model predictions. We 
#really want to see all of our values in the diagonal, such that all 0s 
#and 1s in our dataset were correctly classified by the model
confusionMatrix(as.factor(binary_outcome), as.factor(polarbear$survival))

#model with intercept only
survmod1 <- glm(survival ~ 1 , data = polarbear, family = "binomial")
#model with age only
survmod2 <- glm(survival ~ age, data = polarbear, family = "binomial")
#model with ice-free days only
survmod3 <- glm(survival ~ ice , data = polarbear, family = "binomial")
#model with age and ice-free days
survmod4 <- glm(survival ~ age + ice , data = polarbear, family = "binomial")

#define list of models
models <- list(survmod1, survmod2, survmod3, survmod4)

#specify model names
mod.names <- c("intercept.only", "age", "ice", "age.ice")

#calculate AICc of each model
aictab(cand.set = models, modnames = mod.names, second.ord = T)

#First, because we are interested in survival of polar bears across all #possible values of number of ice-free days, let’s calculate a series #of 100 values from the minimum to maximum number of ice-free days in #our data
ice_values <- data.frame(ice = seq(min(polarbear$ice), max(polarbear$ice), length.out=100))

#then let’s make predictions on the logit scale (type="link”) 
predictions <- predict(survmod3, newdata=ice_values, type="link", se.fit=TRUE)

#now we can use the 1/1+exp(-PREDICTIONS) equation to convert these #values back to the 0-1 scale. 
predictions_real <- 1/(1+exp(-(predictions$fit)))

#then we can make a really basic plot of those predictions:
plot(x=ice_values$ice, y=predictions_real)

