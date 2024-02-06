#homework 2
library(here)
library(tidyverse)
library(faraway)
library(MASS)

income<- read.csv('./hw 2/income.csv', header = T)
gala <- read.csv('./hw 2/gala.csv', header = T)

model <- lm(income ~ years, data = income)
summary(model)
plot(model)

model.test <- lm(sqrt(abs(model$residuals)) ~ model$fitted.values)
summary(model.test)

#run weighted least squares, weights are inversely proportional to the variance, large variance, small weight
weights <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
model2 <- lm(income ~ years, weights = weights, data = income)
summary(model2)

#check for constant variance, need to use standardized residuals, plot residuals in terms of std dev 
plot(fitted(model2), rstandard(model2))
abline(h=0)

model2.test <- lm(sqrt(abs(model2$residuals)) ~ model2$fitted.values)
summary(model2.test)

#gala dataset
model <- lm(Species ~ Area + Nearest + Scruz + Adjacent, data=gala)
summary(model)
plot(model)

#box cox transformation to meet assumption of normally distributed errors, transform y variable to (y^lambda-1) / lambda
boxcox <- boxcox(model,    
                 lambda = seq(-1, 1, by=.1), 
                 plotit = TRUE,  
                 xlab = expression(lambda), 
                 ylab = "log-Likelihood")
#the below line tells you what value of x is associated with the maximum value of y on that curve
lambda <- boxcox$x[which.max(boxcox$y)]

# transform our y-variable using this new lambda using 
gala$Y_TRANSFORM <- (gala$Species ^ lambda - 1) / lambda

# re-run our model using this new y-variable and tell me what those coefficients are:
model_new <- lm(Y_TRANSFORM ~ Area + Nearest + Scruz + Adjacent, data=gala)
summary(model_new)
plot(model_new)
plot(model)
