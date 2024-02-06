library(tidybayes)
library(ggplot2)
library(faraway)
library(here)

turtle <- read.csv('./week 1/turtle.csv', header = T)

dim(turtle)
mean(turtle$yucca)

model<- lm(obt.hr ~ plastron, turtle)
summary(model)

ggplot(turtle, aes(x = plastron, y = obt.hr))+
         geom_point()+
          stat_smooth(method = "lm")

  ggplot(model, aes(y = model$residuals, x = model$fitted.values))+
  geom_point() 

plot(model$fitted.values,model$residuals, abline(h=0))

model.test <- lm(sqrt(model$residuals)~model$fitted.values)
summary(model.test)

qqnorm(residuals(model), main = "")
qqline(residuals(model))

acf(model$residuals)

cook <- cooks.distance(model)
halfnorm(cook, ylab = "Cook's distance")
