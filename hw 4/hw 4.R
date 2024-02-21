library(here)
library(lme4)
install.packages("AICcmodavg")
library(AICcmodavg)

caterpillar <- read.csv("./hw 4/caterpillar.csv", header=T)

mod4 <- lm(Growth ~ Exposure + Temperature, data=caterpillar)
summary(mod4)
mod1 <- lm(Growth ~ 1, data=caterpillar)
#model with growth as a function of herbicide exposure
mod2 <- lm(Growth ~ Exposure, data=caterpillar)
#model with growth as a function of temperature
mod3 <- lm(Growth ~ Temperature, data=caterpillar)

#define list of models
models <- list(mod1, mod2, mod3, mod4)

#specify model names
mod.names <- c("intercept.only", "exposure", "temperature", "exposure.temperature")

#calculate AIC of each model
#a note that second.ord = T will calculate AICc, which comes next
aictab(cand.set = models, modnames = mod.names, second.ord = F)

aictab(cand.set = models, modnames = mod.names, second.ord = T)

bictab(cand.set = models, modnames = mod.names)

bats <- read.csv("./hw 4/bats.csv", header=T)
#model with three random intercepts
mod1 <- lmer(bat.temp ~ ambient + (1|day) + (1|colony) + (1|colony:ind.col), data=bats, REML=TRUE)
summary(mod1)

mod2 <- lmer(bat.temp ~  (1|day) + (1|colony) + (1|colony:ind.col), data=bats, REML=F)


#model with ambient temp as a fixed effect
mod1 <- lmer(bat.temp ~ ambient + (1|day) + (1|colony) + (1|colony:ind.col), data=bats, REML=FALSE)

#model without ambient temp as a fixed effect
#ignore the warning here; it is telling us we may be building a model that is too complex, as the random effects are really small
mod2 <- lmer(bat.temp ~ 1 + (1|day) + (1|colony) + (1|colony:ind.col), data=bats, REML=FALSE)

summary(mod2)
#create list of models
models <- list(mod1, mod2)
#specify model names
mod.names <- c("with.ambient", "without.ambient")
#create AICc table
aictab(cand.set = models, modnames = mod.names, second.ord=T)
