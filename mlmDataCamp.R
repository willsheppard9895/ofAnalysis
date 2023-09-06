library(tidyverse)
library(stats)
library(lme4)

# read in data
ofData <- read_csv("../cleanData/ofData.csv")

#  build a regression model
mod <- lm(absAngError ~ condition, data = ofData)
summary(mod)
mod
coef_plot <- tidy(mod)


ggplot(mod, aes(x = condition, y = absAngError))+
  geom_point(alpha = .01)+
  theme_minimal()+
  geom_abline(intercept = mod$coefficients[1],
              slope = mod$coefficients[2])

# buidl a mixed effecs model
lmer1 <- lmer(absAngError ~ condition * contrast + (1|Participant.Private.ID), data = ofData)
summary(lmer1)
tidy.table(lmer1)
