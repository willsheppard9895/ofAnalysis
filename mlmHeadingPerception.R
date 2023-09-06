library(plyr)
library(car)
library(tidyverse)
library(nlme)
library(reshape)

# read in data
ofData <- read_csv("../cleanData/ofData.csv")

# select needed columns
ofData <- ofData %>%
  select(Participant.Private.ID, condition, contrast, absAngError)%>%
  dplyr::rename(conditionText = condition)%>%
  mutate(condition = case_when(
    conditionText == "clear" ~ 0,
    TRUE ~ 1
  ))%>%
  mutate(contrastHigh = case_when(
    contrast == 'High'~ 1,
    TRUE ~ 0
  ), contrastMed = case_when(
    contrast == 'Medium' ~ 1,
    TRUE ~ 0
  ), contrastLow = case_when(
    contrast == 'Low' ~ 1,
    TRUE ~ 0
  )
  )

# intercept model
interceptOnly <- gls(absAngError ~ 1, 
                     data = ofData,
                     method = 'ML')
summary(interceptOnly)

# random intercept only
# angular error ij = b0j + eij
#               b0j = b0 +u0j
randomInterceptOnly <- lme(absAngError ~ 1, 
                           data = ofData,
                           random = ~1|Participant.Private.ID,
                           method = 'ML')
summary(randomInterceptOnly)

#anova - is there a differencein the fit?
# significantlybetter fit when itercept is allowed to vary across participants
anova(interceptOnly, randomInterceptOnly)


# randomInterceptCondition
# significant improvement in fit

# angular error ij = b0j + b1Condition ij + eij
#               b0j = b0 +u0j

randomInterceptCondition<- lme(absAngError ~ condition, 
                               data = ofData,
                               random = ~1|Participant.Private.ID,
                               method = 'ML')
summary(randomInterceptCondition)
anova(randomInterceptOnly, randomInterceptCondition)

# random intercept - condition, contrast

# angular error ij = b0j + b1Condition ij + b2contrastMed ij + b3contrastLow ij + eij
#               b0j = b0 +u0j
randomInterceptConditionContrast <- lme(absAngError ~ condition + contrastMed + contrastLow, 
                                        data = ofData,
                                        random = ~1|Participant.Private.ID,
                                        method = 'ML')
summary(randomInterceptConditionContrast)

# random intercept for condition, and contrasts allows for more explained variance
anova(randomInterceptOnly, randomInterceptCondition, randomInterceptConditionContrast)

# add random slope for condition
# significantly better fit

# angular error ij = b0j + b1Condition ij + b2contrastMed ij + b3contrastLow ij + eij
#               b0j = b0 +u0j
randomSlopeCondition <- lme(absAngError ~ condition + contrastMed + contrastLow, 
                            data = ofData,
                            random = ~condition|Participant.Private.ID,
                            method = 'ML')
summary(randomSlopeCondition)
anova(randomInterceptConditionContrast, randomSlopeCondition)

# add random slope for contrast
randomSlopeConditionContrast <- lme(absAngError ~ condition + contrastMed + contrastLow, 
                            data = ofData,
                            random = ~condition + contrastMed + contrastLow|Participant.Private.ID,
                            method = 'ML',
                            control =list(msMaxIter = 1000, msMaxEval = 1000))
summary(randomSlopeConditionContrast)
anova(randomSlopeCondition, randomSlopeConditionContrast)
intervals(randomSlopeConditionContrast, which = "fixed")

# add an interaction
# significant improvement in fit

# angular error ij = b0j + b1j Condition ij + b2contrastMed ij + b3contrastLow ij 
#                     + b4(Condition*contrastMed) + b5(condition*contrastLow)
#                      + eij

#               b0j = b0 +u0j
#               b1j = b1 + u1j

randomSlopeConditionInteraction <- lme(absAngError ~ condition + contrastMed + contrastLow + condition:contrastMed + condition:contrastLow, 
       data = ofData,
       random = ~condition + contrastMed + contrastLow|Participant.Private.ID,
       method = 'ML',
       control =list(msMaxIter = 1000, msMaxEval = 1000))
summary(randomSlopeConditionInteraction)
modelComp <- intervals(randomSlopeConditionInteraction, which = 'fixed')
anova(randomSlopeConditionContrast, randomSlopeConditionInteraction)

# condition no lobger predicts angular error (b = -0.06, t = -0.20, p = .840)
# medium contrast predicts worse performance (b = 0.91, t = 4.20, p < .001)
# low contrast predicts worse performance (b = 6.85, t = 13.16, p < .001)
# no sig interaction between condition and medium contrast (b = 0.20, t = 0.67, p = .505)
# sig interaction between confition and low contrast (b = 2.39, t = 7.94, p < .001) CI = 1.80-2.97


# zero inflated model -  a model based ona  distribution that allows for frequent 0 values

#conditional r squared = fixed and random effects
# marginal r squared = only fixed effects
library(performance)
r2_nakagawa(randomSlopeCondition)
r2_nakagawa(randomSlopeConditionContrast)
r2_nakagawa(randomSlopeConditionInteraction)

# graphs - look at error bars
# data skewed  gamma or inverse gausian

#fit linear model, gamma and inverse gausian with AIC - lower is better (less information is lost)
library(lme4)


# if using a log link function - exp() the b value to give a percetage change associated with this condition
rsciGamma <- glmer(absAngError ~ condition + contrastMed + contrastLow + condition:contrastMed + condition:contrastLow +(condition + contrastMed + contrastLow|Participant.Private.ID),
                   data = ofData%>%filter(absAngError >0),
                   family = Gamma(link = 'log')
                   )

rsciGausian <- glmer(absAngError ~ condition + contrastMed + contrastLow + condition:contrastMed + condition:contrastLow +(condition + contrastMed + contrastLow|Participant.Private.ID),
                     data = ofData%>%filter(absAngError >0),
                   family = inverse.gaussian(link = 'log'))

summary(rsciGamma)
summary(rsciGausian)

anova(rsciGamma, rsciGausian)

## let try and plot this model
modelComp <- unlist(modelComp)
ints <- as.list(modelComp)
intsDF <- data.frame(matrix(ncol = 3, nrow = length(ints)/3))
x = c('lower', 'x', 'upper')
colnames(intsDF) <- x

intsDF$lower <- ints[1:6]
intsDF$x <- ints[7:12]
intsDF$upper <- ints[13:18]

y <- c('Intercept', 'condition', 'contrastMed', 'contrastLow', 'condition:contrastMed', 'condition:contrastLow')
rownames(intsDF) <- y

#error <- 4.407755 + 0.913082contrastMid + 6.853938contrastLow + 2.385121condition*contrastLow
cnds <- 