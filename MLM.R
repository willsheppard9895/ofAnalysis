# Create MLMs for optic flow task
# 60ish participants
# Predictor: Blur vs. no Blur
# Predictor: 10%, 50%, 100% Contrast
# Outcome: abs Error

## i will start with these 2 levels as i am not particularly interested in the effect of offset

# import all data
# start with student OF data, this is uncleaned but will give me a sense of how to run these models
allData <- read.csv("../studentData/studentOF.csv")

# the eventualmodel will be something akin to
# absError = b0j + b1j Blur ij + b2j Contrast ij + b3j (Blur ij x Contrast ij) + e ij
# b0j = b0 + u0j
# b1j = b1 + u1j
# b2j = b2 + u2j
# b3j = b3 + u3j
# however, contrast will need to be dummy coded
# 100 v 10
# 100 v 50

# make participant id and contrast factors
allData$Participant.Private.ID <- as.factor(allData$Participant.Private.ID)
allData$contrast <- as.factor(allData$contrast)

# check that this has worked
str(allData)

# specify contrast - 100% as base
levels(allData$contrast)
contrasts(allData$contrast) <- contr.treatment(3, base = 3)

levels(allData$condition)
contrasts(allData$condition) <- contr.treatment(2, base = 2)
# load nlme package
library(nlme)

### build models
# random intercept
blurBase <- nlme::lme(
  fixed = absError ~ 1,
  random = ~1|Participant.Private.ID,
  data = allData, na.action=na.omit
)

# add blur condition as a fixed effect
blurCond <- nlme::lme(
  fixed = absError ~ condition,
  random = ~1|Participant.Private.ID,
  data = allData, na.action=na.omit
)

# add contrast as a fixed effect
blurCont <- nlme::lme(
  fixed = absError ~ condition + contrast,
  random = ~1|Participant.Private.ID,
  data = allData, na.action=na.omit
)

# add an interaction effect
blurInt <- nlme::lme(
  fixed = absError ~ condition + contrast + condition:contrast,
  random = ~1|Participant.Private.ID,
  data = allData, na.action=na.omit
)

# realistically, i think that this will be the model that i ill propose
# seemstheory driven i.e. ever
blurCondRand <- nlme::lme(
  fixed = absError ~ condition + contrast + condition:contrast,
  random = ~condition|Participant.Private.ID,
  data = allData, na.action=na.omit
)

ctrl <- lmeControl(opt='optim')
blurContRand <- nlme::lme(
  fixed = absError ~ condition + contrast + condition:contrast,
  random = ~condition + contrast|Participant.Private.ID,
  control = ctrl,
  data = allData, na.action=na.omit
)

blurIntRand <- nlme::lme(
  fixed = absError ~ condition + contrast + condition:contrast,
  random = ~condition + contrast + condition:contrast|Participant.Private.ID,
  control = ctrl,
  data = allData, na.action=na.omit
)
summary(blurIntRand)
### compare the models
anova(blurBase, blurCond, blurCont, blurInt)
summary(blurInt)
anova(blurBase, blurCond, blurCont, blurInt, blurCondRand, blurContRand, blurIntRand)

# look at contrasts
library(broom.mixed)
test <- broom.mixed::tidy(blurInt)
broom <- broom.mixed::tidy(blurIntRand)

