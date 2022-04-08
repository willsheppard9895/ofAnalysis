library(tidyverse)

# read data
ofData <- read_csv("../testData/ofData.csv")

# set participant id, condition, contrast and offset to factors
ofData$Participant.Private.ID <- as.factor(ofData$Participant.Private.ID)
ofData$condition <- as.factor(ofData$condition)
ofData$contrast <- as.factor(ofData$contrast)
ofData$offset <- as.factor(ofData$offset)

# plot a distribution of abs error by condition
ggplot(ofData, aes(x = absAngError, colour = condition))+
  geom_density()+
  facet_wrap(offset~contrast)+
  scale_x_log10()

# plot distribution of RT by condition
ggplot(ofData, aes(x = Reaction.Time, colour = condition))+
  geom_density()+
  facet_wrap(offset~contrast)+
  scale_x_log10()
  
  
ggplot(ofData, aes(x = contrast, y = absAngError, color = condition))+
  geom_boxplot()
filtData <- ofData %>%
  group_by(condition, contrast)%>%
  filter(absAngError < 1.96*sd(absAngError)+mean(absAngError))
ggplot(filtData, aes(x = contrast, y = absAngError, color = condition))+
  geom_boxplot()
# look at interction
  
ggplot(ofData, aes(x = contrast, y = absAngError, color = condition, group = condition))+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  scale_x_discrete(labels = c("Low", "Medium", "High"),
                   name = "Contrast") +
  scale_y_continuous(name = "Absolute Error (degrees)")
  #facet_grid(~offset)
ggplot(filtData, aes(x = contrast, y = absAngError, color = condition, group = condition))+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  scale_x_discrete(labels = c("Low", "Medium", "High"),
                   name = "Contrast") +
  scale_y_continuous(name = "Absolute Error (degrees)")
#facet_grid(~offset)

ggplot(ofData, aes(x = offset, y = absError, color = condition, group = condition))+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  facet_grid(~contrast)

#ggplot(ofData, aes(x = contrast, y = absError, color = condition, group = condition))+
#  stat_summary(fun.y = mean, geom = "point") +
#  stat_summary(fun.y = mean, geom = "line")+
#  facet_wrap(~offset)

ggplot(ofData, aes(x = contrast, y = Reaction.Time, color = condition, group = condition))+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

ggplot(ofData, aes(x = offset , y = absAngError, fill = condition))+
  geom_bar(stat="identity", position=position_dodge())+
  #geom_errorbar()+
  facet_grid(~contrast)
