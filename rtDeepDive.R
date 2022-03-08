library(tidyverse)

# import data
allData <- read_csv("../testData/ofData.csv")
allData$Y.Coordinate




# viualise the reaction times across offsets and contrast
ggplot(allData, aes(y = Reaction.Time, group = condition, colour = condition))+
  geom_boxplot()+
  facet_grid(rows = vars(offset),
             cols = vars(contrast))

# what are the mean rt#s by participant
pps <- allData %>%
  group_by(Participant.Private.ID, condition) %>%
  summarise(rt = mean(Reaction.Time))
ggplot(pps, aes(x = rt, group = condition, color = condition))+
  geom_boxplot()
