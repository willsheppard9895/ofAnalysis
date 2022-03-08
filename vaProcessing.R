library(tidyverse)
library(car)

# import data
allData <- read_csv("../testData/va.csv")

# create fail column
allData <- allData %>%
  mutate(blurFail = case_when(
    blurPercCorrect <= 0.5 ~ 1,
    TRUE ~ 0
  ))%>%
  mutate(clearFail = case_when(
    clearPercCorrect <= 0.5 ~ 1,
    TRUE ~ 0
  ))

# create a failure data frame
blurData <- allData %>%
  filter(blurFail == 1)%>%
  select(-clearFail, - clearPercCorrect)
clearData <- allData %>%
  filter(clearFail == 1)%>%
  select(-blurFail, -blurPercCorrect)

# indicate which column each particapnt fails on
blurThresholds <- blurData %>%
  group_by(Participant.Private.ID) %>%
  summarise(blurThreshold = first(logMAR))

clearThresholds <- clearData %>%
  group_by(Participant.Private.ID) %>%
  summarise(clearThreshold = first(logMAR))

# merge data frames
thresholds <- merge(blurThresholds, clearThresholds, by = c("Participant.Private.ID"), all = TRUE)

# replace na's with -.2logMAR
thresholds <- thresholds %>%
  replace_na(list(blurThreshold = -0.2, clearThreshold = -0.2))

# check the mean is around 0
mean(thresholds$blurThreshold)
mean(thresholds$clearThreshold)

# wahey, they are significantly different
t.test(thresholds$blurThreshold, thresholds$clearThreshold, paired = TRUE)

# write to csv
write.csv(thresholds, "../testData/vaThresh.csv")
