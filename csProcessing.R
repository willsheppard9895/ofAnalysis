library(tidyverse)
library(car)

# import functions
source('functions_visionProcessing_and_threshEstimation.R')

# import data
allData <- read_csv("../cleanData/cs.csv")

# rename columns to match the function
allData <- allData %>%
  rename(id = Participant.Private.ID)

#separate blur and clear data
blurData <- allData %>%
  filter(condition == 'blur')
clearData <- allData %>%
  filter(condition == 'clear')


# calculate cs Thresholds
csBlur <- precThreshCS(blurData, colName = 'csBlur')
csClear <- precThreshCS(clearData, colName = 'csClear')

# merge dataframes
cs <- full_join(csBlur, csClear, by = 'id')


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
  filter(blurFail == 0)%>%
  select(-clearFail, - clearPercCorrect)
clearData <- allData %>%
  filter(clearFail == 0)%>%
  select(-blurFail, -blurPercCorrect)

# remove rows with NAs
blurData <- blurData[complete.cases(blurData),]
clearData <- clearData[complete.cases(clearData),]

# indicate which column each particapnt fails on
blurThresholds <- blurData %>%
  group_by(Participant.Private.ID) %>%
  summarise(blurThreshold = min(Contrast))

clearThresholds <- clearData %>%
  group_by(Participant.Private.ID) %>%
  summarise(clearThreshold = min(Contrast))

# merge data frames
thresholds <- merge(blurThresholds, clearThresholds, by = c("Participant.Private.ID"), all = TRUE)

# replace na's with -.2logMAR
thresholds <- thresholds %>%
  replace_na(list(blurThreshold = 1, clearThreshold = 1))

thresholds <- thresholds %>%
  filter(blurThreshold < 50 ) %>%
  filter(clearThreshold < 50 )

# check the mean is around 0
mean(thresholds$blurThreshold)
sd(thresholds$blurThreshold)
mean(thresholds$clearThreshold)
sd(thresholds$clearThreshold)

t.test(thresholds$blurThreshold, thresholds$clearThreshold, paired = TRUE, alternative = "g")

# write to csv
write.csv(thresholds, "../testData/csThresh.csv")

