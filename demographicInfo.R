library(tidyverse)

# import data
cleanData <- read.csv("../cleanData/taskData.csv")
demoAll <- read.csv("../studentData/studentDemographicsWideEdit.csv")

# create list of participants to include
pps <- unique(cleanData$Participant.Private.ID)

# check length - should be 52
# is 52
length(pps)

# filter cleanData by participants in pps
demo <- demoAll %>%
  filter(Participant.Private.ID %in% pps)

## create summary measures
demoSummary <- demo %>%
  dplyr::summarise(minAge = min(age_in_years),
                   maxAge = max(age_in_years),
                   meanAge = mean(age_in_years),
                   sdAge = sd(age_in_years),
                   percFemale = (2-mean(sex.quantised))*100,
                   education = which.max(education),
                   percRight = 100 - ((2-mean(Handedness.quantised))*100)
                   )
# most common educaitona level
edu <- demo %>%
  group_by(education)%>%
  count()

edu$perc <- (edu$n/length(pps))*100

# do participants click on the horizon
mean(cleanData$Y.Coordinate, na.rm = TRUE)
sd(cleanData$Y.Coordinate, na.rm = TRUE)
