library(tidyverse)

# import data
allData <- read_csv("../cleanData/taskData.csv")

# create variable with Zone.Name s of inerest
zones <- c("response_button_text", "response_text_entry", "click_painting")

# filter by zones
data <- allData %>%
  filter(Zone.Type %in% zones)
  

# import data
va <- data %>%
  filter(display == "va")

va <- va %>%
  dplyr::select(Participant.Private.ID, condition, Screen.Name, Attempt, Correct)

#revalue to logMAR
# level 1 = 1 logMAR
# level 13 = -.2 logMAR
va$logMAR <- 1.1 - (as.numeric(str_remove(va$Screen.Name, "Level"))/10)
va <- va %>%
  dplyr::select(-Screen.Name)

## calculate perc correct for each level and condition
va <- va %>%
  group_by(Participant.Private.ID, condition, logMAR)%>%
  summarise(totAttempt = sum(Attempt),
         totCorrect = sum(Correct))%>%
  mutate(percCorrect = totCorrect/totAttempt)%>%
  arrange(Participant.Private.ID, condition, -logMAR)

#write_csv(va, "../cleanData/va.csv")

# calculate VA threshold using letterwise scoring
vaThresh <- va %>%
  dplyr::group_by(Participant.Private.ID, condition)%>%
  dplyr::summarise(threshold = 1.1 - 0.1*sum(percCorrect, na.rm = TRUE))
                   
#write_csv(vaThresh, "../cleanData/vaThresh.csv")



##### CS #####
cs <- data %>%
  filter(display == "cs")

cs <- cs %>%
  dplyr::select(Participant.Private.ID, condition, Screen.Name, Attempt, Correct)

# revalue levels to % contrast
# level 1 = 100% contrast
cs$contrast <- 101 - as.numeric(str_remove(cs$Screen.Name, "Level"))

cs <- cs %>%
  dplyr::select(-Screen.Name)

# calculate perc correct for each level
cs <- cs %>%
  group_by(Participant.Private.ID, condition, contrast)%>%
  summarise(totAttempt = sum(Attempt),
            totCorrect = sum(Correct))%>%
  mutate(percCorrect = totCorrect/totAttempt)%>%
  arrange(Participant.Private.ID, condition, -contrast)

#write_csv(cs, "../cleanData/cs.csv")

# select lowest value 0.5 or over
# filter all values under 0.5
csGood <- cs %>%
  filter(percCorrect >= 0.5)

# select lowest value
csThresh <- csGood %>%
  group_by(Participant.Private.ID, condition) %>%
  summarise(threshold = min(contrast))

#write_csv(csThresh, "../cleanData/csThresh.csv")


