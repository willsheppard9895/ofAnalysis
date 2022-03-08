library(tidyverse)

# load OF and vision data
ofAll <- read_csv("../testData/ofDataSummary.csv")
vaAll <- read_csv("../testData/vaThresh.csv")
csAll <- read_csv("../testData/csThresh.csv")

# create means by participant for blur and clear conditions
ofAll <- ofAll %>%
  group_by(Participant.Private.ID)%>%
  summarise(blurError = mean(meanError_blur),
            clearError = mean(meanError_clear))

# pivot data longer to compare vision and heading perception
# of data
ofAll <- ofAll %>%
  pivot_longer(!Participant.Private.ID, names_to = "condition", values_to = "error")

ofAll <- ofAll %>%
  mutate(condition = case_when(
    condition == "blurError" ~ "blur",
    condition == "clearError" ~ "clear"
  ))

#pps <- unique(ofAll$Participant.Private.ID)

# va data
vaThresh <- vaAll %>%
  select(-X1) %>%
  #filter(Participant.Private.ID %in% pps) %>%
  pivot_longer(!Participant.Private.ID, names_to = "condition", values_to = "vaThresh")

vaThresh <- vaThresh %>%
  mutate(condition = case_when(
    condition == "blurThreshold" ~ "blur",
    condition == "clearThreshold" ~ "clear"
  ))
# cs data
csThresh <- csAll %>%
  select(-X1) %>%
  #filter(Participant.Private.ID %in% pps) %>%
  pivot_longer(!Participant.Private.ID, names_to = "condition", values_to = "csThresh")
csThresh <- csThresh%>%
  mutate(condition = case_when(
    condition == "blurThreshold" ~ "blur",
    condition == "clearThreshold" ~ "clear"
  ))

## merge data frames
vaOfData <- merge(ofAll, vaThresh, by = c("Participant.Private.ID", "condition"))
csOfData <- merge(ofAll, csThresh, by = c("Participant.Private.ID", "condition"))
csVaData <- merge(vaThresh, csThresh, by = c("Participant.Private.ID", "condition"))

cor.test(vaOfData$error, vaOfData$vaThresh)
ggplot(vaOfData, aes(x = vaThresh, y = error))+
  geom_point()

cor.test(csOfData$error, csOfData$csThresh)
ggplot(csOfData, aes(x = csThresh, y = error))+
  geom_point()

cor.test(csVaData$vaThresh, csVaData$csThresh)
ggplot(csVaData, aes(x = vaThresh, y = csThresh))+
  geom_point()+
  geom_smooth(method = "lm")
