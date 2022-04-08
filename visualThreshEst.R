library(tidyverse)

# import data
va <- read.csv("../studentData/studentVA.csv")
cs <- read.csv("../studentData/studentCS.csv")

# calculate VA threshold using letterwise scoring
vaThresh <- va %>%
  dplyr::group_by(Participant.Private.ID)%>%
  dplyr::summarise(blurThresh = 1.1 - 0.1*sum(blurPercCorrect, na.rm = TRUE),
                   clearThresh = 1.1 - 0.1*sum(clearPercCorrect, na.rm = TRUE)
                   
  )

# pivot longer
vaThreshLong <- vaThresh %>%
  pivot_longer(!Participant.Private.ID, names_to = "condition", values_to = "threshold")



##### CS #####
csLong <- cs %>%
  pivot_longer(cols = ends_with("PercCorrect"), 
               names_to = "condition", 
               values_to = "percCorrect",
               values_drop_na = TRUE)
csLong <- csLong %>%
  mutate(condition = case_when(
    condition == "blurPercCorrect" ~ "blur",
    condition == "clearPercCorrect" ~ "clear"
  ))

# Create function to identify first visual condition is less than 50%
whichCS <- function(Perc.Correct, Contrast.Sensitivity){
  
  # pick first condition where Perc.Correct < .5
  Low.Perc <- which(Perc.Correct < .5)
  Low.Perc.Index <- first(Low.Perc)
  Low.Perc.CS <- Contrast.Sensitivity[Low.Perc.Index - 1]
}

# Create df containing particiapnt CS
csThresh <- csLong %>%
  group_by(Participant.Private.ID, condition) %>%
  dplyr::summarise(contrastThresh = whichCS(percCorrect, Contrast))

csThresh[is.na(csThresh)]<- 1


csTtest <- csThresh %>%
  pivot_wider(names_from = condition, values_from = contrastThresh)
t.test(csTtest$blur, csTtest$clear)

ggplot(csThresh, aes(x = condition, y = contrastThresh))+
  geom_boxplot()+
  #ylim(0, 20)+
  scale_y_log10()

vaThreshLong <- vaThreshLong %>%
  mutate(condition = case_when(
    condition == "blurThresh" ~ "blur",
    condition == "clearThresh" ~ "clear"
  ))

# join threshold dataframes and change column names
thresholds <- merge(csThresh, vaThreshLong, 
                    by = c("Participant.Private.ID", "condition"))

thresholds <- thresholds %>%
  rename(acuityThresh = threshold)

# read in OF data
of <- read.csv("../studentData/studentOF.csv")

# get a blur and clear score for each participant
ofSummary <- of %>%
  group_by(Participant.Private.ID, condition)%>%
  dplyr::summarise(error = median(absError))

# join to dataframe
thresholds <- merge(thresholds, ofSummary, 
                 by = c("Participant.Private.ID", "condition"))

# plot
ggplot(thresholds, aes(x = condition, y = acuityThresh))+
  geom_boxplot()+
  scale_y_continuous(name = "VA detection threshold (logMAR)")+
  scale_x_discrete(name = "Condition")+
  geom_hline(yintercept = 0.176, linetype="dashed", color = "red", size = 1)+
  geom_hline(yintercept = 0.3, linetype="dashed", color = "red", size = 1)

t.test(x = vaThresh$blurThresh, vaThresh$clearThresh)


ggplot(thresholds, aes(x = condition, y = contrastThresh))+
  geom_boxplot()+
  scale_y_log10(name = "Contrast detection threshold (%)")+
  scale_x_discrete(name = "Condition")

ggplot(thresholds, aes(x = contrastThresh, y = acuityThresh))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10(name = "Contrast detection threshold (% contrast)")+
  scale_y_continuous(name = "VA detection threshold (logMAR)")
cor.test(thresholds$contrastThresh, thresholds$acuityThresh)


ggplot(thresholds, aes(x = contrastThresh, y = error))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10(name = "Contrast detection threshold (% contrast)")+
  scale_y_continuous(name = "Absolute error (pixels)")
cor.test(thresholds$contrastThresh, thresholds$error)

ggplot(thresholds, aes(x = acuityThresh, y = error))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(name = "Absolute error (pixels)") +
  scale_x_continuous(name = "VA detection threshold (logMAR)")#+
  #scale_x_log10()
cor.test(thresholds$error, thresholds$acuityThresh)
