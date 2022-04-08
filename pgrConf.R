library(tidyverse)
library(rstatix)

# import data
allData <- read_csv("../testData/ofData.csv")
allVA <- read_csv("../testData/va.csv")
allCS <- read_csv("../testData/cs.csv")

#read target coordinates
coordinates <- read_csv("../opticFlowOffsets.csv")

# create absolute offset variable
allData <- allData %>%
  mutate(absOffset = abs(offset))

# Participants 5384850 has been removed due to issues with coding
## Check participants with medical condition vs general trend
# 5508881 - cataracts
# 5532853 - Deuteranopia (color blind)
# 5540429 - had cataract, still has film over left eye
# 5658787 - Diabetic Retinopathy
# leaves N of 55
visProbs <- c(5508881, 5532853, 5540429, 5658787, 5384850)
# create data frame where participants with visual issues are marked
probData <- allData %>%
  mutate(prob = case_when(
    Participant.Private.ID %in% visProbs ~ 1,
    TRUE ~ 0
  ))
# look at descriptive stats for the groups
probData %>%
  group_by(prob) %>%
  get_summary_stats(absAngError, type = "mean_sd")

# calculate t score
stat.test <- probData %>%
  t_test(absAngError ~ prob) %>%
  add_significance()
stat.test

# participants with self reported visual problems had a sig. higher absAngError
# therefore remove
data <- subset(allData, !(Participant.Private.ID %in% visProbs))


## plot data on xy axis
ggplot(data = allData, aes(x = X.Coordinate, y = Y.Coordinate, 
                        color = absOffset, fill = absOffset))+
  geom_point(alpha = 0.3)+
  scale_x_continuous(limits = c(0, 640))+
  scale_y_continuous(limits = c(0, 480))+
  geom_hline(yintercept = 255, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept = 320, linetype="dashed", color = "red", size = 1)+
  #geom_vline(xintercept = coordinates$left[1], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[2], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[3], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[4], linetype="dashed", color = "black")+
  geom_vline(xintercept = coordinates$left[5], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[1], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[2], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[3], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[4], linetype="dashed", color = "black")+
  geom_vline(xintercept = coordinates$right[5], linetype="dashed", color = "black")+
  coord_equal()

# create extreme y variables
ymax <- mean(data$Y.Coordinate)+2*sd(data$Y.Coordinate)
ymin <- mean(data$Y.Coordinate)-2*sd(data$Y.Coordinate)

# remove participants with extreme y values
data <- data %>%
  filter(Y.Coordinate < ymax) %>%
  filter(Y.Coordinate > ymin)

## plot data on xy axis after first filter
ggplot(data = data, aes(x = X.Coordinate, y = Y.Coordinate, 
                           color = absOffset, fill = absOffset))+
  geom_point(alpha = 0.3)+
  scale_x_continuous(limits = c(0, 640))+
  scale_y_continuous(limits = c(0, 480))+
  geom_hline(yintercept = 255, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept = 320, linetype="dashed", color = "red", size = 1)+
  #geom_vline(xintercept = coordinates$left[1], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[2], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[3], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[4], linetype="dashed", color = "black")+
  geom_vline(xintercept = coordinates$left[5], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[1], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[2], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[3], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[4], linetype="dashed", color = "black")+
  geom_vline(xintercept = coordinates$right[5], linetype="dashed", color = "black")+
  coord_equal()

# remove rows who are more than 2sd above mean ang error for each participant, condititon and contrast
data <- data %>%
  group_by(condition, contrast, offset)%>%
  mutate(outlier = case_when(
    absAngError > mean(absAngError)+2*sd(absAngError)~1,
    TRUE ~ 0
  ))%>%
  filter(outlier != 1)%>%
  select(-outlier)%>%
  ungroup()



## plot data on xy axis after second filter
ggplot(data = data, aes(x = X.Coordinate, y = Y.Coordinate, 
                        color = absOffset, fill = absOffset))+
  geom_point(alpha = 0.3)+
  scale_x_continuous(limits = c(0, 640))+
  scale_y_continuous(limits = c(0, 480))+
  geom_hline(yintercept = 255, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept = 320, linetype="dashed", color = "red", size = 1)+
  #geom_vline(xintercept = coordinates$left[1], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[2], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[3], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$left[4], linetype="dashed", color = "black")+
  geom_vline(xintercept = coordinates$left[5], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[1], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[2], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[3], linetype="dashed", color = "black")+
  #geom_vline(xintercept = coordinates$right[4], linetype="dashed", color = "black")+
  geom_vline(xintercept = coordinates$right[5], linetype="dashed", color = "black")+
  coord_equal()

# make contrast a factor
data$contrast <- as.factor(data$contrast)


# plot the interaction
ggplot(data = data, aes(x = contrast, y = absAngError, color = condition, group = condition))+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  scale_x_discrete(labels = c("10" = "Low", "50" = "Medium", "100" = "High"),
                   name = "Contrast") +
  scale_y_continuous(name = "Absolute Error (degrees)",
                     breaks = c(4, 6, 8, 10, 12))

# MLM

# make variables factors
data$contrast <- as.factor(data$contrast)
data$condition <- as.factor(data$condition)

# Fixed effects: condition, contrast - we can make a priori predicitons about the direction of these effects, so are not random
# Random effects: Person - no idea what the persons 
# specify contrast - 100% as base
levels(data$contrast)
contrasts(data$contrast) <- contr.treatment(3, base = 3)

# specify clear as base
levels(data$condition)
contrasts(data$condition) <- contr.treatment(2, base = 2)


## build models
# random intercept
blurBase <- nlme::lme(
  fixed = absAngError ~ 1,
  random = ~1|Participant.Private.ID,
  data = data, na.action=na.omit
)

# add blur condition as a fixed effect
blurCond <- nlme::lme(
  fixed = absAngError ~ condition,
  random = ~1|Participant.Private.ID,
  data = data, na.action=na.omit
)

# add contrast as a fixed effect
blurCont <- nlme::lme(
  fixed = absAngError ~ condition + contrast,
  random = ~1|Participant.Private.ID,
  data = data, na.action=na.omit
)
# add an interaction effect
blurInt <- nlme::lme(
  fixed = absAngError ~ condition + contrast + condition:contrast,
  random = ~1|Participant.Private.ID,
  data = data, na.action=na.omit
)

# Compare models
anova(blurBase, blurCond, blurCont, blurInt)

# look at contrasts
library(broom.mixed)
intModelSummary <- broom.mixed::tidy(blurInt)


#### process va thresh data
va <- subset(allVA, !(Participant.Private.ID %in% visProbs))

# replace na's with 0s
va[is.na(va)] <- 0

# calculate threshold using method similar to letterwise scoring
threshVA <- va %>%
  group_by(Participant.Private.ID)%>%
  summarise(blurTotalCorrect = sum(blurPercCorrect),
            clearTotalCorrect = sum(clearPercCorrect))%>%
  summarise(Participant.Private.ID = Participant.Private.ID,
            blurVA = round(1.1 - blurTotalCorrect/10, digits = 2),
            clearVA = round(1.1 - clearTotalCorrect/10, digits = 2))

# pivot longer
threshVA <- threshVA %>%
  pivot_longer(!Participant.Private.ID, names_to = "condition", values_to = "threshold")%>%
  mutate(condition = case_when(
    condition == "blurVA" ~ "blur",
    TRUE ~ "clear"
  ))

vaPlot <- ggplot(threshVA, aes(x = condition, group = condition, y = threshold))+
  geom_boxplot()+
  geom_hline(yintercept = 0.176, linetype="dashed", color = "red", size = 1)+ 
  geom_text(aes(0.6, 0.176, label = "6/9", vjust = -1))+
  geom_hline(yintercept = 0.3, linetype="dashed", color = "red", size = 1)+ 
  geom_text(aes(0.6, 0.3, label = "6/12", vjust = -1))+
  scale_y_continuous(name = "VA detection threshold (logMAR)")+
  scale_x_discrete(name = "Condition")
  
show(vaPlot)
ggsave(plot = vaPlot, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/vaPlot.png",
       dpi = 800)

# look at descriptive stats for the groups
threshVA %>%
  group_by(condition) %>%
  get_summary_stats(threshold, type = "mean_sd")

# calculate t score
va.test <- threshVA %>%
  t_test(threshold ~ condition) %>%
  add_significance()
va.test



###### Process CS thresh data
cs <- subset(allCS, !(Participant.Private.ID %in% visProbs))

# pivot longer
cs <- cs %>%
  pivot_longer(cols = c(blurPercCorrect, clearPercCorrect),
               names_to = "condition", values_to = "percCorrect",
               values_drop_na = TRUE)%>%
  mutate(condition = case_when(
    condition == "blurPercCorrect" ~ "blur",
    TRUE ~ "clear"
  ))

# order by participant, then condition, then contrast
cs <- cs %>%
  arrange(Participant.Private.ID, condition, -Contrast)

# remove contrasts when perc correct is less than 50%
cs <- cs %>%
  filter(percCorrect >= .5)

# choose lowest value as threshold
# Participant 5372572 has threshold of 52 for blur
# Participant 5384675 has threshold of 26 for clear
threshCS <- cs %>%
  group_by(Participant.Private.ID, condition) %>%
  summarise(threshold = min(Contrast))

csPlot <- ggplot(threshCS, aes(x = condition, y = threshold))+
  geom_boxplot()+
  scale_y_log10(name = "Contrast detection threshold (%)")+
  scale_x_discrete(name = "Condition")

show(csPlot)
ggsave(plot = csPlot, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/csPlot.png",
       dpi = 800)

# look at descriptive stats for the groups
threshCS %>%
  group_by(condition) %>%
  get_summary_stats(threshold, type = "mean_sd")

# calculate t score
cs.test <- threshCS %>%
  t_test(threshold ~ condition) %>%
  add_significance()
cs.test

t.test(threshold ~ condition, data = threshCS, paired = TRUE)
