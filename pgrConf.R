library(tidyverse)
library(rstatix)

# import data
#allData <- read_csv("../cleanData/taskData.csv")

# set iamge characteristics
colors <- c("#d5277b", "#279e90")
h = 4
widthOF = 8
w = 5
d = 800
labels = c("Blur", "No blur")
# seperate tasks
allOF <- read_csv("../cleanData/ofData.csv")
allVA <- read_csv("../cleanData/vaThresh.csv")
allCS <- read_csv("../cleanData/csThresh.csv")

#read target coordinates
coordinates <- read_csv("../opticFlowOffsets.csv")

## plot data on xy axis
ggplot(data = allOF, aes(x = X.Coordinate, y = Y.Coordinate, 
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
ymax <- mean(allOF$Y.Coordinate)+2*sd(allOF$Y.Coordinate)
ymin <- mean(allOF$Y.Coordinate)-2*sd(allOF$Y.Coordinate)

# remove participants with extreme y values
data <- allOF %>%
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
data$contrast <- factor(data$contrast, levels = c("Low", "Medium", "High"))

# plot the interaction
ofPlot <- ggplot(data = data, aes(x = contrast, y = absAngError, color = condition, group = condition))+
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  scale_x_discrete(labels = c("10" = "Low", "50" = "Medium", "100" = "High"),
                   name = "Contrast") +
  scale_y_continuous(name = "Absolute Error (degrees)",
                     breaks = c(4, 6, 8, 10, 12))+
  scale_color_manual(values = colors, 
                     labels = labels,
                     name = "Condition")
show(ofPlot)

ggsave(plot = ofPlot, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/ofPlot.png",
       dpi = d,
       height = h,
       width = widthOF)


errorAov <- aov(absAngError ~ condition * contrast, data = data)
summary(errorAov)

postHoc <- TukeyHSD(errorAov)
print(postHoc)
#                               diff       lwr        upr        p adj
#clear:Low-blur:Low       -2.1413155 -2.652572 -1.6300594 0.000000e+00
#clear:Medium-blur:Medium -0.1295950 -0.638863  0.3796730 9.788989e-01
#clear:High-blur:High      0.1191575 -0.391287  0.6296020 9.856646e-01

# create linear models


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
vaPlot <- ggplot(allVA, aes(x = condition, group = condition, y = threshold, color = condition))+
  geom_boxplot()+
  geom_hline(yintercept = 0.176, linetype="dashed", color = "red", size = 1)+ 
  geom_text(aes(0.6, 0.176, label = "6/9", vjust = -1))+
  geom_hline(yintercept = 0.3, linetype="dashed", color = "red", size = 1)+ 
  geom_text(aes(0.6, 0.3, label = "6/12", vjust = -1))+
  scale_y_continuous(name = "Visual Acuity (logMAR)")+
  scale_x_discrete(name = "Condition", labels = labels)+
  scale_color_manual(values = colors, 
                     name = "Condition", 
                     labels = labels)
  
show(vaPlot)
ggsave(plot = vaPlot, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/vaPlot.png",
       dpi = d,
       width = w,
       height = h)

# look at descriptive stats for the groups
allVA %>%
  group_by(condition) %>%
  get_summary_stats(threshold, type = "mean_sd")

# calculate t score
va.test <- allVA %>%
  t_test(threshold ~ condition, paired = TRUE) %>%
  add_significance()
va.test



###### Process CS thresh data

allCS <- allCS %>%
  mutate(CS = -log10(1/threshold))

csPlot <- ggplot(allCS, aes(x = condition, y = CS, color = condition))+
  geom_boxplot()+
  scale_y_continuous(name = "Log Contrast Sensitivity")+
  scale_x_discrete(name = "Condition", labels = labels)+
  scale_color_manual(values = colors, 
                     labels = labels, 
                     name = "Condition")

show(csPlot)
ggsave(plot = csPlot, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/csPlot.png",
       dpi = d,
       width = w,
       height = h)

# look at descriptive stats for the groups
allCS %>%
  group_by(condition) %>%
  get_summary_stats(CS, type = "mean_sd")

# calculate t score
cs.test <- allCS %>%
  t_test(CS ~ condition, paired = TRUE) %>%
  add_significance()
cs.test

# create a single data frame with CS, VA, condition and error score
summaryDF <- data %>%
  group_by(Participant.Private.ID, condition)%>%
  summarise(meanAngError = mean(absAngError))

# merge in VA and CS thresholds
summaryDF <- merge(summaryDF, allCS)%>%
  select(-threshold)
summaryDF <- merge(summaryDF, allVA)
summaryDF$VA <- summaryDF$threshold 

summaryDF <- summaryDF%>%
  select(-threshold)

# create linear model
lmVision <- lm(meanAngError ~ condition + CS + VA, data = summaryDF)
summary(lmVision)

csError <- ggplot(summaryDF, aes(x= CS, y = meanAngError))+
  geom_jitter(aes(color = condition))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = colors, 
                     labels = labels,
                     name = "Condition")+
  scale_y_continuous(name = "Mean Angular Error (degrees)")+
  scale_x_continuous(name = "Log Contrast Sensitivity")
show(csError)

ggsave(plot = csError, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/csError.png",
       dpi = d,
       width = 6,
       height = h)

vaError <- ggplot(summaryDF, aes(x= VA, y = meanAngError))+
  geom_point(aes(color = condition))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = colors,
                     labels = labels,
                     name = "Condition")+
  scale_y_continuous(name = "Mean Angular Error (degrees)")+
  scale_x_continuous(name = "Visual Acuity (logMAR)")
show(vaError)

ggsave(plot = vaError, 
       "C:/Users/cn13ws/OneDrive - University of Leeds/pgrConference/vaError.png",
       dpi = d,
       width = 6,
       height = h)

# create MLM
base <- nlme::lme(
  fixed = meanAngError ~ 1,
  random = ~1|Participant.Private.ID,
  data = summaryDF, na.action=na.omit
)

cond <- nlme::lme(
  fixed = meanAngError ~ condition,
  random = ~1|Participant.Private.ID,
  data = summaryDF, na.action=na.omit
) 

cs <- nlme::lme(
  fixed = meanAngError ~ condition + CS,
  random = ~1|Participant.Private.ID,
  data = summaryDF, na.action=na.omit
)

va <- nlme::lme(
  fixed = meanAngError ~ condition + CS + VA,
  random = ~1|Participant.Private.ID,
  data = summaryDF, na.action=na.omit
)

anova(base, cond, cs, va)

ggplot(summaryDF, aes(x = VA, y = meanAngError, colour = condition))+
  geom_point()+
  geom_smooth(method = "lm")
