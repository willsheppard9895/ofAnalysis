library(tidyverse)
library(lme4)

# create list of file names
files <- c("task-d4fw", "task-dqjp", "task-f3ud", "task-mva3", "task-oex3",
           "task-og71", "task-rek9", "task-s8ch", "task-svji", "task-z5dj")

# import data

# read test data
# this first one is for tab deliminated files
#for(i in files){
#  filepath <- file.path("../data/",paste("data_exp_62188-v20_",i,".csv",sep=""))
#  assign(i, read_delim(filepath, delim = "\t"))
#}

for(i in files){
  filepath <- file.path("../data/",paste("data_exp_62188-v20_",i,".csv",sep=""))
  assign(i, read_csv(filepath))
}
#read target coordinates
coordinates <- read_csv("../opticFlowOffsets.csv")

# comnbine data frames
# cannot combine the empty data sets with those with data in as the data types do not match
#allData <- bind_rows(`task-d4fw`, `task-dqjp`, `task-f3ud`, `task-mva3`, `task-oex3`,
#                     `task-og71`, `task-rek9`, `task-s8ch`, `task-svji`, `task-z5dj`)

allData <- bind_rows(`task-d4fw`, `task-dqjp`, `task-f3ud`, `task-mva3`, `task-oex3`,
                     `task-og71`, `task-rek9`,`task-svji`, `task-z5dj`)


# remove gaps from column names
names(allData) <- make.names(names(allData), unique = TRUE)

# check faulty participant and relabel conditions
pp <- allData %>%
  filter(Participant.Private.ID == 5384850)

pp <- pp %>%
  mutate(Response = case_when(
    display == "lens1" ~ "No blur",
    TRUE ~ Response
  ))


# remove faulty participant
allData <- allData %>%
  filter(Participant.Private.ID != 5384850)


########## UNBLOCK FOR STUDENTS DATA #########
# add participant back in with swapped condition values
#allData <- rbind(allData, pp) 

# create a condition column 
allData <- allData %>%
  mutate(condition = case_when(
    Response == "Monocular blur" ~ "blur",
    Response == "No blur" ~ "clear"
  ))
# fill column with condition above
allData <- allData %>% fill(condition)


# create variable with Zone.Name s of inerest
zones <- c("response_button_text", "response_text_entry", "click_painting")


# remove observations where the response is 2SD slower than the mean or less than .2s, for that participant and video
# removes no participants so step is unnecassary
#data <- data %>%
#  group_by(Participant.Private.ID) %>%
#  mutate(rtOutlier = case_when(
#    #Reaction.Time > mean(Reaction.Time)+2*sd(Reaction.Time) ~ 1,
#    Reaction.Time < .2 ~ 1,
#    TRUE ~ 0
#  ))
#rtOut <- data %>%
#  filter(rtOutlier == 1)
#data <- data %>%
#  filter(rtOutlier == 0)


# create variable wit videos and filter data by this variable to select task rows for the general experiemnt
videos <- c("0.mp4", "4.mp4", "8.mp4", "12.mp4", "16.mp4", "20.mp4",
            "0right.mp4", "4right.mp4", "8right.mp4", "12right.mp4", "16right.mp4", "20right.mp4",
            "50_0.mp4", "50_4.mp4", "50_8.mp4", "50_12.mp4", "50_16.mp4", "50_20.mp4",
            "50_0right.mp4", "50_4right.mp4", "50_8right.mp4", "50_12right.mp4", "50_16right.mp4", "50_20right.mp4",
            "90_0.mp4", "90_4.mp4", "90_8.mp4", "90_12.mp4", "90_16.mp4", "90_20.mp4",
            "90_0right.mp4", "90_4right.mp4", "90_8right.mp4", "90_12right.mp4", "90_16right.mp4", "90_20right.mp4")

############################# always work back from this point, check that all of the code works

data <- allData %>%
  filter(Zone.Type %in% zones)%>%
  filter(display == "ofTest")%>%
  filter(Response == "click")%>%
  filter(video %in% videos)%>%
  select(Participant.Private.ID, condition, Reaction.Time,
         X.Coordinate, Y.Coordinate,
         video)

# set height to midline in pixels
midHeightPx = 255

# select columns of interest
data <- data %>%
  mutate(midDistPx = X.Coordinate - 320)%>%
  mutate(angle2mid = (atan(midDistPx/midHeightPx)*(180/pi) ) )#%>%
  #select(-midDistPx)


# create target, signed error, absolute error, offset and contrast variables
data <- data %>%
  mutate(contrast = case_when(
    str_detect(video, "^50*") ~ 50,
    str_detect(video, "^90*") ~ 10,
    TRUE ~ 100
  )) %>%
  mutate(offset = case_when(
    str_detect(video, "4.mp4$") ~ -4, str_detect(video, "4right.mp4$") ~ 4,
    str_detect(video, "8.mp4$") ~ -8, str_detect(video, "8right.mp4$") ~ 8,
    str_detect(video, "12.mp4$") ~ -12, str_detect(video, "12right.mp4$") ~ 12,
    str_detect(video, "16.mp4$") ~ -16, str_detect(video, "16right.mp4$") ~ 16,
    str_detect(video, "20.mp4$") ~ -20, str_detect(video, "20right.mp4$") ~ 20,
    TRUE ~ 0
  )) %>%
  mutate(angError = angle2mid-offset)%>%
  mutate(absAngError = abs(angError))

test <- data %>%
  group_by(offset) %>%
  summarise(error = mean(absAngError),
            sdError = sd(absAngError))



#%>%
  #mutate(targetAngle = case_when(
  #    str_detect(video, "4.mp4$") | str_detect(video, "4right.mp4$") ~ midHeightpx*tan(4*(pi/180)),
  #    str_detect(video, "8.mp4$") | str_detect(video, "8right.mp4$") ~ 8,
  #    str_detect(video, "12.mp4$") | str_detect(video, "12right.mp4$") ~ 12,
  #    str_detect(video, "16.mp4$") | str_detect(video, "16right.mp4$") ~ 16,
  #    str_detect(video, "20.mp4$") | str_detect(video, "20right.mp4$") ~ 20,
  #    TRUE ~ 0
    #))#%>%
  #mutate(signedError = target-X.Coordinate,
  #       absError = abs(target-X.Coordinate))
  #mutate(angError = angle2mid-offset)

## filter observations with particularly high error
#data <- data %>% group_by(Participant.Private.ID)%>%
#  mutate(errorOutlier = case_when(
#    absError > mean(absError)+2*sd(absError) ~ 1,
#    TRUE ~ 0))
#errorOut <- data %>%
#  filter(errorOutlier == 1)
#data <- data %>%
#  filter(errorOutlier == 0)
write.csv(data, "../testData/ofData.csv")

sdata$offset <- as.integer(data$offset)

ggplot(data = data, aes(x = X.Coordinate, y = offset, 
                        color = offset, fill = offset))+
  geom_point(alpha = 0.01)+
  scale_x_continuous(limits = c(0, 640))+
  #scale_y_continuous(limits = c(0, 480))+
  #geom_hline(yintercept = 255, linetype="dashed", color = "red", size = 1)+
  geom_vline(xintercept = 320, linetype="dashed", color = "red", size = 1)

ggplot(data = data, aes(x = X.Coordinate, y = Y.Coordinate, 
                        color = offset, fill = offset))+
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

# create aggregate measures of error by condition
# pivot wider by condition
dataSummary <- data %>%
  group_by(Participant.Private.ID, condition, offset, contrast)%>%
  summarise(meanRT = mean(Reaction.Time),
            meanError = mean(absError))%>%
  pivot_wider(id_cols = c(Participant.Private.ID, offset, contrast), 
              names_from = condition, 
              values_from = c(meanError, meanRT))
#write.csv(dataSummary, "../testData/ofDataSummary.csv")

ggplot(dataSummary, aes(x = meanError_clear, y = meanError_blur))+
  geom_point()+
  geom_smooth(method = lm)


# t test to see if there is a difference in paired sample t test
t.test(x = dataSummary$meanError_clear, y = dataSummary$meanError_blur, 
       paired = TRUE, alternative = "two.sided")
t.test(x = dataSummary$meanRT_clear, y = dataSummary$meanRT_blur, 
       paired = TRUE, alternative = "two.sided")

errorAov <- aov(absAngError ~ condition * contrast, data = data)
summary(errorAov)

TukeyHSD(errorAov, "contrast")

rtAov <- aov(Reaction.Time ~ condition * contrast * offset, data = data)
summary(rtAov)

errorLmer <- lmer(absAngError ~ condition * contrast * offset + (1|Participant.Private.ID), 
                  data = data)
summary(errorLmer)

rtLmer <- lmer(Reaction.Time ~ condition + (1|Participant.Private.ID), 
        data = data)
summary(rtLmer)
# look into how t run post hoc tests for anova on r

# Mixed Effects Modelling!!!
# i think that i need to look into random effects
# fixed: blur v no blur
# random: contrast, offset, participant?

data$condition <- as.factor(data$condition)
data$contrast <- as.factor(data$contrast)
data$offset <- as.factor(data$offset)

ggplot(data)+
  aes(x = contrast, y = absError)+
  geom_boxplot()+
  scale_y_continuous(trans = "log2")+
  facet_wrap(~condition)

clearAov <- aov(clearMeanError ~ contrast + offset, data = joinedDF)
blurAov <- aov(blurMeanError ~ contrast + offset, data = joinedDF)
summary(clearAov)
summary(blurAov)

write.csv(joinedDF, "../testData/joinedDF.csv")

# make offset and contrast factors
joinedDF$offset <- as.factor(joinedDF$offset)
joinedDF$contrast <- as.factor(joinedDF$contrast)
# some quick graphs
ggplot(data = joinedDF, aes(x = offset, y = clearMeanError))+
  geom_boxplot()
ggplot(data = joinedDF, aes(x = offset, y = blurMeanError))+
  geom_boxplot()

ggplot(data = joinedDF, aes(x = contrast, y = clearMeanError))+
  geom_boxplot()
ggplot(data = joinedDF, aes(x = contrast, y = blurMeanError))+
  geom_boxplot()

# how is signed error affected by offset?
ggplot(data, aes(x = signedError, y = video))+
  geom_point()+
  facet_wrap(~offset)
