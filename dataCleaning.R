library(tidyverse)

######## Particiapnts that are removed and why ########
######## Remaining N = 52
# 5384850 - not sure which condition is which
# 5722070 - over 2sd above mean of error in both rounds of stationary calibration
# 5384675 - over 2sd above mean of error in both rounds of moving calibration 
## All participants with self-reported visual conditions removed - sig. greater ngular error than general sample - can't tell if issues are due to exisiting problem or maniulation
# 5508881 - cataracts
# 5532853 - Deuteranopia (color blind)
# 5540429 - had cataract, still has film over left eye
# 5658787 - Diabetic Retinopathy
# 5372572- spamming the same key on blur CS 

# create list of file names
files <- c("task-d4fw", "task-dqjp", "task-f3ud", "task-mva3", "task-oex3",
           "task-og71", "task-rek9", "task-s8ch", "task-svji", "task-z5dj")
for(i in files){
  filepath <- file.path("../data/",paste("data_exp_62188-v20_",i,".csv",sep=""))
  assign(i, read_csv(filepath))
}
#read target coordinates
coordinates <- read_csv("../opticFlowOffsets.csv")

# read demographcics data
demoData <- read.csv("../studentData/studentDemographicsWide.csv")

# comnbine data frames
# cannot combine the empty data sets with those with data in as the data types do not match
#allData <- bind_rows(`task-d4fw`, `task-dqjp`, `task-f3ud`, `task-mva3`, `task-oex3`,
#                     `task-og71`, `task-rek9`, `task-s8ch`, `task-svji`, `task-z5dj`)

taskData <- bind_rows(`task-d4fw`, `task-dqjp`, `task-f3ud`, `task-mva3`, `task-oex3`,
                     `task-og71`, `task-rek9`,`task-svji`, `task-z5dj`)

# remove gaps from column names
names(taskData) <- make.names(names(taskData), unique = TRUE)

# remove participant 5384850 - not sure which condition is which
# leaves n of 59
taskData <- taskData %>%
  filter(Participant.Private.ID != 5384850)

# create a condition column 
taskData <- taskData %>%
  mutate(condition = case_when(
    Response == "Monocular blur" ~ "blur",
    Response == "No blur" ~ "clear"
  ))
# fill column with condition above
taskData <- taskData %>% fill(condition)

# select data for ofTest and ofCalibrate
ofTasks <- c("ofTest", "ofCalibrate")
responseZones <- c("response_button_text", "response_text_entry", "click_painting")

ofAllData <- taskData %>%
  filter(display %in% ofTasks) %>%
  filter(Zone.Type %in% responseZones)%>%
  filter(Response != "click limit reached")

## Calculate angle to the centre for all data points
# create center height variable
centerHeight <- 255

ofAllData <- ofAllData %>%
  mutate(centerDist = X.Coordinate - 320)%>%
  mutate(centerAng = atan(centerDist/centerHeight)*(180/pi))

## select ofCalibrate data
allOfCalibrate <- ofAllData %>%
  filter(display == "ofCalibrate")

# select columns
ofCalibrate <- allOfCalibrate %>%
  select(Participant.Private.ID, condition, randomise_trials, cross, X.Coordinate, centerDist, centerAng)

# read in calibration target location spreadsheets
calibTargets <- read_csv("../calibrationTargets.csv")

# create new column with angle to center
calibTargets <- calibTargets %>%
  mutate(targetCenterAng = atan(centerDist/Y.Coordinate)*(180/pi))


# add new column matching target angle to cross
ofCalibrate <- ofCalibrate %>%
  mutate(targetCenterAng = case_when(
    cross == calibTargets$cross[1] ~ calibTargets$targetCenterAng[1],
    cross == calibTargets$cross[2] ~ calibTargets$targetCenterAng[2],
    cross == calibTargets$cross[3] ~ calibTargets$targetCenterAng[3],
    cross == calibTargets$cross[4] ~ calibTargets$targetCenterAng[4],
    cross == calibTargets$cross[5] ~ calibTargets$targetCenterAng[5]
  )) %>%
  mutate(angDiff = abs(targetCenterAng - centerAng))

# By particiapnt and condition, look at mean error
ofCalibrateSummary <- ofCalibrate %>%
  group_by(Participant.Private.ID, condition)%>%
  summarise(meanError = mean(angDiff))

# pivot wider to get 1 participant per row and create an aggregate error score
# participant: 5722070 had ~ 20degrees of error in both rounds of calibration. Remove
# leaves N of 58
ofCalibrateSummary <- ofCalibrateSummary %>%
  pivot_wider(names_from = condition, values_from = meanError) %>%
  mutate(totalError = blur + clear)

# partiicpants with over 10 degrees of error in both condiitons
# 5722070
blurCalibEx <- mean(ofCalibrateSummary$blur)+2*sd(ofCalibrateSummary$blur)
clearCalibEx <- mean(ofCalibrateSummary$clear)+2*sd(ofCalibrateSummary$clear)

ofCalibrateExclude <- ofCalibrateSummary %>%
  filter(blur > blurCalibEx & clear > clearCalibEx)

# leaves N of 58
ofAllData <- ofAllData %>%
  filter(Participant.Private.ID != ofCalibrateExclude$Participant.Private.ID[1])

###### Now analyse second practice data
# create list of practice videos
practiceVids <- c("40.mp4", "40right.mp4")

allOfPractice <- ofAllData %>%
  filter(video %in% practiceVids)

allOfPractice <- allOfPractice %>%
  mutate(targetCenterAng = case_when(
    video == "40.mp4" ~ -40,
    video == "40right.mp4" ~ 40
  )) %>%
  mutate(angDiff = abs(targetCenterAng - centerAng))

# create summary measures
ofPracticeSummary <- allOfPractice %>%
  group_by(Participant.Private.ID, condition)%>%
  summarise(meanError = mean(angDiff))

ofPracticeSummary <- ofPracticeSummary %>%
  pivot_wider(names_from = condition, values_from = meanError) %>%
  mutate(totalError = blur + clear)

# particpants with over 10 degrees of error in both condiitons
# 5384675

blurPracEx <- mean(ofPracticeSummary$blur)+2*sd(ofPracticeSummary$blur)
clearPracEx <- mean(ofPracticeSummary$clear)+2*sd(ofPracticeSummary$clear)

ofPracticeExclude <- ofPracticeSummary %>%
  filter(blur > blurPracEx & clear > clearPracEx)


# remove these participants from allOfData
ofAllData <- ofAllData %>%
  filter(Participant.Private.ID != ofPracticeExclude$Participant.Private.ID[1])

# create data set for main vids
# create variable wit videos and filter data by this variable to select task rows for the general experiemnt
videos <- c("0.mp4", "4.mp4", "8.mp4", "12.mp4", "16.mp4", "20.mp4",
            "0right.mp4", "4right.mp4", "8right.mp4", "12right.mp4", "16right.mp4", "20right.mp4",
            "50_0.mp4", "50_4.mp4", "50_8.mp4", "50_12.mp4", "50_16.mp4", "50_20.mp4",
            "50_0right.mp4", "50_4right.mp4", "50_8right.mp4", "50_12right.mp4", "50_16right.mp4", "50_20right.mp4",
            "90_0.mp4", "90_4.mp4", "90_8.mp4", "90_12.mp4", "90_16.mp4", "90_20.mp4",
            "90_0right.mp4", "90_4right.mp4", "90_8right.mp4", "90_12right.mp4", "90_16right.mp4", "90_20right.mp4")

allOfMain <- ofAllData %>%
  filter(video %in% videos)

# create target, signed error, absolute error, offset and contrast variables
allOfMain <- allOfMain %>%
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
  mutate(angError = centerAng-offset)%>%
  mutate(absAngError = abs(angError))


## Check participants with medical condition vs general trend
# 5508881 - cataracts
# 5532853 - Deuteranopia (color blind)
# 5540429 - had cataract, still has film over left eye
# 5658787 - Diabetic Retinopathy
visProbs <- c(5508881, 5532853, 5540429, 5658787)

# create data frame where participants with visual issues are marked
probData <- allOfMain %>%
  mutate(prob = case_when(
    Participant.Private.ID %in% visProbs ~ 1,
    TRUE ~ 0
  ))
# look at descriptive stats for the groups
library(rstatix)
probData %>%
  group_by(prob) %>%
  get_summary_stats(absAngError, type = "mean_sd")

# calculate t score
prob.test <- probData %>%
  t_test(absAngError ~ prob) %>%
  add_significance()
prob.test

# participants with self reported visual problems had a sig. higher absAngError
# therefore remove
ofMain <- subset(allOfMain, !(Participant.Private.ID %in% visProbs))

# create a  list of remaining participants in optic flow
ofParticiapnts <- as.list(unique(ofMain$Participant.Private.ID))

################ import CS data ##################
allCS <- read_csv("../testData/cs.csv")
# remove participants not in OF data
CS <- allCS %>%
  filter(Participant.Private.ID %in% ofParticiapnts)

# 5372572- spamming the same key on blur CS - remove
CS <- CS %>%
  filter(Participant.Private.ID != 5372572)
csParticiapnts <- as.list(unique(CS$Participant.Private.ID))

################# import VA data #################
allVA <- read_csv("../testData/va.csv")

# remove participants not in OF data
VA <- allVA %>%
  filter(Participant.Private.ID %in% csParticiapnts)

# some partiicapnts havenn't done so well, but there are no runs to 0
# exclude no participants due to VA
data2Save <- taskData %>%
  filter(Participant.Private.ID %in% csParticiapnts)

write_csv(data2Save, "../cleanData/taskData.csv")
