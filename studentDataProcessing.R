library(tidyverse)

# create list of file names
files <- c("task-d4fw", "task-dqjp", "task-f3ud", "task-mva3", "task-oex3",
           "task-og71", "task-rek9", "task-s8ch", "task-svji", "task-z5dj")

# import data
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

# add participant back in with swapped condition values
allData <- rbind(allData, pp) 

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

# filter data into individual data sets for cs, va and of
csAllData <- allData %>%
  filter(Zone.Type %in% zones)%>%
  filter(display == "cs") 

vaAllData <- allData %>%
  filter(Zone.Type %in% zones)%>%
  filter(display == "va")
  
ofAllData <- allData %>%
  filter(Zone.Type %in% zones)%>%
  filter(display == "ofTest")%>%
  filter(Response == "click")

# remove rows where response was in < .2s
#csData <- csData %>%
#  group_by(Participant.Private.ID) %>%
#  mutate(rtOutlier = case_when(
#    Reaction.Time < .2 ~ 1,
#    TRUE ~ 0
#  ))
#csOut <- csData %>%
#  filter(rtOutlier == 1)
#csData <- csData %>%
#  filter(rtOutlier == 0)

#vaData <- vaData %>%
#  group_by(Participant.Private.ID) %>%
#  mutate(rtOutlier = case_when(
#    Reaction.Time < .2 ~ 1,
#    TRUE ~ 0
#  ))
#vaOut <- vaData %>%
#  filter(rtOutlier == 1)
#vaData <- vaData %>%
#  filter(rtOutlier == 0)

#ofData <- ofData %>%
#  group_by(Participant.Private.ID) %>%
#  mutate(rtOutlier = case_when(
#    Reaction.Time < .2 ~ 1,
#    TRUE ~ 0
#  ))
#ofOut <- ofData %>%
#  filter(rtOutlier == 1)
#ofData <- ofData %>%
#  filter(rtOutlier == 0)


## Process OF data

# create variable wit videos and filter data by this variable to select task rows for the general experiemnt
videos <- c("0.mp4", "4.mp4", "8.mp4", "12.mp4", "16.mp4", "20.mp4",
            "0right.mp4", "4right.mp4", "8right.mp4", "12right.mp4", "16right.mp4", "20right.mp4",
            "50_0.mp4", "50_4.mp4", "50_8.mp4", "50_12.mp4", "50_16.mp4", "50_20.mp4",
            "50_0right.mp4", "50_4right.mp4", "50_8right.mp4", "50_12right.mp4", "50_16right.mp4", "50_20right.mp4",
            "90_0.mp4", "90_4.mp4", "90_8.mp4", "90_12.mp4", "90_16.mp4", "90_20.mp4",
            "90_0right.mp4", "90_4right.mp4", "90_8right.mp4", "90_12right.mp4", "90_16right.mp4", "90_20right.mp4")

ofData <- ofAllData %>%
  filter(video %in% videos)

# select columns of interest
ofData <- ofData %>%
  select(Participant.Private.ID, condition, Reaction.Time,
         X.Coordinate, Y.Coordinate,
         video)


# create target, signed error, absolute error, offset and contrast variables
ofData <- ofData %>%
  mutate(contrast = case_when(
    str_detect(video, "^50*") ~ 50,
    str_detect(video, "^90*") ~ 10,
    TRUE ~ 100
  )) %>%
  mutate(offset = case_when(
    str_detect(video, "4.mp4$") | str_detect(video, "4right.mp4$") ~ 4,
    str_detect(video, "8.mp4$") | str_detect(video, "8right.mp4$") ~ 8,
    str_detect(video, "12.mp4$") | str_detect(video, "12right.mp4$") ~ 12,
    str_detect(video, "16.mp4$") | str_detect(video, "16right.mp4$") ~ 16,
    str_detect(video, "20.mp4$") | str_detect(video, "20right.mp4$") ~ 20,
    TRUE ~ 0
  ))%>%
  mutate(target = case_when(
    str_detect(video, "4.mp4$") ~ 304,
    str_detect(video, "4right.mp4$") ~ 336,
    str_detect(video, "8.mp4$") ~ 288,
    str_detect(video, "8right.mp4$") ~ 352,
    str_detect(video, "12.mp4$") ~ 272,
    str_detect(video, "12right.mp4$") ~ 368,
    str_detect(video, "16.mp4$") ~ 255,
    str_detect(video, "16right.mp4$") ~ 385,
    str_detect(video, "20.mp4$") ~ 238,
    str_detect(video, "20right.mp4$") ~ 402,
    TRUE ~ 320
  ))%>%
  mutate(absError = abs(target-X.Coordinate))

# remove X.Coordinate, Y.Coordinate, video, target
ofData <- ofData %>%
  select(-X.Coordinate, -Y.Coordinate, -video, -target)

ofData <- ofData[order(ofData$Participant.Private.ID, ofData$condition, ofData$contrast, ofData$offset),]

# Pivot wider to be entered into SPSS
# to uniquely identify the data points, they need a trial number to sit next to participant id
noTrials <- 6
reps <- length(ofData$Participant.Private.ID)/noTrials
pattern <- rep(c(1:6), times = reps)

ofData$trialNo <- pattern

ofWide <- ofData %>%
  pivot_wider(names_from = c(condition, contrast, offset), values_from = c(Reaction.Time, absError))%>%
  select(-trialNo)

ofData <- ofData %>%
  select(-trialNo)

write.csv(ofData, "../testData/of.csv")

## Process cs Data

# select columns of interest
csData <- csAllData %>%
  select(Participant.Private.ID, condition, Screen.Name, Attempt, Correct)

# revalue levels to % contrast
# level 1 = 100% contrast
csData$Contrast <- 101 - as.numeric(str_remove(csData$Screen.Name, "Level"))

csData <- csData %>%
  select(-Screen.Name)

# inspectg raw data of weirtd particiapnt
test <- csAllData %>%
  filter(Participant.Private.ID == 5372572)%>%
  select(letter, Response, Correct, display, condition)

# calculate percentage correct for each % contrast for each particiapnt
csData <- csData %>%
  group_by(Participant.Private.ID, condition, Contrast)%>%
  summarise(Attempts = sum(Attempt),
         Correct = sum(Correct))%>%
  mutate(percCorrect = Correct/Attempts)%>%
  select(-Attempts, - Correct) %>%
  ungroup()

# create 2 data frames for the conditions
csBlur <- csData %>%
  filter(condition == "blur")
csClear <- csData %>%
  filter(condition == "clear")


# rename perc/Correct to include condition
csBlur$blurPercCorrect <- csBlur$percCorrect 
csClear$clearPercCorrect <- csClear$percCorrect


# remove unnecassary columns
csBlur <- csBlur %>%
  select(- condition, - percCorrect)
csClear <- csClear%>%
  select(- condition, - percCorrect)


# bind the data frames
csData <- merge(csBlur, csClear, by = c("Participant.Private.ID", "Contrast"), all = TRUE)

csData<- csData[order(csData$Participant.Private.ID, -csData$Contrast),]

#write.csv(csData, "../testData/cs.csv")

## Process va Data

# select columns of interest
vaData <- vaAllData %>%
  select(Participant.Private.ID, condition, Screen.Name, Attempt, Correct)

#revalue to logMAR
# level 1 = 1 logMAR
# level 13 = -.2 logMAR
vaData$logMAR <- 1.1 - (as.numeric(str_remove(vaData$Screen.Name, "Level"))/10)
vaData <- vaData %>%
  select(-Screen.Name)

# calculate perc correct for each particiapnt and logMAR value
vaData <- vaData %>%
  group_by(Participant.Private.ID, condition, logMAR)%>%
  summarise(Attempts = sum(Attempt),
            Correct = sum(Correct))%>%
  mutate(percCorrect = Correct/Attempts)%>%
  select(-Attempts, - Correct) %>%
  ungroup()

# create 2 data frames for the conditions
vaBlur <- vaData %>%
  filter(condition == "blur")
vaClear <- vaData %>%
  filter(condition == "clear")


# rename perc/Correct to include condition
vaBlur$blurPercCorrect <- vaBlur$percCorrect 
vaClear$clearPercCorrect <- vaClear$percCorrect


# remove unnecassary columns
vaBlur <- vaBlur %>%
  select(- condition, - percCorrect)
vaClear <- vaClear%>%
  select(- condition, - percCorrect)


# bind the data frames
vaData <- merge(vaBlur, vaClear, by = c("Participant.Private.ID", "logMAR"), all = TRUE)

vaData <- vaData[order(vaData$Participant.Private.ID, -vaData$logMAR),]
write.csv(vaData, "../testData/va.csv")

# create a  list of first 3 participants, filter each data set by this to give students on 3 pps of data
number <- 10
pps <- unique(ofData$Participant.Private.ID)[1:number]
ofShort <- ofData %>%
  filter(Participant.Private.ID %in% pps)
ofShortWide <- ofWide%>%
  filter(Participant.Private.ID %in% pps)
csShort <- csData%>%
  filter(Participant.Private.ID %in% pps)
vaShort <- vaData%>%
  filter(Participant.Private.ID %in% pps)
## Save data frames
write.csv(ofData, "../studentData/studentOF.csv")
write.csv(ofWide, "../studentData/studentOFwide.csv")
write.csv(vaData, "../studentData/studentVA.csv")
write.csv(csData, "../studentData/studentCS.csv")
