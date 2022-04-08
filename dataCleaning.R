library(tidyverse)


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

## select ofCalibrate data
ofCalibrate <- ofAllData %>%
  filter(display == "ofCalibrate")
