library(tidyverse)

# import data
allData <- read_csv("../cleanData/taskData.csv")

# create variable with Zone.Name s of inerest
zones <- c("response_button_text", "response_text_entry", "click_painting")

# create variable wit videos and filter data by this variable to select task rows for the general experiemnt
videos <- c("0.mp4", "4.mp4", "8.mp4", "12.mp4", "16.mp4", "20.mp4",
            "0right.mp4", "4right.mp4", "8right.mp4", "12right.mp4", "16right.mp4", "20right.mp4",
            "50_0.mp4", "50_4.mp4", "50_8.mp4", "50_12.mp4", "50_16.mp4", "50_20.mp4",
            "50_0right.mp4", "50_4right.mp4", "50_8right.mp4", "50_12right.mp4", "50_16right.mp4", "50_20right.mp4",
            "90_0.mp4", "90_4.mp4", "90_8.mp4", "90_12.mp4", "90_16.mp4", "90_20.mp4",
            "90_0right.mp4", "90_4right.mp4", "90_8right.mp4", "90_12right.mp4", "90_16right.mp4", "90_20right.mp4")

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
  mutate(angle2mid = (atan(midDistPx/midHeightPx)*(180/pi) ) )

# create target, signed error, absolute error, offset and contrast variables
data <- data %>%
  mutate(contrast = case_when(
    str_detect(video, "^50*") ~ "Medium",
    str_detect(video, "^90*") ~ "Low",
    TRUE ~ "High"
  )) %>%
  mutate(offset = case_when(
    str_detect(video, "4.mp4$") ~ -4, str_detect(video, "4right.mp4$") ~ 4,
    str_detect(video, "8.mp4$") ~ -8, str_detect(video, "8right.mp4$") ~ 8,
    str_detect(video, "12.mp4$") ~ -12, str_detect(video, "12right.mp4$") ~ 12,
    str_detect(video, "16.mp4$") ~ -16, str_detect(video, "16right.mp4$") ~ 16,
    str_detect(video, "20.mp4$") ~ -20, str_detect(video, "20right.mp4$") ~ 20,
    TRUE ~ 0
  )) %>% mutate(absOffset = abs(offset))%>%
  mutate(angError = angle2mid-offset)%>%
  mutate(absAngError = abs(angError))

write.csv(data, "../cleanData/ofData.csv")
