library(tidyverse)

# import data
allData <- read_csv("../testData/jamesData.csv")
coordinates <- read_csv("../opticFlowOffsets.csv")

# remove gaps from column names
names(allData) <- make.names(names(allData), unique = TRUE)


# create variable with Zone.Name s of inerest
zones <- c("response_button_text", "response_text_entry", "click_painting")

# filter allData by zones of interset
data <- allData %>%
  filter(Zone.Type %in% zones)

# split the data frame by condition
lens1 <- data[1:(match("lens2",data$display)-1),]
lens2 <- data[match("lens2",data$display):length(data$display),]

# select optic flow data only
clearOF <- lens1 %>%
  filter(display == "ofTest")%>%
  filter(Response == "click")
blurOF <- lens2 %>%
  filter(display == "ofTest")%>%
  filter(Response == "click")

# create variable wit videos for full contrast condition
videos <- c("0.mp4", "4.mp4", "8.mp4", "12.mp4", "16.mp4", "20.mp4",
            "0right.mp4", "4right.mp4", "8right.mp4", "12right.mp4", "16right.mp4", "20right.mp4")

# filter OF variables by videos
clearOF <- clearOF %>%
  filter(video %in% videos)
blurOF <- blurOF %>%
  filter(video %in% videos)

# select columns of interest
clearOF <- clearOF %>%
  select(Reaction.Time,
         X.Coordinate, Y.Coordinate,
         video)
blurOF <- blurOF %>%
  select(Reaction.Time,
         X.Coordinate, Y.Coordinate,
         video)

# make X and Y coordinates numeric
clearOF$X.Coordinate <- as.numeric(clearOF$X.Coordinate)
clearOF$Y.Coordinate <- as.numeric(clearOF$Y.Coordinate) 

blurOF$X.Coordinate <- as.numeric(clearOF$X.Coordinate)
blurOF$Y.Coordinate <- as.numeric(clearOF$Y.Coordinate)

# create target varaible
clearOF <- clearOF %>%
  mutate(target = case_when(
    video == "4.mp4" ~ 304,
    video == "4right.mp4" ~ 336,
    video == "8.mp4" ~ 288,
    video == "8right.mp4" ~ 352,
    video == "12.mp4" ~ 272,
    video == "12right.mp4" ~ 368,
    video == "16.mp4" ~ 255,
    video == "16right.mp4" ~ 385,
    video == "20.mp4" ~ 238,
    video == "20right.mp4" ~ 402,
    TRUE ~ 320
  ))

blurOF <- blurOF%>%
  mutate(target = case_when(
    video == "4.mp4" ~ 304,
    video == "4right.mp4" ~ 336,
    video == "8.mp4" ~ 288,
    video == "8right.mp4" ~ 352,
    video == "12.mp4" ~ 272,
    video == "12right.mp4" ~ 368,
    video == "16.mp4" ~ 255,
    video == "16right.mp4" ~ 385,
    video == "20.mp4" ~ 238,
    video == "20right.mp4" ~ 402,
    TRUE ~ 320
  ))

# create absolute error varaible
clearOF <- clearOF %>%
  mutate(signedError = target-X.Coordinate,
         absError = abs(target-X.Coordinate))
blurOF <- blurOF %>%
  mutate(signedError = target-X.Coordinate,
         absError = abs(target-X.Coordinate))

# lets have a look at some aggregate measures by video
clearSummary <- clearOF %>%
  group_by(video) %>%
  summarise(meanRT = mean(Reaction.Time),
            clearMeanError = mean(absError))

blurSummary <- blurOF %>%
  group_by(video) %>%
  summarise(meanRT = mean(Reaction.Time),
            blurMeanError = mean(absError))

joinedDF <- clearSummary %>%
  select(-meanRT)

joinedDF$blurMeanError <- blurSummary$blurMeanError

t.test(x = joinedDF$clearMeanError, y = joinedDF$blurMeanError, paired = TRUE, alternative = "two.sided")
