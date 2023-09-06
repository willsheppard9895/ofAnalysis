library(tidyverse)

# import data
allData <- read_csv("../cleanData/taskData.csv")

# create variable with Zone.Name s of inerest
zones <- c("response_button_text", "response_text_entry", "click_painting")

# filter by zones
data <- allData %>%
  filter(Zone.Type %in% zones)
  

# import data
va <- data %>%
  filter(display == "va")

va <- va %>%
  dplyr::select(Participant.Private.ID, condition, Screen.Name, Attempt, Correct)

#revalue to logMAR
# level 1 = 1 logMAR
# level 13 = -.2 logMAR
va$logMAR <- 1.1 - (as.numeric(str_remove(va$Screen.Name, "Level"))/10)
va <- va %>%
  dplyr::select(-Screen.Name)

## calculate perc correct for each level and condition
va <- va %>%
  group_by(Participant.Private.ID, condition, logMAR)%>%
  summarise(totAttempt = sum(Attempt),
         totCorrect = sum(Correct))%>%
  mutate(percCorrect = totCorrect/totAttempt)%>%
  arrange(Participant.Private.ID, condition, -logMAR)


# calculate letter wise scoring
va <- va %>%
  group_by(Participant.Private.ID, condition, logMAR)%>%
  mutate(letterScore = case_when(
    percCorrect < 0.1 ~ 0,
    percCorrect >= 0.1 && percCorrect < 0.3 ~ 0.2,
    percCorrect >= 0.3 && percCorrect < 0.5 ~ 0.4,
    percCorrect >= 0.5 && percCorrect < 0.7 ~ 0.6,
    percCorrect >= 0.7 && percCorrect < 0.9 ~ 0.8,
    percCorrect >= 0.9 ~ 1.0,
    TRUE ~ percCorrect
  ))
#write_csv(va, "../cleanData/va.csv")

# calculate VA threshold using letterwise scoring
vaThresh <- va %>%
  dplyr::group_by(Participant.Private.ID, condition)%>%
  dplyr::summarise(threshold = 1.1 - 0.1*sum(letterScore, na.rm = TRUE))
                   
#write_csv(vaThresh, "../cleanData/vaThresh.csv")



##### CS #####
cs <- data %>%
  filter(display == "cs")

cs <- cs %>%
  dplyr::select(Participant.Private.ID, condition, Screen.Name, Attempt, Correct)

# revalue levels to % contrast
# level 1 = 100% contrast
cs$contrast <- 101 - as.numeric(str_remove(cs$Screen.Name, "Level"))

cs <- cs %>%
  dplyr::select(-Screen.Name)

# calculate perc correct for each level
cs <- cs %>%
  group_by(Participant.Private.ID, condition, contrast)%>%
  summarise(totAttempt = sum(Attempt),
            totCorrect = sum(Correct))%>%
  mutate(percCorrect = totCorrect/totAttempt)%>%
  arrange(Participant.Private.ID, condition, -contrast)


# splitblur and clear
csBlur <- cs %>%
  filter(condition == 'blur')
csClear <- cs %>%
  filter(condition == 'clear')
# calculate percise cs estimate

ppList <- unique(cs$Participant.Private.ID)

dfThresh <- data.frame(matrix(NA,    # Create empty data frame
                                 nrow = length(ppList),
                                 ncol = 5))


cols <- c("id",
          "thresh", "threshPerc",
          "subThresh", "subThreshPerc")

colnames(dfThresh) <- cols

rm(cols)

percThresh <- data.frame(matrix(NA,    # Create empty data frame
                                   nrow = length(ppList),
                                   ncol = 2))

for (pp in ppList){
  x <- csBlur %>%
    filter(Participant.Private.ID == pp)
  
  threshInd <- max(which(x$percCorrect > .5))
  thresh <- x$contrast[threshInd]
  threshPerc <- x$percCorrect[threshInd]
  
  if (thresh == 1){
    subThresh <- 0.0 
  }else{
    subThresh <- thresh - 1 
  }
  
  subThreshInd <- which(x$contrast == subThresh)
  
  if (subThresh == 0){
    subThreshPerc <- 0
  }else{
    subThreshPerc<- x$percCorrect[subThreshInd] 
  }
  
  percList <- seq(from = subThreshPerc, to = threshPerc, by = 0.01)
  threshList <- seq(from = subThresh, to = thresh, length.out = length(percList))
  
  interDF <- data.frame(matrix(NA,    # Create empty data frame
                               nrow = length(percList),
                               ncol = 3))
  cols <- c('id', 'percCorrect', 'thresh')
  interDF[1] <- pp
  interDF[2] <- percList
  interDF[3] <- threshList
  
  colnames(interDF) <- cols
  
  rm(cols)
  
  interDF <- interDF %>%
    filter(percCorrect > 0.5)
  
  outDF <- interDF[1,]
  
  #print(head(outDF))
  
  cols <- c('id', 'thresh')
  colnames(percThresh) <- cols
  rm(cols)
  percThresh[which(ppList == pp), 1]<-outDF[1]
  percThresh[which(ppList == pp), 2]<-outDF[3]
}



percThreshBlur <- percThresh %>%
  group_by(id)%>%
  mutate(thresh = case_when(
    thresh < 1 ~ 1,
    TRUE ~ thresh
  ))%>%
  mutate(thresh = round(thresh, 2))%>%
  mutate(cs = 2 + log10(1/thresh))

percThreshBlur <- percThreshBlur %>%
  rename(csBlur = cs) %>%
  select(-thresh) 


for (pp in ppList){
  x <- csClear %>%
    filter(Participant.Private.ID == pp)
  
  threshInd <- max(which(x$percCorrect > .5))
  thresh <- x$contrast[threshInd]
  threshPerc <- x$percCorrect[threshInd]
  
  if (thresh == 1){
    subThresh <- 0.0 
  }else{
    subThresh <- thresh - 1 
  }
  
  subThreshInd <- which(x$contrast == subThresh)
  
  if (subThresh == 0){
    subThreshPerc <- 0
  }else{
    subThreshPerc<- x$percCorrect[subThreshInd] 
  }
  
  percList <- seq(from = subThreshPerc, to = threshPerc, by = 0.01)
  threshList <- seq(from = subThresh, to = thresh, length.out = length(percList))
  
  interDF <- data.frame(matrix(NA,    # Create empty data frame
                               nrow = length(percList),
                               ncol = 3))
  cols <- c('id', 'percCorrect', 'thresh')
  interDF[1] <- pp
  interDF[2] <- percList
  interDF[3] <- threshList
  
  colnames(interDF) <- cols
  
  rm(cols)
  
  interDF <- interDF %>%
    filter(percCorrect > 0.5)
  
  outDF <- interDF[1,]
  
  #print(head(outDF))
  
  cols <- c('id', 'thresh')
  colnames(percThresh) <- cols
  rm(cols)
  percThresh[which(ppList == pp), 1]<-outDF[1]
  percThresh[which(ppList == pp), 2]<-outDF[3]
}

percThreshClear <- percThresh %>%
  group_by(id)%>%
  mutate(thresh = case_when(
    thresh < 1 ~ 1,
    TRUE ~ thresh
  ))%>%
  mutate(thresh = round(thresh, 2))%>%
  mutate(cs = 2 + log10(1/thresh))

percThreshClear <- percThreshClear %>%
  rename(csClear = cs) %>%
  select(-thresh)

csThresh <- merge(percThreshClear, percThreshBlur, by = 'id')

csThresh <- csThresh %>%
  pivot_longer(!id, names_to = "condition", names_prefix = "cs", values_to = "CS")

csThresh <- csThresh %>%
  mutate(condition = case_when(
    condition == 'Blur' ~ 'blur',
    condition == 'Clear' ~ 'clear'
  ))

write_csv(csThresh, "../cleanData/csThresh.csv")


