rm(list=ls());gc()
require(ggplot2)
require(dplyr)
require(xtable)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


##############################
### PREP EXPEIRMENTAL DATA ###
##############################

d = read.csv("Becker Guilbeault Smith - Crowd Classification Problem - Study 1b - Data.csv", stringsAsFactors=F) %>%
  mutate(
    correct_1 = response_1 == correctAnswer
    , correct_2 = response_2 == correctAnswer
    , correct_3 = response_3 == correctAnswer
    , is.valid = !is.na(response_1) & !is.na(response_2) & !is.na(response_3)
  ) %>% mutate(
    questionSet = NA  
  ) %>% mutate_when(
    grepl("cola", question), list(questionSet = "Pepsi vs Coke")
    , grepl("candies", question), list(questionSet = "Candies")
    , grepl("employ", question), list(questionSet = "Employment")
    , grepl("tech", question), list(questionSet = "Technology")
    , grepl("dessert", question), list(questionSet = "Calories")
  )

### AGGREGATE BY TRIAL

ag = d %>% group_by(trial_id, question, questionSet) %>%
  summarize(
    N=length(response_1)
    , correct_1 = mean(correct_1[is.valid])
    , correct_2 = mean(correct_2[is.valid])
    , correct_3 = mean(correct_3[is.valid])
    , change_13 = correct_3 - correct_1
  )




### SUMMARIZE EMPIRICAL DATA

empirical_sum = ag %>% 
  group_by(correct_1, change_13, questionSet, question,trial_id) %>%
  summarize(
    change_accuracy=mean(change_13)
    , initial_accuracy = mean(correct_1)
    , final_accuracy=mean(correct_3)
    #, expected_accuracy = unique(expected_accuracy)
    , N=length(correct_1)
  )
