### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc();
library(tidyverse)
library(ggplot2)

### useful tools
source("../dependencies.R")

d = read.csv("Becker Guilbeault Smith - Crowd Classification Problem - Pilot Data .csv", stringsAsFactors=F) %>%
  mutate(
    initially_correct = response_1=="More than 350"
  , switch = ifelse(response_1==response_3, "stay","switch")
  , switch12 = ifelse(response_1==response_2, "stay","switch")
  , majority_disagree = ifelse(condition<0.5, "disagree","agree")
)

### THE TESTS

ANALYSIS_1 = glm(switch=="switch" ~ initially_correct + majority_disagree, d, family="binomial")

ANALYSIS_2 = glm(switch=="switch" ~ initially_correct + condition, d, family="binomial")

#ANALYSIS_3 = see figure A3

summary(ANALYSIS_1)
summary(ANALYSIS_2)
