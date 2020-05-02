### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION


rm(list=ls());gc()
library(tidyverse)
source("Main Experiment - Prep Data.R")

ag = ag %>%
  mutate(
    initially_accurate = correct_1>=0.5
    , improve = change_13>0
    , abs_maj = abs(1-correct_1)
    , abs_maj_sq = abs_maj^2
  )


### THE TESTS
ANALYSIS_1 = wilcox.test(change_13 ~ initially_accurate, ag)

ANALYSIS_2 = prop.test(table(ag$improve, ag$initially_accurate))

ANALYSIS_3a = wilcox.test(ag$change_13[ag$initially_accurate])
ANALYSIS_3b = wilcox.test(ag$change_13[!ag$initially_accurate])  

ANALYSIS_4a = prop.test(table(ag$improve[ag$initially_accurate]))
ANALYSIS_4b = prop.test(table(ag$improve[!ag$initially_accurate]))

ANALYSIS_5 = lm(abs(change_13) ~ abs_maj + abs_maj_sq, ag)

ANALYSIS_1$p.value
ANALYSIS_2$p.value
ANALYSIS_3a$p.value
ANALYSIS_3b$p.value
ANALYSIS_4a$p.value
summary(ANALYSIS_5)
