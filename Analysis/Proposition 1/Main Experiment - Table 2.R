### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc()
library(tidyverse)
source("Main Experiment - Prep Data.R")

### descriptive stats
xtab=empirical_sum %>%
  mutate(
      change = "Unchanged" 
    , initially_accurate = ifelse(initial_accuracy>0.5, "Accurate","Inaccurate")
  ) %>% mutate_when(
      change_13!=0, list(change=ifelse(change_13[change_13!=0]<0, "Decreased", "Increased"))
    , empirical_sum$initial_accuracy==0.5, list(initially_accurate="Split")
  ) %>% {table(.$change, .$initially_accurate)}


### view table
print(xtab[c("Increased","Decreased","Unchanged"),])

### output LaTeX table
xtable(xtab[c("Increased","Decreased","Unchanged"),])
