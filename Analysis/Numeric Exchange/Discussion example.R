################################################################################

# This is the script to recreate the example about the calories task in the 
# discussion of the numeric exchange

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc();

# Loading dependencies
library(tidyverse)

################
# Loading data #
################

# Loading the Becker et al. dataset
d2 <-  read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
              , stringsAsFactors=F) %>% 
  mutate(
    pre_influence = response_1
    , post_influence = response_3
    , trial=paste0("becker", group_number, "-",task)
    , dataset="becker"
  ) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset","task")
  )

######################
# Preparing the data #
######################

# Creating the whether participants would have theoretically said "yes" to
# whether or not there were fewer than 600 calories in the meal
d2 <- d2 %>% mutate(
  initial_yes = pre_influence < 600
  , final_yes = post_influence < 600
)

###########
# Results #
###########

# Proportions of participants who would have initially said yes
prop.table(table(d2$initial_yes[d2$task == "1b"]))[2]

# Proportions of participants who would have finally said yes
prop.table(table(d2$final_yes[d2$task == "1b"]))[2]
