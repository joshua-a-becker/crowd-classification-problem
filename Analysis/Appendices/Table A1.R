################################################################################

# This is the script to create Table A1

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc();

# Loading dependencies
library(readxl,warn.conflicts = F, quietly = T)
library(httr,warn.conflicts = F, quietly = T)
library(tidyverse)
source("dependencies.R")

################
# Loading data #
################

source("Analysis/Load data for numeric exchange.R")

######################
# Preparing the data #
######################

### PROPORTION CORRECT SOME GIVEN THRESHOLD VALUE T
correctAtT = function(dist, t, truth) {
  mean((t<=dist) == (t<=truth))
}

### INVERSE CDF
distAsPercentile = function(dist, p) {
  as.numeric(quantile(dist, probs=seq(0,1,by=0.01))[p*100+1])
}

### % CORRECT GIVEN THRESHOLD AS PERCENTILE
correctAtP = function(dist1, dist2, p, truth) {
  t = distAsPercentile(dist1, p)
  correctAtT(dist2, t, truth)
}

### CRAWL ALONG ALL THE POSSIBLE THRESHOLD VALUES
### AND MEASURE OUTCOMES FOR EACH DATASET AND QUESTION
# (This takes some time) #
reanalysis = do.call(rbind, lapply(head(seq(0,1,by=0.01), -1)[-1], function(p) {
  do.call(rbind, lapply(unique(d$trial), function(x){
    samp = subset(d, trial==x)
    
    t = distAsPercentile(samp$pre_influence, p)
    
    mu1 = mean(samp$pre_influence)
    med1 = median(samp$pre_influence)
    mu2 = mean(samp$post_influence)
    truth = unique(samp$truth)
    
    predict_shrink = ((t<mu2) & (t>med1)) | ((t>mu2) & (t<med1))
    
    data.frame(p=p 
               , trial=x
               , pre_influence=correctAtP(samp$pre_influence, samp$pre_influence, p, unique(samp$truth))
               , post_influence = correctAtP(samp$pre_influence, samp$post_influence, p, unique(samp$truth))
               , predict_amplify = !predict_shrink
               , dataset=unique(samp$dataset)
    )
  }))
})) %>% 
  mutate( amplify = ((pre_influence > 0.5) & (post_influence>pre_influence)) | ((pre_influence < 0.5) & (post_influence<pre_influence))
          , change = post_influence - pre_influence
  )

### ACCURACY ACROSS FULL RANGE
accuracy=function(x){
  x %>%
    mutate(
      p=round(p*100) # avoid FLOP errors
    ) %>%
    summarize(
      total = mean(predict_amplify==amplify)
      , acc_01 = mean((predict_amplify==amplify)[p %in% seq(0,100,by=1)])
      , acc_05 = mean((predict_amplify==amplify)[p %in% seq(0,100,by=5)])
      , acc_10 = mean((predict_amplify==amplify)[p %in% seq(0,100,by=10)])
    )
}

####################
# Making the table #
####################

# TABLE A1
reanalysis %>%
  group_by(dataset,trial) %>%
  accuracy %>%
  group_by(dataset) %>%
  summarize(
    acc_01 = mean(acc_01)
    , acc_05 = mean(acc_05)
    , acc_10 = mean(acc_10)
  )
