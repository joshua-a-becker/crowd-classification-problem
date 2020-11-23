################################################################################

# This is the script to create Figure 3

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


#####################
# Making the figure #
#####################

# Making the figure
reanalysis %>% 
  mutate(
    accuracy_round = round(pre_influence,01)
  ) %>%
  group_by(accuracy_round, predict_amplify, dataset
           ) %>%
  summarize(
    pre_influence = mean(pre_influence)
    , post_influence = mean(post_influence)
    , change=mean(change)
  ) %>%
  ggplot(aes(x=accuracy_round, y=change, color=predict_amplify)) + 
  geom_line() +
  facet_wrap(.~dataset, scales="free_y") +
  scale_color_manual(values = c("red","black")) +
  labs(y="Change in Accuracy\n(Positive = More Accurate)", x="Initial Accuracy", color="") +
  guides(color=F) +
  #scale_y_continuous(lim=c(-0.11,0.15))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines")) +
  scale_y_continuous(lim=c(-0.5,0.5))+
  scale_x_continuous(lim=c(0,1))+
  neat_theme

# Saving the figure
ggsave("Figures/Figure 3.png", width=8.2, height=2.8)

