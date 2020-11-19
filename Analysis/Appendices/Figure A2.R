################################################################################

# This is the script to create Figure A2

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc()

# Loading dependencies
library(tidyverse)
source("dependencies.R")

################
# Loading data #
################

# Loading the numeric simulations data
fs=list.files("Simulations/Numeric Simulations/")
outcomes=lapply(
  fs[grepl("numsim",fs)],
  FUN=function(x){
    read.csv(paste0("Simulations/Numeric Simulations/",x))
  }) %>%
  do.call(rbind, .)

table(outcomes$R, outcomes$N)

#######################################################
# Creating variables from the data to make the figure #
#######################################################

### CONVENIENCE FUNCTION--
### IS MAJORITY ENHANCED?
maj_enhanced = function(p1, p2) {
  # 1.  value is further from 0.5
  # 2.  sign is the same
  sign(p1-0.5)==sign(p2-0.5) &
    abs(p1-0.5)<abs(p2-0.5)
}

pred_enhance = function(M,C,t){
  #C<T<M or M<T<C
  return( ! ((C<t & t<M) | (M<t & t<C)) )
}

outcomes$maj_enhanced = maj_enhanced(outcomes$pre_vote, outcomes$post_vote)
outcomes$pred_enhance = pred_enhance(outcomes$M, outcomes$C, outcomes$t_num)
outcomes$correct = outcomes$pred_enhance == outcomes$maj_enhanced

N.labs <- c("100", "1000")
names(N.labs) <- c("N=100", "N=1000")

#####################
# Making the figure #
#####################

# Making the figure
outcomes %>% 
  group_by(t_pct, pred_enhance) %>%
  subset(B_func=="norm") %>%
  subset(R==10) %>%
  group_by(  R
             , t_pct
             , N
             , B_func
  ) %>%
  summarize(
    correct=mean(correct)
  ) %>%
  mutate(
    EN=paste0("N=",N)
  ) %>%
  ggplot(aes(x=t_pct, y=correct*1)) +
  geom_point() + geom_line() +
  labs(color="Predicted to Grow?"
       , x="Threshold (Percentile)"
       , y="% Predicted Correctly") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  scale_y_continuous(#breaks=seq(0,1,by=0.2),
    lim=c(0,1)
  ) +
  facet_grid(.~EN) +
  neat_theme

# Saving the figure
ggsave("Figures/Figure A2.png"
       , width=4, height=2.4)
