rm(list=ls());gc()
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

##################
### SETUP DATA ###
##################

### Download data from PNAS server
d=read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv"))



### FOR EACH QUESTION, GO OVER THE RANGE OF THRESHOLDS AND ASK:
### How many people would get the correct over/under for this threshold?

d_q = subset(d, task=="1a")

t_set=as.numeric(quantile(d_q$response_1, probs=seq(0,1,by=0.01)))
t = t_set[1]

### If THEIR RESPONSE IS ON THE SAME SIDE OF TRUTH AS t
### Then they give the right discrete choice answer.
by_threshold=data.frame(   q=q
                         , t=t_set
                         , prop_correct=sapply(t_set, function(t){ 
                             mean((d_q$response_1 < t) == (d_q$truth<t))
                           }))

### CLEARLY, WE CAN GET A LOT OF PEOPLE TO BE WRONG.
ggplot(by_threshold, aes(x=t, y=prop_correct)) + geom_line()

### THE THING THAT MATTERS IS HOW MANY PPL ARE BETWEEN THE MEDIAN AND TRUTH
mean((d_q$response_1>median(d_q$response_1)) & (d_q$response_1<d_q$truth))

### THE ONLY THING THAT MATTERS IS HOW MANY PEOPLE ARE
### on the opposite side of the truth from the median
mean(d_q$response_1>d_q$truth)
min(by_threshold$prop_correct)

### A FUNCTION TO CALCULATE THAT:
calcPropCorrect = function(distribution, truth) {
  distribution=distribution[!is.na(distribution)]
  if(median(distribution)<truth) {
    mean(distribution>truth)
  } else {
    mean(distribution<truth)
  }
}


worstCaseByQuestion = ddply(d, .(task), summarize
      , err_med = (median(response_1, na.rm=T)/unique(truth))-1
      , max_prop_wrong = 1-calcPropCorrect(response_1, unique(truth))
      )


### THE MAX-PROP-WRONG INCREASES WITH ERROR
ggplot(worstCaseByQuestion, aes(x=abs(err_med), y=max_prop_wrong)) +
  geom_point() + beckertheme

hist(worstCaseByQuestion$max_prop_wrong)

1.25-1
0.5-1


