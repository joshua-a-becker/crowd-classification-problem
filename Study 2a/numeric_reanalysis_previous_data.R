rm(list=ls());gc();
library(readxl)
library(httr)
library(tidyverse)
library(ggplot2)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


d = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
              , stringsAsFactors=F) %>% 
  mutate(
    pre_influence = response_1
  , post_influence = response_3
  , question = task
  , trial=paste0(group_number, "-",question)
) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth")
  )


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
reanalysis = do.call(rbind, lapply(seq(0,1,by=0.01), function(p) {
  do.call(rbind, lapply(unique(d$trial), function(x){
    samp = subset(d, trial==x)
    cbind(  p 
            , unique(samp$trial)
            , correctAtP(samp$pre_influence, samp$pre_influence, p, unique(samp$truth))
            , correctAtP(samp$pre_influence, samp$post_influence, p, unique(samp$truth))
    )
  }))
})) %>% 
  data.frame(stringsAsFactors=F)

### RENAME COLUMNS FOR CONVENIENCE
colnames(reanalysis) = c("p","trial","pre_influence","post_influence")

### CONVERT BACK TO NUMERIC OJBJECTS
reanalysis$pre_influence=as.numeric(reanalysis$pre_influence)
reanalysis$post_influence=as.numeric(reanalysis$post_influence)
reanalysis$p = as.numeric(reanalysis$p)

### CALCULATE CHANGE
reanalysis$change = reanalysis$post_influence - reanalysis$pre_influence

### LABEL OUTCOMES
reanalysis$initially_accurate = factor(reanalysis$pre_influence > 0.5)
levels(reanalysis$initially_accurate)=c("Initially Inaccurate","Initially Accurate")

d_sum = reanalysis %>% 
  group_by(p, initially_accurate) %>%
  summarize(
        pre_influence = mean(pre_influence)
      , post_influence = mean(post_influence)
      , change=mean(change)
)


ggplot(d_sum, aes(x=p, y=change, color=initially_accurate)) + 
  geom_line() +
  #facet_grid(.~dataset) +
  beckertheme +
  scale_color_manual(values = c("black","blue")) +
  labs(y="Change in Percent Correct", x="Threshold as Percentile", color="") +
  guides(color=F) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines"))


ggsave("Reanalysis Figure - Appendix Version.png", width=3, height=3)


reanalysis$accuracy_round = round(reanalysis$pre_influence,01)
d_sum_main_text = reanalysis %>% 
  group_by(accuracy_round) %>%
  summarize(
    pre_influence = mean(pre_influence)
    , post_influence = mean(post_influence)
    , change=mean(change)
  )


ggplot(d_sum_main_text, aes(x=accuracy_round, y=change)) + 
  geom_line() +
  #facet_grid(.~dataset) +
  beckertheme +
  scale_color_manual(values = c("black","red")) +
  labs(y="Change in Accuracy\n(Positive = More Accurate)", x="Initial Accuracy", color="") +
  guides(color=F) +
  scale_y_continuous(lim=c(-0.11,0.15))+
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines"))

ggsave("Reanalysis Figure - Main Text.png", width=2.8, height=2.4)



### FIND AN ILLUSTRATIVE CASE
set="7-1b"

### check correct answer
unique(d$truth[d$trial==set])

# there are 729 calories
# what if we are interested only in foods with fewer than 600 calories?

### percent correct pre-discussion
mean(d$pre_influence[d$trial==set]<600)

### percent correct post-discussion
mean(d$post_influence[d$trial==set]<600)

### numeric accuracy pre-discussion
mean(d$pre_influence[d$trial==set])

### numeric accuracy post-discussion
mean(d$post_influence[d$trial==set])

