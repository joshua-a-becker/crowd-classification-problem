rm(list=ls());gc();
library(readxl)
library(httr)
library(dplyr)
library(ggplot2)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


d = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
              , stringsAsFactors=F) %>% 
  mutate(
    pre_influence = response_1
  , post_influence = response_3
  , question = paste0("becker",task)
  , trial=paste0(group_number, question)
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


colnames(reanalysis) = c("p","trial","pre_influence","post_influence")

reanalysis$pre_influence=as.numeric(reanalysis$pre_influence)
reanalysis$post_influence=as.numeric(reanalysis$post_influence)
reanalysis$p = as.numeric(reanalysis$p)
reanalysis$change = reanalysis$post_influence - reanalysis$pre_influence
reanalysis$initially_accurate = factor(reanalysis$pre_influence > 0.5)
levels(reanalysis$initially_accurate)=c("Initially Inaccurate","Initially Accurate")

reanalysis$accuracy_round = round(reanalysis$pre_influence,01)

d_sum = reanalysis %>% 
  group_by(accuracy_round) %>%
  summarize(
        pre_influence = mean(pre_influence)
      , post_influence = mean(post_influence)
      , change=mean(change)
)


ggplot(d_sum, aes(x=accuracy_round, y=change)) + 
  geom_line() +
  #facet_grid(.~dataset) +
  beckertheme +
  scale_color_manual(values = c("black","red")) +
  labs(y="Change in Accuracy\n(Positive = More Accurate)", x="Initial Accuracy", color="") +
  guides(color=F) +
  scale_y_continuous(lim=c(-0.11,0.15))+
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines"))


ggsave("Reanalysis Figure - Alternative X Axis.png", width=2.8, height=2.4)

i=0

i=i+1
tr=unique(reanalysis$trial)[i]
ggplot(subset(reanalysis,trial==tr), aes(x=accuracy_round, y=change)) + 
  stat_summary(fun.y="mean", geom="line")+
  geom_vline(xintercept=0.4, linetype="dashed")+
  geom_vline(xintercept=0.5)+
  beckertheme +
  xlim(c(0,1))+
  labs(y="Change in Accuracy\n(Positive = More Accurate)", x="Initial Accuracy", color="") +
  guides(color=F) +
  scale_y_continuous(lim=c(-0.11,0.15))+
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines"))



### show that mean improves

ag = d %>% group_by(trial) %>%
  summarize(
      pre_influence = mean(pre_influence)
    , post_influence = mean(post_influence)
    , pre_influence_err = abs(pre_influence-unique(truth))
    , pre_influence_err_pct = abs(pre_influence-unique(truth))/unique(truth)
    , post_influence_err = abs(post_influence-unique(truth))
    , chg_err = post_influence_err - pre_influence_err
    , chg_err_pct = (post_influence_err - pre_influence_err)/pre_influence_err
    , improve=chg_err<0
  )


ag$err_pct = round(ag$pre_influence_err_pct,1)
ag$err_pct[ag$err_pct>1]=1
ag_sum_err = summarySE(ag, measurevar="improve", groupvars=c("err_pct"))

ggplot(ag_sum_err, aes(x=err_pct, y=improve)) + 
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=improve-ci, ymax=improve+ci))

summary(glm(improve ~ dataset, ag, family="binomial"))

summary(glm(improve ~ question, subset(ag, dataset=="Lorenz et al"), 
            family="binomial"))

prop.test(table(ag$improve[ag$dataset=="Lorenz et al"]))



### find an interesting case
big_improv = ag[ag$chg_err_pct<(-0.1),]

reanalysis$trial_set=paste0(reanalysis$trial,reanalysis$question)
check = subset(reanalysis, trial_set %in% big_improv$trial)
cases=check[check$change<(-0.1),]

table(cases$trial)

set="7becker1b"

truth=unique(d$truth[d$trial==set]) #truth

dist1=d$pre_influence[d$trial==set]
dist2=d$post_influence[d$trial==set]

mean(dist1<600)
mean(dist2<600)

mean(dist1)
mean(dist2)

(err2 - err1)/err1

cases[cases$trial==set,]

quantile(dist1, probs=seq(0,1,by=0.01))
