library(readxl)
library(httr)
library(dplyr)
library(ggplot2)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


d1 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
              , stringsAsFactors=F)

url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"


GET(url, write_disk(tf <- "lorenz_et_al.xls"))
d2 <- read_excel(tf) %>% subset(Information_Condition!="no")


### MAKE COMPATIBLE VARIABLE NAMES
### ACROSS THE TWO SETS
d1$pre_influence = d1$response_1
d1$post_influence = d1$response_3
d1$question = paste0("becker",d1$task)
d1$dataset = "Becker et al"

d2$pre_influence = d2$E1
d2$post_influence = d2$E5
d2$question = paste0("lorenz",d2$Question)
d2$group_number = d2$Session_Date
d2$truth=d2$Truth
d2$dataset = "Lorenz et al"

d = rbind(
    d1[d1$network=="Decentralized",c("group_number","question","pre_influence","post_influence","truth","dataset")]
  , d2[,c("group_number","question","pre_influence","post_influence","truth","dataset")]
) %>% 
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>% 
  mutate(
    trial = paste0(group_number, question)
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

d2$trial=paste0(d2$Session_Date, d2$question)
table(d2$trial)
t=unique(d2$trial)[1]
dist1 = d$pre_influence[d$trial==t]
dist2 = d$post_influence[d$trial==t]

unique(d$truth[d$trial==t])
d2$trial = paste0(d2$Session_Date,d2$question)
unique(d2$Information_Condition[d2$trial==t])

sort(dist1)
sort(dist2)

median(dist1)
median(dist2)
mean(dist1)
mean(dist2)

mean(dist1>=320)
mean(dist2>=320)

correctAtT(dist1, 200, 734)
correctAtT(dist2, 200, 734)
correctAtP(dist1, dist2, 0.4, 734)





samp=subset(d, trial=="081117_1016lorenz5")
p=0.83

correctAtP(samp$pre_influence, samp$pre_influence, p, unique(samp$truth))
correctAtP(samp$pre_influence, samp$post_influence, p, unique(samp$truth))
correctAtT(samp$pre_influence, 1518.39, unique(samp$truth))
correctAtT(samp$post_influence, 1518.39, unique(samp$truth))

t=1518
dist = samp$post_influence
truth=unique(samp$truth)
mean((t<=dist) == (t<=truth))

sort(dist)

### CRAWL ALONG ALL THE POSSIBLE THRESHOLD VALUES
### AND MEASURE OUTCOMES FOR EACH DATASET AND QUESTION
reanalysis = do.call(rbind, lapply(seq(0,1,by=0.01), function(p) {
  do.call(rbind, lapply(unique(d$trial), function(x){
    samp = subset(d, trial==x)
    cbind(  p 
            , unique(samp$dataset)
            , unique(samp$question)
            , unique(samp$group_number)
            , correctAtP(samp$pre_influence, samp$pre_influence, p, unique(samp$truth))
            , correctAtP(samp$pre_influence, samp$post_influence, p, unique(samp$truth))
    )
  }))
})) %>% 
  data.frame(stringsAsFactors=F)

colnames(reanalysis) = c("p","dataset","question","trial","pre_influence","post_influence")

reanalysis$pre_influence=as.numeric(reanalysis$pre_influence)
reanalysis$post_influence=as.numeric(reanalysis$post_influence)
reanalysis$p = as.numeric(reanalysis$p)
reanalysis$change = reanalysis$post_influence - reanalysis$pre_influence
reanalysis$initially_accurate = factor(reanalysis$pre_influence > 0.5)
levels(reanalysis$initially_accurate)=c("Initially Inaccurate","Initially Accurate")

d_sum = reanalysis %>% 
  group_by(dataset, p, initially_accurate) %>%
  summarize(
        pre_influence = mean(pre_influence)
      , post_influence = mean(post_influence)
      , change=mean(change)
)


ggplot(d_sum %>% subset(dataset=="Becker et al"), 
       aes(x=p, y=change, color=initially_accurate)) + 
  geom_line() +
  #facet_grid(.~dataset) +
  beckertheme +
  scale_color_manual(values = c("black","red")) +
  labs(y="Change in Percent Correct", x="Threshold as Percentile", color="") +
  guides(color=F) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines"))


#ggsave("Reanalysis Figure.png", width=3, height=3)


ggplot(reanalysis %>% subset(dataset=="Lorenz et al"), 
       aes(x=p, y=change, color=initially_accurate)) + 
  geom_line() +
  facet_grid(question~trial) +
  beckertheme +
  scale_color_manual(values = c("black","red")) +
  labs(y="Change in Percent Correct", x="Threshold as Percentile", color="") +
  guides(color=F) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines"))


### show that mean improves

ag = d %>% group_by(trial,dataset, question, truth) %>%
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
big_improv = ag[ag$chg_err_pct<(-1*0.9),]

reanalysis$trial_set=paste0(reanalysis$trial,reanalysis$question)
check = subset(reanalysis, trial_set %in% big_improv$trial)

check$change = check$post_influence - check$pre_influence

cases=check[check$change<(-0.2),]



