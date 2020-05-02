### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc();

library(tidyverse)
source("../dependencies.R")


d1 = read.csv("gurcay_data.csv", stringsAsFactors=F) %>% 
  mutate(
    pre_influence = est1
  , post_influence = est2
  , question = question.no
  , trial=paste0("gurcay",group, "-",question)
  , truth=true.values
  , dataset="gurcay"
) %>% 
  subset(condition!="C" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset")
  )


d2 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
             , stringsAsFactors=F) %>% 
  mutate(
    pre_influence = response_1
    , post_influence = response_3
    , question = task
    , trial=paste0("becker", group_number, "-",question)
    , dataset="becker"
  ) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset")
  )

d = rbind(d1,d2)




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
reanalysis = do.call(rbind, lapply(seq(0,1,by=0.05), function(p) {
  do.call(rbind, lapply(unique(d$trial), function(x){
    samp = subset(d, trial==x)
    
    t = distAsPercentile(samp$pre_influence, p)
    
    mu1 = mean(samp$pre_influence)
    med1 = median(samp$pre_influence)
    mu2 = mean(samp$post_influence)
    truth = unique(samp$truth)

    predict_switch = ((t<mu2) & (t>med1)) | ((t>mu2) & (t<med1))
    
    data.frame(p=p 
            , trial=x
            , pre_influence=correctAtP(samp$pre_influence, samp$pre_influence, p, unique(samp$truth))
            , post_influence = correctAtP(samp$pre_influence, samp$post_influence, p, unique(samp$truth))
            , predict_switch = predict_switch
            , dataset=unique(samp$dataset)
    )
  }))
})) %>% 
  mutate(
           switch = ((pre_influence > 0.5) & (post_influence<pre_influence)) | ((pre_influence < 0.5) & (post_influence>pre_influence))
           , change = post_influence - pre_influence
  )


reanalysis %>% 
  mutate(
    accuracy_round = round(pre_influence,01)
  ) %>%
  group_by(accuracy_round, predict_switch, dataset) %>%
  summarize(
    pre_influence = mean(pre_influence)
    , post_influence = mean(post_influence)
    , change=mean(change)
  ) %>%
  ggplot(aes(x=accuracy_round, y=change, color=predict_switch)) + 
  geom_line() +
  facet_grid(.~dataset) +
  scale_color_manual(values = c("black","red")) +
  labs(y="Change in Accuracy\n(Positive = More Accurate)", x="Initial Accuracy", color="") +
  guides(color=F) +
  #scale_y_continuous(lim=c(-0.11,0.15))+
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  theme(panel.spacing = unit(2, "lines")) +
  neat_theme

ggsave("Figure 3.png", width=6, height=3)



### STATISTICAL TEST (MODEL FIT)
mygrid=expand.grid(unique(reanalysis$trial), seq(0,0.5,by=0.05)) %>%
  data.frame %>%
  rename(
    trial=Var1
    , t=Var2
  ) %>%
  mutate(
    trial=as.character(trial)
  ) %>%
  merge(.,reanalysis, by="trial")

### statistical tests
for_stats = mygrid %>%
  group_by(t) %>%
  mutate(
      same = ((pre_influence>0.5) & (post_influence>0.5)) | ((pre_influence<0.5) & (post_influence<0.5)) 
    , bigger = (abs(post_influence-0.5) > abs(pre_influence-0.5))
    , amplify = bigger & same
    , correct = xor(predict_switch, amplify)
    , narrow = abs(p-0.5)<=t
  ) %>%
  group_by(trial, t) %>%
  summarize(
     correct_narrow = mean(correct[narrow])
    , N=sum(narrow)
    , correct = mean(correct)
  ) %>%
  group_by(t) %>%
  summarize(
      lower = try(t.test(correct_narrow)$conf.int[1], silent=T)
    , upper = try(t.test(correct_narrow)$conf.int[2], silent=T)
    , wilcox.p = try(wilcox.test(correct_narrow, mu=0.5)$p.val, silent=T)
    , correct_narrow=mean(correct_narrow)
  )


### bonus figure not shown in text
for_stats %>%
  subset(t>=0.05) %>%
  ggplot(aes(x=t, y=correct_narrow)) + 
  geom_point()+
  geom_line()+
  xlim(c(0,0.5))+
  ylim(c(0.395,0.8))+
  geom_ribbon(aes(ymin=lower,ymax=upper)
              , alpha=0.1) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  labs(x="Threshold Radius\n(From Median)",y="% Correctly Predicted") +
  neat_theme

### the model fit across all thresholds
subset(for_stats, t==0.5)

