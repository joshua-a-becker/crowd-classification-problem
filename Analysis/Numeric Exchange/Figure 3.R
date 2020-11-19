### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc();
library(readxl,warn.conflicts = F, quietly = T)
library(httr,warn.conflicts = F, quietly = T)
library(tidyverse)
source("dependencies.R")


d1 = read.csv("Data/gurcay_data.csv", stringsAsFactors=F) %>% 
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


lorenz_url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"
GET(lorenz_url, write_disk(tf <- "Data/lorenz_et_al.xls", overwrite=T))
# if(!file.exists("Exclude/lorenz_et_al.xls")) {
#   GET(lorenz_url, write_disk(tf <- "Exclude/lorenz_et_al.xls", overwrite=T))  
# }
d3 <- read_excel("Data/lorenz_et_al.xls") %>%
  mutate(
    pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date,Question)
    , task=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Delphi"
  ) %>% 
  subset(network=="Decentralized") %>%
  group_by(trial, task) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  ) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset")
  )


d = rbind( d1
          ,d2
          ,d3
          ) %>%
  group_by(trial) %>%
  mutate(
      mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    , improve = abs(mu2-truth) < abs(mu1-truth)
    , worse = abs(mu2-truth) > abs(mu1-truth)
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


# This takes a bit of time
reanalysis %>% 
  mutate(
    accuracy_round = round(pre_influence,01)
  ) %>%
  group_by(accuracy_round, predict_amplify#, dataset
           ) %>%
  summarize(
    pre_influence = mean(pre_influence)
    , post_influence = mean(post_influence)
    , change=mean(change)
  ) %>%
  ggplot(aes(x=accuracy_round, y=change, color=predict_amplify)) + 
  geom_line() +
  #facet_wrap(.~dataset, scales="free_y") +
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

ggsave("Figures/Figure 3a.png", width=8.2, height=2.8)



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

reanalysis %>%
  group_by(dataset,trial) %>%
  accuracy %>%
  group_by(dataset) %>%
  summarize(
     stderr = t.test(total)$stderr
    , total = mean(total)
    , acc_01 = mean(acc_01)
    , acc_05 = mean(acc_05)
    , acc_10 = mean(acc_10)
    , N=n()
  )

  

d %>% 
  group_by(trial) %>%
  summarize(improve=unique(improve)) %>%
  merge(reanalysis, by="trial") %>%
  group_by(dataset,trial,improve) %>%
  accuracy %>%
  group_by(improve, dataset) %>%
  summarize(
      stderr = t.test(total)$stderr
    , conf1 = t.test(total, conf.level=0.99)$conf.int[1]
    , conf2 = t.test(total, conf.level=0.99)$conf.int[2]
    , total = mean(total)
    , acc_01 = mean(acc_01)
    , acc_05 = mean(acc_05)
    , acc_10 = mean(acc_10)
    , N=n()
  ) %>%
  mutate(improve=ifelse(improve,"Mean\nImproved","Mean\nWorse")) %>%
  ggplot(aes(x=improve, shape=dataset,y=total)) +
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=conf1, ymax=conf2), width=0
                , position=position_dodge(0.5)) +
  labs(x="", y="% Consistent w/ Model") +
  ylim(0,1)+
  neat_theme

ggsave("Figures/Figure 3b - Prop2_Predicted_by_Improvement.png", width=4.45, height=3)




d %>% 
  group_by(trial) %>%
  summarize(improve=unique(improve)) %>%
  merge(reanalysis, by="trial") %>%
  group_by(dataset,trial) %>%
  accuracy %>%
  group_by(dataset) %>%
  summarize(
    stderr = t.test(total)$stderr
    , conf1 = t.test(total, conf.level=0.99)$conf.int[1]
    , conf2 = t.test(total, conf.level=0.99)$conf.int[2]
    , total = mean(total)
    , acc_01 = mean(acc_01)
    , acc_05 = mean(acc_05)
    , acc_10 = mean(acc_10)
    , N=n()
  ) %>%
  ggplot(aes(x=dataset,y=total)) +
  geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=conf1, ymax=conf2), width=0
                , position=position_dodge(0.5)) +
  labs(x="Prediction: Grow", y="% Accurately Predicted") +
  ylim(0,1)

