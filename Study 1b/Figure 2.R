rm(list=ls());gc()
source("Study 1b - Prep Data.R")


### LOAD SIMULATION OUTCOMES
fs=list.files("Empirically Calibrated Simulations")
outcomes=lapply(
  fs[grepl("empirical_sim",fs)],
  FUN=function(x){
    read.csv(paste0("Empirically Calibrated Simulations/",x))
  }) %>%
  do.call(rbind, .) %>%
  mutate(
      change_accuracy = final_accuracy - initial_accuracy 
    , expected_accuracy = round(expected_accuracy,2)
    , did_improve = factor(change_accuracy>0)
    , maj_size = abs(0.5-initial_accuracy)
  )


### SUMMARIZE THEORETICAL DATA

model_sum = outcomes %>%
  subset(overfit=F & maj_size<0.5) %>%
  group_by(initial_accuracy) %>%
  summarize(
    sd=sd(change_accuracy)
    , top_95 = quantile(change_accuracy,probs=c(0.995))
    , bot_95 = quantile(change_accuracy,probs=c(0.005))
    , change_accuracy=mean(change_accuracy)
  )


xlabs=paste0(seq(0,100,by=025),"%")
ylabs=paste0(seq(-50,50,by=25),"%")
ggplot() + 
  #geom_ribbon(data=model_sum, aes(x=initial_accuracy, ymin=bot_95, ymax=top_95), 
  #            fill="black", alpha=0.2) +
  geom_line(data=model_sum, color="black", 
            aes(x=initial_accuracy, y=change_accuracy, group=1)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") + 
  #geom_abline(slope=1, linetype="dashed", color="black")+
  stat_smooth(data=ag
              , aes(x=correct_1
                    , y=change_13
              ), color="#e41a1c", span=1, size=0.5, method="loess", fill=NA
  ) +
  geom_point(data=empirical_sum %>% arrange(desc(N))
             , aes(x=initial_accuracy
                   , y=change_accuracy
                   , fill=factor(questionSet)
                   #  , size=N
             ), size=2, shape=21
  ) +
  scale_size_continuous(range=c(2,4))+
  #scale_color_manual(values=c("#377eb8","#4daf4a","#984ea3","#a65628"))+
  scale_x_continuous(lim=c(0,1), labels=xlabs)+
  scale_y_continuous(lim=c(-0.43,0.43), 
                     labels=ylabs, breaks=seq(-0.5,0.5,by=0.25))+
  beckertheme +
  labs(  color="", fill=""
         , x="Initial Accuracy"
         , y="Change in Accuracy\n(Positive = More Accurate)"
         , size="Number of Outcomes")
#

ggsave("Figure 2.png", width=5.5, height=2.5)




##########
## GOODNESS OF FIT
#############


lookup = function(x1){
  unlist(sapply(x1, function(x){model_sum$change_accuracy[model_sum$initial_accuracy==(round(x*2,1)/2)]}))
}

ag = ag %>%
  mutate(
    predict=lookup(correct_1)
  )

SS_tot = sum((ag$change_13-mean(ag$change_13))^2)
SS_res=sum((ag$predict-ag$change_13)^2)

R2 = SS_res/SS_tot

fit_plot = ggplot(ag, aes(y=change_13,x=correct_1)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0.5)

#fitplot


get_R2= function(span){
  sapply(span, function(sp){
    lmod = loess(change_13 ~ correct_1, data=ag, span=sp)
    R2=sum((lmod$fitted-mean(ag$change_13))^2)/sum((ag$change_13-mean(ag$change_13))^2)
    R2    
  })%>%unlist
}

get_R2(0.8)

summary(mod)$r.squared

summary(lmod)
