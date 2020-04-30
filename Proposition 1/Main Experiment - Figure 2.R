### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc()
source("Main Experiment - Prep Data.R")


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

model_sum = subset(outcomes, maj_size<0.5) %>%
  group_by(initial_accuracy, overfit) %>%
  summarize(
      sd=sd(change_accuracy)
    , se = sd/(sqrt(n()))
    , top_95 = quantile(change_accuracy,probs=c(0.995))
    , bot_95 = quantile(change_accuracy,probs=c(0.005))
    , change_accuracy=mean(change_accuracy)
    , ci_bot = change_accuracy-se*1.96
    , ci_top = change_accuracy+se*1.96
    , N=n()
  )


xlabs=paste0(seq(0,100,by=025),"%")
ylabs=paste0(seq(-50,50,by=25),"%")
ggplot() + 
  geom_line(data=model_sum
            , color="black"
            , aes(x=initial_accuracy
                  , y=change_accuracy
                  , linetype=overfit)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") + 
  scale_size_continuous(range=c(2,4))+
  scale_x_continuous(lim=c(0,1), labels=xlabs)+
  scale_y_continuous(lim=c(-0.43,0.43), 
                     labels=ylabs, breaks=seq(-0.5,0.5,by=0.25))+
  labs(  color="", fill=""
         , linetype="Acc./Rev. Corr."
         , x="Initial Accuracy"
         , y="Change in Accuracy\n(Positive = More Accurate)"
         , size="Number of Outcomes") +
  scale_linetype_manual(
    values=c("solid","dashed")
    ,labels=c("Omitted","Included")
  )+
  neat_theme
#

ggsave("Figure A4.png", width=6.5, height=2.5)


ggplot() + 
  geom_line(data=model_sum %>% subset(overfit==F)
            , color="black"
            , aes(x=initial_accuracy, y=change_accuracy, group=1)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0.5, linetype="dashed") + 
  #geom_abline(slope=1, linetype="dashed", color="black")+
  stat_smooth(data=ag
              , aes(x=correct_1
                    , y=change_13
              ), color="#e41a1c", span=0.9, size=0.5, method="loess", fill=NA
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
  labs(  color="", fill=""
         , x="Initial Accuracy"
         , y="Change in Accuracy\n(Positive = More Accurate)"
         , size="Number of Outcomes") +
  neat_theme


ggsave("Figure 2.png", width=5.5, height=2.5)





lookup = function(x1, this_overfit){
  thisd=model_sum %>%
    subset(overfit==this_overfit)
  unlist(sapply(x1, function(x){
    thisd$change_accuracy[thisd$initial_accuracy==(round(x*2,1)/2)]
    }))
}

ag = ag %>%
  mutate(
      predict_overfit=lookup(correct_1, this_overfit=T)
    , predict_prereg=lookup(correct_1, this_overfit=F)
  )

naive_linear=lm(change_13 ~ correct_1>0.5, ag)
poly_fit = loess(change_13 ~ correct_1, data=ag, span=0.9)


SS_tot = sum((ag$change_13-mean(ag$change_13))^2)
SS_res_prereg=sum((ag$predict_prereg-ag$change_13)^2)
SS_res_overfit=sum((ag$predict_overfit-ag$change_13)^2)
SS_res_linear=sum(((ag$change_13-naive_linear$fitted.values)^2))
SS_res_poly=sum(((ag$change_13-poly_fit$fitted)^2))


R2_linear = 1 - (SS_res_linear/SS_tot)
R2_prereg = 1 - (SS_res_prereg/SS_tot)
R2_overfit = 1 - (SS_res_overfit/SS_tot)
R2_poly = 1 - (SS_res_poly/SS_tot)
