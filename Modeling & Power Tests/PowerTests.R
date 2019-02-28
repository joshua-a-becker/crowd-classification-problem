rm(list=ls());gc()
library(sampling)
library(plyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

outcomes=read.csv("simulation_outcomes.csv")
outcomes=read.csv("simulation_outcomes_k10.csv")
#outcomes=read.csv("simulations_10_22.csv")


outcomes$change_accuracy = outcomes$final_accuracy - outcomes$initial_accuracy 
outcomes$expected_accuracy = round(outcomes$expected_accuracy,2)
outcomes$did_improve = factor(outcomes$change_accuracy>0)

outcomes$maj_size = abs(0.5-outcomes$initial_accuracy)

d_sum = ddply(subset(outcomes, maj_size<0.5), .(initial_accuracy), summarize
      , sd=sd(change_accuracy)
      , top_95 = quantile(change_accuracy,probs=c(0.975))
      , bot_95 = quantile(change_accuracy,probs=c(0.025))
      , change_accuracy=mean(change_accuracy)
      )


exp_d_sum = ddply(subset(outcomes, maj_size<0.5), .(expected_accuracy), summarize
              , sd=sd(change_accuracy)
              , top_95 = quantile(change_accuracy,probs=c(0.975))
              , bot_95 = quantile(change_accuracy,probs=c(0.025))
              , change_accuracy=mean(change_accuracy)
)


abs_d_sum = ddply(subset(outcomes, maj_size<0.5), .(maj_size), summarize
              , abs_top_95 = quantile(abs(change_accuracy),probs=c(0.975))
              , abs_bot_95 = quantile(abs(change_accuracy),probs=c(0.025))
              , abs_change_accuracy=mean(abs(change_accuracy))
)


quantile(subset(outcomes, initial_accuracy==0.25)$change_accuracy, probs=0.975)[1]
mean(subset(outcomes, initial_accuracy==0.25)$change_accuracy)

### GET EXPECTED EMPIRICAL DATA
d = subset(outcomes, expected_accuracy %in% c(0.3,0.5,0.7))
d.samp = d[strata(d, stratanames=c("expected_accuracy"), c(20,20,20), method=c("srswor"))$ID_unit,]
d.samp.sum=ddply(d.samp, .(initial_accuracy, change_accuracy, expected_accuracy), summarize
                 , N = length(initial_accuracy))

abs.d.samp.sum=ddply(d.samp, .(maj_size, change_accuracy, expected_accuracy), summarize
                 , N = length(initial_accuracy))

### POWER FIGURE:
### CHANGE IN ACCURACY AS A RESULT IF -EXPECTED- INITIAL ACCURACY
xlabs=paste0(seq(0,100,by=025),"%")
ylabs=paste0(seq(-50,50,by=25),"%")
ggplot(exp_d_sum, aes(x=expected_accuracy, y=change_accuracy)) +
  geom_ribbon(aes(ymin=bot_95, ymax=top_95), 
              fill="black", alpha=0.2) +
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept=0, linetype="dashed") + 
  scale_x_continuous(lim=c(0,1), labels=xlabs)+
  scale_y_continuous(lim=c(-0.5,0.5), labels=ylabs, breaks=seq(-0.5,0.5,by=0.25))+
  beckertheme +
  labs(  color="Empirical Data\n(Expected Accuracy)"
         , x="Initial Accuracy (Actual)"
         , y="Change in Pct. Accuracy\n(Positive = More Accurate)"
         , size="Number of Outcomes")



### MAIN FIGURE:
### CHANGE IN ACCURACY AS A RESULT OF INITIAL ACCURACY

xlabs=paste0(seq(0,100,by=025),"%")
ylabs=paste0(seq(-50,50,by=25),"%")
ggplot() +
  geom_ribbon(data=d_sum, aes(x=initial_accuracy, ymin=bot_95, ymax=top_95), 
              fill="black", alpha=0.2) +
  #geom_point(data=d_sum, aes(x=initial_accuracy, y=change_accuracy, group=1, color="black")) + 
  geom_line(data=d_sum, aes(x=initial_accuracy, y=change_accuracy, group=1)) + 
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_point(  data=d.samp.sum%>%arrange(desc(N))
               , aes(  x=initial_accuracy
                     , y=change_accuracy
                     , color=factor(expected_accuracy)
                     , size=N)
               ) +
  scale_size_continuous(range=c(2,4))+
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")
                     , labels=c("30%","50%","70%"))+
  scale_x_continuous(lim=c(0,1), labels=xlabs)+
  scale_y_continuous(lim=c(-0.5,0.5), labels=ylabs, breaks=seq(-0.5,0.5,by=0.25))+
  beckertheme +
  labs(  color="Empirical Data\n(Expected Accuracy)"
       , x="Initial Accuracy (Actual)"
       , y="Change in Pct. Accuracy\n(Positive = More Accurate)"
       , size="Number of Outcomes")
#ggsave("Model Figures/main_fig_left.png", width=7, height=3)

### SECONDARY FIGURE
### CHANGE IN ACCURACY AS FUNCTION OF SIZE OF MAJORITY

xlabs=paste0(seq(0,50,by=10),"%")
ylabs=paste0(seq(0,50,by=10),"%")
ggplot() +
  geom_ribbon(data=abs_d_sum, aes(x=maj_size, ymin=abs_bot_95, ymax=abs_top_95), 
              fill="black", alpha=0.2) +
  #geom_point(data=d_sum, aes(x=initial_accuracy, y=change_accuracy, group=1, color="black")) + 
  geom_line(data=abs_d_sum, aes(x=maj_size, y=abs_change_accuracy, group=1)) + 
  geom_hline(yintercept=0, linetype="dashed") + 
  geom_point(  data=abs.d.samp.sum%>%arrange(desc(N))
               , aes(  x=maj_size
                     , y=abs(change_accuracy)
                     , color=factor(expected_accuracy)
                     , size=N)
               ) +
  scale_size(range=c(2,4))+
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")
                     , labels=c("30%","50%","70%"))+
  scale_x_continuous(lim=c(0,0.5), labels=xlabs)+
  scale_y_continuous(lim=c(0,0.5), labels=ylabs, breaks=seq(0,0.5,by=0.1))+
  beckertheme +
  labs(  color="Empirical Data\n(Expected Accuracy)"
       , size="Number of Outcomes"
       , x="Initial Size of Majority"
       , y="| Change in Accuracy |\n(Abs. Value)"
         )
#ggsave("Model Figures/main_fig_right.png", width=7, height=3)

two_samp = subset(d.samp, expected_accuracy%in%c(0.3,0.7))

d_sum = summarySE(d.samp, groupvars=c("expected_accuracy"), measurevar="change_accuracy")
ggplot(d_sum, aes(x=factor(expected_accuracy), y=change_accuracy)) +
  geom_point() +
  geom_errorbar(aes(ymin=change_accuracy-ci, ymax=change_accuracy+ci), width=0) +
  scale_y_continuous(  lim=c(-0.5,0.5)
                     , breaks=seq(-0.5,0.5,by=0.25)
                     , labels=paste0(seq(-50,50,by=25),"%")) + 
  scale_x_discrete(labels=c("30%","50%","70%")) +
  geom_hline(yintercept=0, linetype="dashed") + 
  labs(  x="Expected Accuracy\n(Experimental Condition"
       , y="Change in Pct. Accuracy\n(Positive = More Accurate)") +
  beckertheme
#ggsave("Main Experimental Figure.png", width=3, height=3)


d.samp = d[strata(d, stratanames=c("expected_accuracy"), c(20,20,20), method=c("srswor"))$ID_unit,]

wilcox.test(d.samp$change_accuracy[d.samp$expected_accuracy==0.5],
            d.samp$change_accuracy[d.samp$expected_accuracy==0.3])

wilcox.test(d.samp$change_accuracy[d.samp$expected_accuracy==0.5],
            d.samp$change_accuracy[d.samp$expected_accuracy==0.7])

wilcox.test(d.samp$change_accuracy[d.samp$expected_accuracy==0.5])



### POWER TEST FOR MAIN ANALYSIS
initially_inaccurate = subset(outcomes, expected_accuracy==0.3)
initially_accurate = subset(outcomes, expected_accuracy==0.7)
initially_middle = subset(outcomes, expected_accuracy==0.5)

boots=1000
power_test = data.frame(
  sample_size=numeric()
  , one.wilcox.p = numeric()
  , one.prop.p = numeric()
  , compare.wilcox.p = numeric()
  , compare.prop.p = numeric()
  , compare.low.middle = numeric()
  , compare.high.middle = numeric()
)



total = 5*boots
pb=txtProgressBar(0,total)
for(samp_size in c(5,10,15,20,30)) {
  for(i in 1:boots) {
    samp_accurate = initially_accurate[sample(nrow(initially_accurate), samp_size, replace=T),]
    samp_inaccurate = initially_inaccurate[sample(nrow(initially_inaccurate), samp_size, replace=T),]
    samp_middle = initially_middle[sample(nrow(initially_middle), samp_size, replace=T),]
    
    comp_tab=rbind(samp_accurate, samp_inaccurate)
    new_row = data.frame(
      sample_size = samp_size
      , one.wilcox.p = wilcox.test(samp_accurate$change_accuracy)$p.val
      , one.prop.p = prop.test(table(samp_accurate$did_improve))$p.val
      , compare.wilcox.p = wilcox.test(samp_accurate$change_accuracy, samp_inaccurate$change_accuracy)$p.val
      , compare.prop.p = fisher.test(table(comp_tab$expected_accuracy, comp_tab$change_accuracy<0))$p.val
      , compare.low.middle = wilcox.test(samp_accurate$change_accuracy, samp_middle$change_accuracy)$p.val
      , compare.high.middle = wilcox.test(samp_middle$change_accuracy, samp_inaccurate$change_accuracy)$p.val
    )
    power_test[nrow(power_test)+1,]=new_row
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
}


power_sum = melt(power_test, id.vars="sample_size")


power_outcomes = ddply(power_sum, .(variable, sample_size),  summarize,
                       prop_sig_05 = mean(value<0.05)
                       ,prop_sig_01 = mean(value<0.01)
)

names(power_outcomes)[1]=c("test")
power_sum = melt(power_outcomes, id.vars=c("sample_size","test"))


ggplot(power_sum %>% subset(sample_size>10)
       , aes(x=sample_size, y=value, color=variable)) + 
  geom_point() +
  facet_grid(.~test)+ beckertheme
