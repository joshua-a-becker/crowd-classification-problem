rm(list=ls());gc()
require(ggplot2)
require(dplyr)
require(xtable)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


d = read.csv("Study1bData.csv", stringsAsFactors=F) %>% mutate(
    correct_1 = response_1 == correctAnswer
  , correct_2 = response_2 == correctAnswer
  , correct_3 = response_3 == correctAnswer
  , is.valid = !is.na(response_1) & !is.na(response_2) & !is.na(response_3)
)

d$questionSet = NA
d$questionSet[grepl("cola", d$question)] = "Pepsi vs Coke"
d$questionSet[grepl("candies", d$question)] = "Candies"
d$questionSet[grepl("employ", d$question)] = "Employment"
d$questionSet[grepl("tech", d$question)] = "Technology"
d$questionSet[grepl("dessert", d$question)] = "Calories"


ag = d %>% group_by(trial_id, question, questionSet) %>%
  summarize(
                 N=length(response_1)
               , correct_1 = mean(correct_1[is.valid])
               , correct_2 = mean(correct_2[is.valid])
               , correct_3 = mean(correct_3[is.valid])
               , change_13 = correct_3 - correct_1
               )


table(ag$question)
table(ag$questionSet)

### THEORETICAL OVERLAY
outcomes=read.csv("simulation_outcomes_k10.csv")
outcomes$change_accuracy = outcomes$final_accuracy - outcomes$initial_accuracy 
outcomes$expected_accuracy = round(outcomes$expected_accuracy,2)
outcomes$did_improve = factor(outcomes$change_accuracy>0)
outcomes$maj_size = abs(0.5-outcomes$initial_accuracy)
model_sum = subset(outcomes, maj_size<0.5) %>%
  group_by(initial_accuracy) %>%
  summarize(
                sd=sd(change_accuracy)
              , top_95 = quantile(change_accuracy,probs=c(0.975))
              , bot_95 = quantile(change_accuracy,probs=c(0.025))
              , change_accuracy=mean(change_accuracy)
)



empirical_sum = ag %>% 
  group_by(correct_1, change_13, questionSet, question,trial_id) %>%
  summarize(
     change_accuracy=mean(change_13)
    , initial_accuracy = mean(correct_1)
    , final_accuracy=mean(correct_3)
    #, expected_accuracy = unique(expected_accuracy)
    , N=length(correct_1)
    )


xlabs=paste0(seq(0,100,by=025),"%")
ylabs=paste0(seq(-50,50,by=25),"%")
ggplot() + 
  geom_ribbon(data=model_sum, aes(x=initial_accuracy, ymin=bot_95, ymax=top_95), 
              fill="black", alpha=0.2) +
  geom_line(data=model_sum, color="#666666", 
            aes(x=initial_accuracy, y=change_accuracy, group=1)) + 
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
  scale_y_continuous(lim=c(-0.5,0.5), labels=ylabs, breaks=seq(-0.5,0.5,by=0.25))+
  beckertheme +
  labs(  color="", fill=""
         , x="Initial Accuracy"
         , y="Change in Accuracy\n(Positive = More Accurate)"
         , size="Number of Outcomes")
#

ggsave("Figures/Figure 2.png", width=5.5, height=2.5)

low=subset(ag, correct_1<0.5)
high=subset(ag, correct_1>0.5)
split=subset(ag, correct_1==0.5)

low_move=subset(ag, correct_1<0.5 & change_13!=0)
mean(low_move$change_13<0)
high_move=subset(ag, correct_1>0.5 & change_13!=0)
mean(high_move$change_13>0)
mean(high_move$change_13)

nrow(low)
mean(low$change_13)
mean(low$change_13>=0)
mean(low_move$change_13)

nrow(high)
mean(high$change_13)
mean(high$change_13>=0)
mean(high_move$change_13)



### LOW IMPROVED?

chisq.test(table(low$change_13[low$change_13!=0]<=0))
chisq.test(table(low$change_13<=0))
chisq.test(table(low$change_13[low$change_13!=0]<=0))
chisq.test(table(low$change_13<0))
chisq.test(table(low_move$change_13<0))
wilcox.test(low$change_13)
wilcox.test(low_move$change_13)

### HIGH IMPROVED?
chisq.test(table(high$change_13>0))
chisq.test(table(high_move$change_13>0))
wilcox.test(high$change_13)

### CONDITIONS DIFFERENT?
wilcox.test(low$change_13,
            high$change_13)


### WERE INITIALLY ACCURATE PEOPLE MORE LIKELY TO SWITCH?
d$initially_correct = d$correct_1
d$switch = d$response_1!=d$response_3
ag$pct_agree_with_correct = ag$correct_1
d_merge=merge(d, ag, by="trial_id")

prop.table(table(d_merge$switch, d_merge$initially_correct), margin=2)
chisq.test(table(d_merge$switch, d_merge$initially_correct), margin=2)

summary(glm(switch ~ initially_correct + pct_agree_with_correct, family="binomial", data=d_merge))
summary(glm(switch ~ initially_correct * pct_agree_with_correct, family="binomial", data=d_merge))

summary(glm(switch ~ initially_correct, family="binomial", data=d_merge))



ddply(d, .(question), summarize,
      correct=unique(correctAnswer))


ag$correct_1
ag$change_13
ggplot(ag, aes(x=correct_1, y=change_13)) + 
  stat_smooth(color="#e41a1c", span=0.9, size=0.5, method="loess", fill=NA) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0.5)



### descriptive stats
empirical_sum$change = "Unchanged"
empirical_sum$change[empirical_sum$change_13!=0] = 
  ifelse(empirical_sum$change_13[empirical_sum$change_13!=0]<0, "Decreased", "Increased")
empirical_sum$initially_accurate = ifelse(empirical_sum$initial_accuracy>0.5, "Accurate","Inaccurate")
empirical_sum$initially_accurate[empirical_sum$initial_accuracy==0.5]="Split"
xtab=table(empirical_sum$change, 
      empirical_sum$initially_accurate)
xtable(xtab)

### interesting cases
as.data.frame(empirical_sum) %>% arrange(initial_accuracy, change_accuracy) %>% 
  select(trial_id, initial_accuracy, change_accuracy)


ag[ag$trial_id==48,]

