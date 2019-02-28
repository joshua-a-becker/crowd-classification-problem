rm(list=ls());gc();
library(dplyr)
library(ggplot2)

### useful tools
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

d = read.csv("study1a_dataset.csv", stringsAsFactors=F) %>%
  mutate(
    initially_correct = response_1=="More than 350"
  , switch = ifelse(response_1==response_3, "stay","switch")
  , switch12 = ifelse(response_1==response_2, "stay","switch")
  , majority_disagree = ifelse(condition<0.5, "disagree","agree")
)

### only analyze data for subjects who responded at both Round 1 and Round 3
d_valid = subset(d, !is.na(response_1) & !is.na(response_3))

### calculate the probability of switching as a function of social information
by_pct = d_valid %>% 
  group_by(condition) %>% 
  summarize(
                 prob = mean(switch=="switch")
               , N = length(switch)
               , SE = sqrt((prob*(1-prob))/N)
)

### crossed with accuracy
by_pct_accuracy = d_valid %>% 
  group_by(condition, initially_correct) %>% 
  summarize(
    prob = mean(switch=="switch")
    , N = length(switch)
    , SE = sqrt((prob*(1-prob))/N)
  )



### ESTIMATION OF PROBABILITY OF CHANGING BY pctAgree
xlabs=paste0(seq(10,90,by=10),"%")
ylabs=paste0(seq(0,100,by=25),"%")
ggplot(by_pct, aes(x=factor(condition), y=prob)) + 
  geom_point() + #geom_line() +
  geom_errorbar(aes(ymin=prob-SE*1.96, ymax=prob+SE*1.96), width=0) + 
  labs(x="Percent Agreeing with Subject",y="Probability of Changing")+
  scale_y_continuous(lim=c(-0.03,0.75),labels=ylabs,breaks=seq(0,1,by=0.25))+
  scale_x_discrete(labels=xlabs)+
  beckertheme + rotateX + adjX
ggsave("Study 1a - Figure 1.png", width=4, height=3)

### ESTIMATION OF PROBABILITY OF CHANGING BY pctAgree & initially_correct
ggplot(by_pct_accuracy, aes(x=factor(condition), y=prob, 
                            group=initially_correct,color=initially_correct)) + 
  geom_point() +geom_line() +
  geom_errorbar(aes(ymin=prob-SE*1.96, ymax=prob+SE*1.96), width=0) + 
  labs(x="Percent Agreeing with Subject",y="Probability of Changing")+
  #scale_y_continuous(lim=c(-0.03,0.75),labels=ylabs,breaks=seq(0,1,by=0.25))+
  scale_x_discrete(labels=xlabs)+
  beckertheme + rotateX + adjX
ggsave("Study 1a - Figure 1b - Not Interesting.png", width=4, height=3)


### 2 SAMPLE TEST: PROBABILITY OF CHANGING BY MAJORITY/MINORITY
xtab = table(d_valid$switch[d_valid$condition!="0.5"], 
             d_valid$majority_disagree[d_valid$condition!="0.5"])
mean(d_valid$switch[d_valid$condition=="0.5"]=="switch")

prop.table(xtab, margin=2)
fisher.test(xtab)



### LOGISTIC REGRESSION, CONTROLLING FOR ACCURACY & pctAgree
mod1 = glm(switch=="switch" ~ initially_correct
           , d_valid
           , family="binomial")
mod2 = glm(switch=="switch" ~ condition + initially_correct
           , d_valid
           , family="binomial")
summary(mod1)
summary(mod2)

table(d_valid$initially_correct)
xtab=table(d_valid$switch, d_valid$initially_correct)
prop.table(xtab, margin=2)
prop.table(xtab2, margin=2)

fisher.test(xtab)

### FISHER TEST ON PROB OF SWITCHING BY AGREE
### 2 SAMPLE TEST: PROBABILITY OF CHANGING BY MAJORITY/MINORITY
xtab = table(d_valid$switch, d_valid$initially_correct)
prop.table(xtab, margin=2)
fisher.test(xtab)


### ESTIMATION OF PROBABILITY OF CHANGING BY pctAgree
xlabs=paste0(seq(10,90,by=10),"%")
ylabs=paste0(seq(0,100,by=25),"%")
ggplot(by_pct, aes(x=factor(pctAnswer), y=prob)) + 
  geom_point() + geom_errorbar(aes(ymin=prob-SE*1.96, ymax=prob+SE*1.96), width=0) + 
  labs(x="Percent Agreeing with Subject",y="Probability of Changing")+
  scale_y_continuous(lim=c(-0.03,0.75),labels=ylabs,breaks=seq(0,1,by=0.25))+
  scale_x_discrete(labels=xlabs)+
  beckertheme + rotateX + adjX
ggsave("Main Pilot Figure.png", width=4, height=3)


### OF PPL IN 50/50 CONDITION... DOES ACCURACY MATTER?
# pct_50 = subset(d_valid, pctAnswer==0.5)
# fisher.test(table(pct_50$initially_correct, pct_50$switch))
### DOESN'T WORK B/C NOBODY IN 50/50 SWITCHED!

### DID WE GET 50/50 AROUND THE MEDIAN?
xtab_choice_question = table(byround$response_1)
prop.test(xtab_choice_question)

