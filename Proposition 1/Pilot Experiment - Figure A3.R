### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc();
library(tidyverse)
library(ggplot2)

### useful tools
source("../dependencies.R")

d = read.csv("Becker Guilbeault Smith - Crowd Classification Problem - Pilot Data .csv", stringsAsFactors=F) %>%
  mutate(
    initially_correct = response_1=="More than 350"
  , switch = ifelse(response_1==response_3, "stay","switch")
  , switch12 = ifelse(response_1==response_2, "stay","switch")
  , majority_disagree = ifelse(condition<0.5, "disagree","agree")
)


### calculate the probability of switching as a function of social information
### only analyze data for subjects who responded at both Round 1 and Round 3
by_pct = d %>%
  subset(!is.na(response_1) & !is.na(response_3)) %>% 
  group_by(condition) %>% 
  summarize(
                 prob = mean(switch=="switch")
               , N = length(switch)
               , SE = sqrt((prob*(1-prob))/N)
)


### ESTIMATION OF PROBABILITY OF CHANGING BY percent agreement
xlabs=paste0(seq(10,90,by=10),"%")
ylabs=paste0(seq(0,100,by=25),"%")
ggplot(by_pct, aes(x=factor(condition), y=prob)) + 
  geom_point() + #geom_line() +
  geom_errorbar(aes(ymin=prob-SE*1.96, ymax=prob+SE*1.96), width=0) + 
  labs(x="Percent Agreeing with Subject",y="Probability of Changing")+
  scale_y_continuous(lim=c(-0.03,0.75),labels=ylabs,breaks=seq(0,1,by=0.25))+
  scale_x_discrete(labels=xlabs)+
  neat_theme + rotateX + adjX
ggsave("Figure A3.png", width=4, height=3)

