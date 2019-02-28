rm(list=ls());gc()
library(dplyr)
library(ggplot2)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


d.raw=read.csv("continuous_choice_outcomes.csv", stringsAsFactors=F) %>%
  mutate(
    trial = as.character(trial)    
  )

d.raw = subset(d.raw, trial %in% names(which(table(d.raw$trial)==5)))

d.raw$cor = round(d.raw$cor,1)

d = d.raw %>% 
  group_by(trial, shape, network, c.quantile, t.quantile, rev.choice) %>%
  summarize(
            prob.1 = prob.correct[round==1]
          , prob.5 = prob.correct[round==5]
          , improv = prob.5 - prob.1
          )


decent_all = subset(d, network=="decentralized, k=4") %>%
  group_by(shape, network, c.quantile, t.quantile, rev.choice) %>%
  summarize(
      EN = length(prob.1)
    , accuracy = mean(prob.1)
    , initially_accurate = mean(prob.1)>0.5
    , improv = mean(improv)
)


ggplot(subset(decent_all), 
       aes(x=factor(c.quantile), y=factor(t.quantile), fill=improv, color=initially_accurate)) +
  scale_fill_gradient2(high="blue",low="red", mid="grey", midpoint=0)+
  scale_color_manual(values=c("white","grey"))+
  geom_tile() +
  guides(color=F)+
  facet_grid(rev.choice~shape) +
  beckertheme + labs(title="initial accuracy")




