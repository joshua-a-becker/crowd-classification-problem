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

model_sum = subset(outcomes, maj_size<0.5) %>%
  group_by(initial_accuracy) %>%
  summarize(
    sd=sd(change_accuracy)
    , top_95 = quantile(change_accuracy,probs=c(0.975))
    , bot_95 = quantile(change_accuracy,probs=c(0.025))
    , change_accuracy=mean(change_accuracy)
  )

lookup = function(x1){
  unlist(sapply(x1, function(x){model_sum$change_accuracy[model_sum$initial_accuracy==(round(x*2,1)/2)]}))
}

ag = ag %>%
  mutate(
    predict=lookup(correct_1)
  )

SS_tot = sum((ag$change_13-mean(ag$change_13))^2)
SS_reg=sum((ag$predict-mean(ag$change_13))^2)

R2 = SS_reg/SS_tot

ggplot(ag, aes(y=change_13,x=correct_1)) +
         geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0.5)
       
mod=lm(change_13 ~ correct_1, ag)

sum((mod$fitted.values-mean(ag$change_13))^2)
sum((ag$change_13-mean(ag$change_13))^2)

summary(mod)

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

