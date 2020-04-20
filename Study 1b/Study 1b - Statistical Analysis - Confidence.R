source("Study 1b - Prep Data.R")


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


# first, simply compare the proportions
prop.table(table(d_merge$switch, d_merge$initially_correct), margin=2)
chisq.test(table(d_merge$switch, d_merge$initially_correct))

# then, add controls for the effect of social information
summary(glm(switch ~ initially_correct + pct_agree_with_correct, family="binomial", data=d_merge))




### interesting cases
as.data.frame(empirical_sum) %>% arrange(initial_accuracy, change_accuracy) %>% 
  select(trial_id, initial_accuracy, change_accuracy)


ag[ag$trial_id==48,]

