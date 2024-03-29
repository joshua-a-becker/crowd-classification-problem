################################################################################

# This is the script to create Figure A4

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc()

################
# Loading data #
################

# Loading experimental data
source("Analysis/Prep main experiment data.R")

# Loading empirically calibrated simulation outcomes
fs=list.files("Simulations/Empirically Calibrated Simulations/")
outcomes=lapply(
  fs[grepl("empirical_sim",fs)],
  FUN=function(x){
    read.csv(paste0("Simulations/Empirically Calibrated Simulations/",x))
  }) %>%
  do.call(rbind, .) %>%
  mutate(
      change_accuracy = final_accuracy - initial_accuracy 
    , expected_accuracy = round(expected_accuracy,2)
    , did_improve = factor(change_accuracy>0)
    , maj_size = abs(0.5-initial_accuracy)
  )

# Summarizing the empirically calibrated simulation outcomes
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

####################
# Making Figure A4 #
####################

# Making the figure
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
    values=c("solid","dotted")
    ,labels=c("Omitted","Included")
  )+
  neat_theme

# Saving the figure
ggsave("Figures/Figure A4.png", width=5.5, height=2.5)
