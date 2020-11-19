################################################################################

# This is the script for calculating the goodness of fit of the different models
# for binary exchange

################################################################################

###############
# Preparation #
###############

# Cleaning the environment
rm(list=ls());gc()

################
# Loading data #
################

# Empirical data
source("Analysis/Prep main experiment data.R")

# Outcomes of the Empirically Calibrated Simulations
fs=list.files("Simulations/Empirically Calibrated Simulations")
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

############
# Analyses #
############

# Summarize theoretical data
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

# Lookup function
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

# generate statistical models
naive_linear=lm(change_13 ~ correct_1>0.5, ag)
poly_fit = loess(change_13 ~ correct_1, data=ag, span=0.9)

# calculate total sum-of-squares for empirical data
SS_tot = sum((ag$change_13-mean(ag$change_13))^2)

# calculate sum of squares for each model residual
SS_res_prereg=sum((ag$predict_prereg-ag$change_13)^2)
SS_res_overfit=sum((ag$predict_overfit-ag$change_13)^2)
SS_res_linear=sum(((ag$change_13-naive_linear$fitted.values)^2))
SS_res_poly=sum(((ag$change_13-poly_fit$fitted)^2))

# calculate R2 for each model
R2_linear = 1 - (SS_res_linear/SS_tot)
R2_prereg = 1 - (SS_res_prereg/SS_tot)
R2_overfit = 1 - (SS_res_overfit/SS_tot)
R2_poly = 1 - (SS_res_poly/SS_tot)

##########
# Output #
##########

### R2 for every model ###
R2_linear
R2_prereg
R2_overfit
R2_poly

### MSE for every model ###
SS_res_prereg
SS_res_overfit
SS_res_linear
SS_res_poly

######################
# Plot of R2 and MSE #
######################

MSE = c(
  SS_res_linear
  ,SS_res_prereg
  ,SS_res_overfit
  ,SS_res_poly
)

R2 = c(
  R2_linear
  ,R2_prereg
  ,R2_overfit
  ,R2_poly
)

plot(MSE, R2)
