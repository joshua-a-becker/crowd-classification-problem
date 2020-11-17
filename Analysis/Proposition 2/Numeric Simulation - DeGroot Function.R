library(expm)
library(tidyverse)

source('Generate_A_Matrix.R')

### SET POPULATION SIZE
N = 1000

### SET NUM OF ROUNDS
R=10

### SET BELIEF DISTRIBUTION
#B_func = function(N){exp(rnorm(N,5,0.5))}
B_func = function(N){rnorm(N,0,1)}
#B_func = function(N){exp(rnorm(N,6,0.5))}

### MANUALLY CALCULATE 'THRESHOLD' VALUES
rng=seq(0.1,0.9,by=0.025)
thresholds = cbind(
  rng
  , quantile(B_func(10000), rng)
)


### RUN NUMERICAL CALCULATIONS 100 TIMES
outcomes = data.frame()
for(i in 1:500){
  print(i)
  ### GENERATE A MATRIX
  A = gen_A(N)
  
  ### GENERATE BELIEF DISTRIBUTION
  B_pre =  B_func(N) %>% matrix
  
  ### RUN NUMERIC CALCULATION
  B_post = (A %^% R) %*% B_pre
  
  ### CALCULATE RESULTS
  new_outcomes = apply(thresholds, 1, function(t) {
    data.frame(  
             t_pct=t[1]
           , t_num = t[2]
           , M = median(B_pre)
           , mu = mean(B_pre)
           , C = mean(B_post)
           , pre_vote = mean(B_pre>t[2])
           , post_vote = mean(B_post>t[2])
    )
  }) %>% do.call(rbind, .)
  outcomes = rbind(outcomes, new_outcomes)  
}

saveRDS(outcomes, file="outcomes.Rdata")

### CONVENIENCE FUNCTION--
### IS MAJORITY ENHANCED?
maj_enhanced = function(p1, p2) {
  # 1.  value is further from 0.5
  # 2.  sign is the same
  sign(p1-0.5)==sign(p2-0.5) &
    abs(p1-0.5)<abs(p2-0.5)
}

pred_enhance = function(M,C,t){
  #C<T<M or M<T<C
  return( ! ((C<t & t<M) | (M<t & t<C)) )
}

outcomes$maj_enhanced = maj_enhanced(outcomes$pre_vote, outcomes$post_vote)


outcomes$pred_enhance = pred_enhance(outcomes$M, outcomes$C, outcomes$t_num)

outcomes$correct = outcomes$pred_enhance == outcomes$maj_enhanced

outcomes %>% 
  group_by(t_pct, pred_enhance) %>%
  add_tally(name="count") %>%
  subset(count>10) %>%
  mutate(
    mu=round(mu/20)*20
    , C=round(C/20)*20
    , mu_t = myfn(mu)
    , C_t = myfn(C)
    , delta = round(abs(mu_t-C_t),1)
  ) %>%
  ggplot(aes(x=round(t_pct,1), y=correct*1
             #, color=pred_enhance
                     )) +
  stat_summary(fun.y="mean", geom="line") +
  stat_summary(fun.y="mean", geom="point") +
  theme_test() +
  #facet_grid(delta~.)+
  labs(color="Predicted to Grow?"
       , x="Threshold (Percentile)"
       , y="Percent Predicted Correctly") +
  geom_vline(xintercept=0.5, linetype="dashed") +
  scale_y_continuous(breaks=seq(0,1,by=0.2)
                     , lim=c(0,1)
                     )

outcomes %>% group_by(t_pct) %>%
  summarize(correct = mean(correct))

ggsave("DeGroot Model - Numeric Calculations.png"
       , width=5, height=3)

mean(outcomes$pred_enhance==outcomes$maj_enhanced)

table(outcomes$t_pct, outcomes$pred_enhance)

constr=outcomes %>%
  mutate(
    t_pct=round(t_pct,1)
  ) %>%
  group_by(t_pct
           #, pred_enhance
           ) %>%
  summarize(
    correct=mean(correct)
    , count=n()
  ) %>%
  #subset(!pred_enhance) %>%
  subset(count>10)

table(dx$count)  

names(outcomes)

mean(outcomes$pre_vote)
mean(outcomes$post_vote)

th=data.frame(thresholds) %>%
  mutate(
    V2=round(V2/20)*20
  ) %>%
  rename(
    t_num=V2
    , t_pct=rng
  )

myfn=function(x){ 
  lapply(x, FUN=function(t){
    dout = th$t_pct[th$t_num==t]
    if(length(dout)==0){
      return(NA)
    } else{
      return(max(dout))
    }
  }) %>% unlist
}

dz = outcomes %>%
  mutate(
      mu=round(mu/20)*20
    , C=round(C/20)*20
    , mu_t = myfn(mu)
    , C_t = myfn(C)
  )

hist(dz$C_t)
hist(dz$mu_t)

hist(dz$mu_t, freq=F)
hist(outcomes$t_pct[!outcomes$pred_enhance], freq=F)

quantile(dz$C_t)
quantile(outcomes$t_pct[!outcomes$pred_enhance])
