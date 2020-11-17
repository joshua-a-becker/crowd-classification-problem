library(expm)
library(tidyverse)

source('Generate_A_Matrix.R')


### SET BELIEF DISTRIBUTION
B_func = function(N){rnorm(N,0,1)};B_name="norm"
#B_func = function(N){exp(rnorm(N,6,0.5))};B_name="lognorm"

### MANUALLY CALCULATE 'THRESHOLD' VALUES
rng=seq(0.1,0.9,by=0.025)
thresholds = cbind(
  rng
  , quantile(B_func(10000), rng)
)


### RUN NUMERICAL CALCULATIONS 100 TIMES
#outcomes = data.frame()
filename = paste0("sims/numsim",sample(10000000,1),".csv")

for(N in c(100,1000)) {
  for(R in c(10,3)) {
    for(i in 1:1000){
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
               , N=N
               , R=R
               , B_func = B_name
        )
      }) %>% do.call(rbind, .)
      #outcomes = rbind(outcomes, new_outcomes)
      #write.csv(outcomes, filename, row.names=F)
      write.table(new_outcomes, filename, sep = ","
                  , col.names = !file.exists(filename)
                  , append = T
                  , row.names=F)
    }
  }
}