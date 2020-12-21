################################################################################

# This is the script to create the simulations for proposition 2.
# THIS SHOULD BE RUN BEFORE ANYTHING ELSE

################################################################################

###############
# Preparation #
###############

# Loading dependencies
library(expm)
library(tidyverse)
library(here)

# Loading the simulation functions
source(here('Simulations/Numeric Simulations/Numeric Simulation - Generate A Matrix.R'))

###########################
# Running the simulations #
###########################

### SET BELIEF DISTRIBUTION
B_func = function(N){rnorm(N,0,1)};B_name="norm"

### MANUALLY CALCULATE 'THRESHOLD' VALUES
rng=seq(0.1,0.9,by=0.025)
thresholds = cbind(
  rng
  , quantile(B_func(10000), rng)
)

### Save output as it's generated in case we get interrupted
filename = here(paste0("Simulations/Numeric Simulations/numsim",sample(10000000,1),".csv"))

### fix randomizaiton for replication
set.seed(42)

### run 10k times!
for(i in 1:10000){
  for(R in c(10)) {
    for(N in c(100,1000)) {
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
      write.table(new_outcomes, filename, sep = ","
                  , col.names = !file.exists(filename)
                  , append = T
                  , row.names=F)
    }
  }
}