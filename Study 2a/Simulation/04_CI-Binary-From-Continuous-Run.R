rm(list=ls());gc()
library(ggplot2)
source('03_CI-Binary-From-Continuous.R', echo=TRUE)


### FIXED PARAMETERS

N=100
g = degree.sequence.game(rep(4,N), method="vl")
g$name = "decentralized, k=4"
g$cent=0

file = "continuous_choice_outcomes.csv"





write.table(t(names(outcome_record(new_population())))
            , file=file
            , sep=",", row.names=F, col.names=F)
counter=0
for(i in 1:1000) {
  for(c.quantile in seq(0.1,0.9,by=0.1)) {
    for(t.quantile in seq(0.1,0.9,by=0.1)) {
      for(shape in c("skew","symmetrical")) {
        for(rev.choice in c(T,F)) {
          
            ### RUN ONE GAME
            pop = new_population(N=100
                                 , shape=shape
                                 , c.quantile=c.quantile
                                 , t.quantile=t.quantile
                                 , rev.choice=rev.choice
                                 , cor=1
                                 , g=g) 
            
            
            counter = counter+1
            
            for(round in 1:5){
              # RECORD FIRST
              new_row = outcome_record(pop, round, trial=counter)
              write.table(new_row
                          , file=file
                          , sep=",", row.names=F, col.names=F, append=T)
              
              # THEN UPDATE
              pop$choice = updated_belief(pop)
              
              
            
          }
        }
      }
    }
  }
}
