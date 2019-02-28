rm(list=ls());gc()
library(igraph)
library(reshape2)
library(plyr)
source('Y:/CI/Simulation/Binary Choice/Signal Model Simulation Functions.R', echo=F)

### STORE SIMULATION DATA

game_outcomes = data.frame(N=numeric(),
                  var.term=numeric(),
                  baseline.prob.correct=numeric(),
                  network=character(),
                  centralization=numeric(),
                  round=numeric(),
                  prob.correct=numeric(),
                  stringsAsFactors=F
                  )


### FIXED PARAMETERS
N = 50              # the size of the population
var.term = 10       # determines the variance of signals


g1 = degree.sequence.game(rep(4,N), method="vl")
g1$name = "decentralized, k=4"
g2 = graph.star(N, mode="undirected")
g1$cent=centralization.degree(g1)$centralization
g2$cent=centralization.degree(g2)$centralization


num.trials = 100

counter=0

for(j in 1:num.trials) {
  print(j)
  for(g in list(g1,g2)) {
    for(prob.correct in c(0.2,0.4,0.5,0.6,0.8)) {
      ### RUN ONE GAME
      
      pop = new_population(N=N, var.term=var.term, prob.correct=prob.correct, g=g)
      
      counter = counter+1

      for(round in 1:5){
        # RECORD FIRST
        new_row = outcome_record(pop, round)
        new_row$trial = counter
        game_outcomes = rbind(game_outcomes, new_row)
        
        # THEN UPDATE
        pop$belief = updated_belief(pop)
        
        
      }
    } 
  }
}  
  




d = ddply(game_outcomes, .(trial), summarize,
          accuracy_1 = prob.correct[round==1],
          accuracy_5 = prob.correct[round==5]
      )

d$improv = d$accuracy_5 - d$accuracy_1

ggplot(d, aes(x=baseline.prob.correct, y=improv)) + 
  stat_summary(fun.y="mean", geom="point") +
  facet_grid(.~network)
