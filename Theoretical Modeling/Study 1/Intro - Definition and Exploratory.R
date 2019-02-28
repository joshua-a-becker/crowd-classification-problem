rm(list=ls()); gc();
library(ggplot2)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

### GENERAL ASSUMPTIONS - FOR BINARY CHOICE
### 1.  a person will never change their answer when they're in the majority
### 2.  a person not in the majority WILL change their answer if some 
###     threshold proportion of their neighbors have a different answer
### 3.  thus, each person's decision to change their answer can be represented
###     by a threshold with T>0.5
### 4.  this threshold is correlated with confidence
### 5.  confidence is correlated with accuracy



### HOW TO MODEL CONFIDENCE AND ACCURACY IN BINARY CHOICE?
### 1.  Each person has a 'signal' Si from 0 to 1
### 2.  The state of the world is always 1
### 3.  The determines their proability of choosing the correct outcome:
###          - a signal of 1 = 100% probability of choosing correctly
###          - a signal of 0 = 0% probability of choosing correctly
### 4.  Probability of correctness corresponse to confidence, which corresponds
###     to threshold:  
###     Si=1 means 100% chance of correct answer means 100% confidence -> threshold=1
###     Si=0 means 0% chance of correct answer means 0% confidence -> threshold=0.5
### 5.  signals are roughly normally distributed with mean=0.5

### GENERATING THE SIGNAL, BELIEF, AND CONFIDENCE
N = 10000           # the size of the population
var.term = 10       # determines the variance of signals
                    # lower value -> higher variance
prob.correct = 0.7  # the overal probability of correct choice


### GENERATING THE SIGNAL - ROUGHLY NORMAL
signal = rbinom(N, var.term, prob.correct)/var.term

### GENERATING CONFIDENCE:  | signal - 0.5 | * 2
### ie, the clarity of the signal, which can be strongly correct (1), strongly incorrect (0), 
### or ambiguous (0.5)
confidence = abs(signal-0.5)*2

### CORRECT FOR ERRORS IN R's FLOAT HANDLING
confidence =  round(confidence, 1)
### GENERATE THRESHOLD:  (confidence/2) + 0.5
threshold = (confidence / 2) + 0.5


### CHECK THAT THE DISTRIBUTION OF SIGNALS IS PLAUSIBLE
hist(signal, main="Distribution of Si", xlab="Signal (Si)")


### CHECK THAT THE DISTRIBUTION OF THRESHOLD IS PLAUSIBLE
hist(threshold)



### GENERATE BELIEFS
### note - a signal determines probability of being correct
belief = unlist(lapply(signal, FUN=function(x){runif(1,0,1)<x}))*1

### DEMONSTRATE THAT THE PROBABILITY OF CORRECT BELIEF IS AS EXPECTED
mean(belief)




population = data.frame(signal=signal,
                        belief=belief,
                        confidence=confidence,
                        threshold=threshold)

### CORRELATE CONFIDENCE WITH PROBABILITY OF BEING CORRECT
ggplot(population, 
       aes(x=confidence, y=belief)) +
  stat_summary(fun.y="mean", geom="point") + joshtheme +
  labs(x="Confidence",y="Proportion Correct")


### CORRELATE THRESHOLD  WITH PROBABILITY OF BEING CORRECT
ggplot(data.frame(correct=ifelse(belief,1,0), threshold=threshold), 
       aes(x=threshold, y=correct)) +
  stat_summary(fun.y="mean", geom="point")



