### TO USE:
### SET WORKING DIRECTORY TO SOURCE FILE LOCATION

rm(list=ls());gc();
library(readxl,warn.conflicts = F, quietly = T)
library(httr,warn.conflicts = F, quietly = T)
library(tidyverse)
library(magrittr)
source("../dependencies.R")


d1 = read.csv("gurcay_data.csv", stringsAsFactors=F) %>% 
  mutate(
    pre_influence = est1
    , post_influence = est2
    , question = question.no
    , task=question
    , trial=paste0("gurcay",group, "-",question)
    , truth=true.values
    , dataset="gurcay"
  ) %>% 
  subset(condition!="C" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset","task")
  )


d2 = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
              , stringsAsFactors=F) %>% 
  mutate(
    pre_influence = response_1
    , post_influence = response_3
    , trial=paste0("becker", group_number, "-",task)
    , dataset="becker"
  ) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset","task")
  )


lorenz_url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"
GET(lorenz_url, write_disk(tf <- "lorenz_et_al.xls", overwrite=T))
# if(!file.exists("Exclude/lorenz_et_al.xls")) {
#   GET(lorenz_url, write_disk(tf <- "Exclude/lorenz_et_al.xls", overwrite=T))  
# }
d3 <- read_excel("lorenz_et_al.xls") %>%
  mutate(
    pre_influence = E1
    , post_influence = E5
    , dataset="lorenz2011"
    , truth=Truth
    , trial= paste0(Information_Condition, Session_Date,Question)
    , task=Question
    , network= fct_recode(Information_Condition, "Decentralized" = "full", "Solo" = "no", "Decentralized"="aggregated")  
    , communication="Delphi"
  ) %>% 
  subset(network=="Decentralized") %>%
  group_by(trial, task) %>%
  mutate(
    soc_info = mean(pre_influence, na.rm=T)
  ) %>% 
  subset(network=="Decentralized" &
           (!is.na(pre_influence) & !is.na(post_influence)),
         select=c("trial","pre_influence","post_influence","truth","dataset","task")
  )


d = rbind(d1,d2,d3) %>%
  group_by(trial) %>%
  mutate(
      mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    , med1 = median(pre_influence)
    , improve = abs(mu2-truth) < abs(mu1-truth)
    , worse = abs(mu2-truth) > abs(mu1-truth)
  )


head(d)

ag = d %>% 
  group_by(trial, dataset,task) %>%
  summarize(
      truth=unique(truth)
    , med=unique(med1)
    , mu=unique(mu1)
    , C = unique(mu2)
    , med_under = ifelse(med<truth, "Med U","Med Ov")
    , mu_under = ifelse(mu2<truth, "Mu U","Mu Ov")
    , med_less = ifelse(med<mu2, "M<u","u>M")
    , prop_btw = mean( (truth<pre_influence&pre_influence<C) | (C<pre_influence&pre_influence<truth) )
    , improve=unique(improve)
  )

mean(ag$prop_btw)

ag %>%
  group_by(trial,dataset) %>%
  summarize(prop=mean(prop_btw)) %>%
  group_by(dataset) %>%
  summarize(prop=mean(prop))

table(ag$med_under, ag$mu_under, ag$med_less)

# M < ?? < ??
# 28%
ag %$% mean(med<mu & mu<truth)

# ?? < M < ??
# 24%
ag %$% mean(truth<med & med<mu)

# M < ?? < ??
# 24%
ag %$% mean(med<truth & truth<mu)



# Condition 1:  
mean(ag$improve)
ag %>% group_by(dataset) %>%
  summarize(improve=mean(improve))

