# Becker et al.
d2 <-  read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
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

d2 <- d2 %>% mutate(
  initial_yes = pre_influence < 600
  , final_yes = post_influence < 600
)

prop.table(table(d2$initial_yes))

prop.table(table(d2$final_yes))
