library(Matrix)
library(arules)
library(arulesViz)
library(colorspace)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(knitr)
library(DT)
library(stringr)


con_music = dbConnect(SQLite(), dbname="../data/music")
#reading users profiles
users_360k = dbGetQuery( con_music,'SELECT * FROM user_prof_360k' )

#reading data 360k
data_360k = dbGetQuery( con_music,'SELECT * FROM data360k' )

#cleaning data
#data_360k %<>% filter(plays < 6500)

artists <- data_360k %>%  
  group_by(artname) %>%  
  summarise(sum = sum(plays), avg = mean(plays), n = n()) %>%
  filter(n > 20) %>% 
  arrange(desc(n))

people <- data_360k %>%  
  group_by(usersha1id) %>%  
  summarise(sum = sum(plays), avg = mean(plays), n = n()) %>%
  filter(n > 10) %>% 
  arrange(desc(sum))

data_360k %<>% filter(artname %in% artists$artname)
data_360k %<>% filter(usersha1id %in% people$usersha1id)

transactions = as.data.frame(data_360k[,c(1,3)])
transactions <- as(split(transactions[,"artname"], transactions[,"usersha1id"]), "transactions")


get_artist_name <- function(artistName){
  artists %>% filter(artname == artistName) %>% dim %>% .[1]
}

trim <- function(x){
  substr(x, 2, str_length(x)-1)
}
get_artist_recommended <- function(artistList, out_length){
  capture.output(rules <- apriori(transactions, parameter = list(support = 0.001,confidence = 0.01, maxtime = 1000, minlen=2),
                  appearance = list(lhs = artistList, default="rhs"))) %>% invisible()
  capture.output(rules <- rules %>% inspect() %>% data.frame()) %>% invisible()
  rules %<>% 
    mutate(val = 30*confidence + lift) %>%
    group_by(rhs) %>%
    summarise(val = mean(val)) %>%
    arrange(desc(val))
  ret = as.character(rules[1:out_length, "rhs"])
  ret = sapply(ret, trim) %>% as.vector()
  ret
}
# inspect(rules[1:10])
# inspect(sort(rules, by = "lift")[1:20])

# pink_floyd_rules = subset(rules, items %in% c("nightwish","within temptation"))
# inspect(sort(pink_floyd_rules, by = "lift")[1:40])
