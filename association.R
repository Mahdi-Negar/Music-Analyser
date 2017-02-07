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
```

con = dbConnect(SQLite(), dbname="data/music")
#reading users profiles
users_360k = dbGetQuery( con,'SELECT * FROM user_prof_360k' )

#reading data 360k
data_360k = dbGetQuery( con,'SELECT * FROM data360k' )

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
itemFrequencyPlot(transactions, topN=40)
rules = apriori(transactions, parameter = list(support = 0.001,confidence = 0.01, maxtime = 1000))

inspect(rules[1:10])
inspect(sort(rules, by = "lift")[1:20])

pink_floyd_rules = subset(rules, items %in% c("pink floyd","roger waters"))
inspect(sort(pink_floyd_rules, by = "lift")[1:40])
