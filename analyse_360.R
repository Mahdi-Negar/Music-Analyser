system('.import abc.tsv data360k')

create table data360k 
  (usersha1 character(40), 
  artid varchar(36), 
  artname varchar(40), 
  plays interger)

.separator "\t" "\n"
.import usersha1-artmbid-artname-plays.tsv data360k

library("RSQLite")
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
con = dbConnect(SQLite(), dbname="mahdi")
p1 = dbGetQuery( con,'SELECT * FROM data360k WHERE usersha1 in
                 (select distinct usersha1 from data360k order by RANDOM() LIMIT 100000)' )

p1 %<>% filter(traid < 6500)

grp_art <- p1 %>%  
  group_by(artname) %>%  
  summarise(sum = sum(traid), avg = mean(traid), n = n()) %>%
  filter(n > 1)

p1 %<>% filter(!artname %in% grp_art$artname)


grp_art %<>% filter(n > 70)
ggplot(grp_art, aes(x=n)) +
  geom_histogram(binwidth = 5)

ggplotly()
