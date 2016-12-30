create table user_prof_1k (usersha1 character(40),
                             gender varchar(1), 
                             age integer, 
                             country varchar(30),
                             signupDate varchar(12));
.separator "\t" "\n";
.import lastfm-dataset-360K/usersha1-profile.tsv user_prof_360k
select * from user_prof_1k limit 20;

create table data1k (userid character(11),
                     timestamp character(20),
                     mbartid character(36), artname varchar(30),
                     mbtrid character(36), trname varchar(70));
.separator "\t" "\n";
.import lastfm-dataset-1K/abc.tsv data1k

library(RSQLite)
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(knitr)
#reading users profiles
con = dbConnect(SQLite(), dbname="data/music")
users_1k = dbGetQuery( con,'SELECT * FROM user_prof_1k where usersha1 != \'#id\';' )

#reading data 1k
con2 = dbConnect(SQLite(), dbname="data/music")
data_1k = dbGetQuery( con2,'SELECT * FROM data1k where mbtrid != "" ' )

#cleaning data
tracks <- data_1k %>% 
  group_by(mbtrid,trname, artname) %>% 
  summarise(plays = n()) %>% 
  arrange(desc(plays)) %>% 
  filter(plays > 20)

barplot(tracks$plays)

people <- data_1k %>% 
  group_by(userid) %>% 
  summarise(listens = n()) %>% 
  arrange(listens) %>% 
  filter(listens > 20)

data_1k %<>% filter(mbtrid %in% tracks$mbtrid)
data_1k %<>% filter(userid %in% people$userid)

#fixing dates
mydate = as.POSIXlt(strptime(data_1k$timestamp, format='%Y-%m-%dT%H:%M:%SZ'))
data_1k$year = as.factor(mydate$year+1900)
data_1k$hour = as.factor(mydate$hour)
data_1k$month = as.factor(mydate$mon+1)

#histogram year
ggplot(data_1k, aes(x=year)) +
  geom_histogram(stat="count")
ggplotly()

#histogram hour
ggplot(data_1k, aes(x=hour)) +
  geom_histogram(stat="count")
ggplotly()

#histogram month
ggplot(data_1k, aes(x=month)) +
  geom_histogram(stat="count")
ggplotly()

# most played musics
kable(tracks[1:20,])
