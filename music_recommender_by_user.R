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


con_track = dbConnect(SQLite(), dbname="../data/track_metadata.db")
#reading data 360k
triplets = dbGetQuery( con_track,'SELECT * FROM triplets' )

#cleaning data
#data_360k %<>% filter(plays < 6500)

our_musics <- triplets %>%  
  group_by(songid) %>%  
  summarise(sum = sum(plays), avg = mean(plays), n = n()) %>%
  filter(n > 20) %>% 
  arrange(desc(n))

our_people <- triplets %>%  
  group_by(usersha1) %>%  
  summarise(sum = sum(plays), avg = mean(plays), n = n()) %>%
  filter(n > 10) %>% 
  arrange(desc(sum))

triplets %<>% filter(songid %in% our_musics$songid)
triplets %<>% filter(usersha1 %in% our_people$usersha1)

our_transactions = as.data.frame(triplets[,])
our_transactions <- as(split(our_transactions[,"songid"], our_transactions[,"usersha1"]), "transactions")


get_song_id <- function(song_id){
  our_musics %>% filter(songid == song_id) %>% dim %>% .[1]
}

our_trim <- function(x){
  substr(x, 2, str_length(x)-1)
}
get_music_recommended <- function(songList, out_length){
  capture.output(rules <- apriori(our_transactions, parameter = list(support = 0.001,confidence = 0.01, maxtime = 1000, minlen=2),
                                  appearance = list(lhs = songList, default="rhs"))) %>% invisible()
  capture.output(rules <- rules %>% inspect() %>% data.frame()) %>% invisible()
  rules %<>% 
    mutate(val = 30*confidence + lift) %>%
    arrange(desc(val))
  ret = as.character(rules[1:out_length, "rhs"])
  ret = sapply(ret, our_trim) %>% as.vector()
  ret
}
