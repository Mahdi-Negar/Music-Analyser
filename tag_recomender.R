library("RSQLite")
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(Matrix)
library(data.table)
library(tidyr)
library(DT)
library(proxy)
con = dbConnect(SQLite(), dbname="../data/track_metadata.db")

search_music <- function(song_name, artist_name){
  query = ""
  if(!is.null(song_name) && song_name != ""){
    query = paste0(query, " title like '", song_name, "'")
  }
  if(!is.null(artist_name) && artist_name != ""){
    if(!is.null(song_name) && song_name != "")
      query = paste0(query, " and")
    query = paste0(query, " artist_name like '", artist_name, "'")
  }
  if(query == "")
    return (data.frame())
  print(query)
  songs = dbGetQuery(con, paste0("select track_id, title, artist_name, duration, artist_familiarity  from songs where ", query, " order by artist_familiarity desc limit 50"))
  data.frame(songs)
}

song_tag = dbGetQuery(con, "select * from pca_tag")


relative_by_tag <- function(song_id, ret_size, include_this){
  query = dbGetQuery(con, paste0("select * from pca_tag where tid='", song_id, "'")) %>% 
    data.frame()
  if(!include_this){
    this_artist = dbGetQuery(con, 
                             paste0("select track_id from songs where artist_id in (select artist_id from songs where track_id='",
                                    song_id, "')"))
    this_artist = this_artist$track_id %>% 
      paste(collapse ="', '")
    print(paste0("select * from pca_tag where tid not in ('", this_artist, "')"))
    all_song = dbGetQuery(con, paste0("select * from pca_tag where tid not in ('", this_artist, "')"))
  }else{
    all_song = song_tag
  }
  tids = simil(query[1,1:150], all_song[,1:150], method="cosine") %>%
    rank()
  tids = all_song[which(tids > length(tids) - ret_size), 151] %>%
    paste(collapse = "', '")
  print(paste0("select * from genre join songs on genre.tid=track_id join msd_lastfm_tags on genre.tid==msd_lastfm_tags.tid where track_id in ('", tids,"')"))
  relative = dbGetQuery(con, paste0("select * from genre join songs on genre.tid=track_id join msd_lastfm_tags on genre.tid==msd_lastfm_tags.tid where track_id in ('", tids,"')"))
  relative %<>% data.frame()
}

your_tags <- function(song_id){
  tags = dbGetQuery(con, paste0("select * from msd_lastfm_tags where tid=='", song_id,"'"))
  ret = tags %>%     
    group_by(your_tags = var) %>%
    summarise(val = sum(val)) %>%
    arrange(desc(val))
}
recommended_songs <- function(relative){
  ret = relative %>% 
    group_by(title, artist_name, genre, year) %>%
    summarise()
}

recommended_tags <- function(relative, ret_size){
relative %>% 
  group_by(recommended_tags = var) %>%
  summarise(val = sum(val)/ret_size) %>%
    arrange(desc(val))
}
# song_tag = dbGetQuery(con, "select * from pca_tag")
# 
# single_song = song_tag[1,]
# 
# tmp <- simil(single_song[1:150], song_tag[1,1:150])
# tid = simil(single_song[1:150], song_tag[2:10000,1:150], method="cosine") %>% which.max()
# 
# relative = dbGetQuery(con, paste0("select * from genre join songs on genre.tid=track_id join msd_lastfm_tags on genre.tid==msd_lastfm_tags.tid where track_id=='", song_tag[tid, 151],"'"))
# 
# pca