library("RSQLite")
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(Matrix)
library(data.table)
library(tidyr)
setwd("Courses/Data Analysis/Project/Music-Analyser/")

#########write tags to db
x = fread("data/msd_lastfm_map.cls", sep = "\t", fill = TRUE, header = FALSE)
colnames(x) = c("tid", 'genre',
                paste0(c('val', 'tag'), as.integer(c(seq_len(202)/2)))[2:201])
dbWriteTable(con, "genre", x[,1:2])
dbSendQuery(con, "create index idx_tid_genre on genre(tid);")
dbSendQuery(con, "create index idx_genre_genre on genre(genre);")
add_to_tag <- function(i){
  tmp=x[,(2*i+1):(2*i+2)]
  tmp=cbind(x[,1], tmp)
  colnames(tmp) = c("tid", "var", "val")
  tmp = na.omit(tmp)
}
all_tags = data.frame(colnames(c("tid", "var", "val")))
for(i in 1:100){
  all_tags = rbind(all_tags,
                   add_to_tag(i))
}
all_tags[,3] = as.integer(unlist(all_tags[,3]))
#all_tags %<>% filter(val > 10)
all_tags %<>%
  filter(val>0) %>%
  group_by(tid, var)  %>%
  summarise(val=mean(val)) %>%
  arrange(tid) %>%
  as.data.frame()
con = dbConnect(SQLite(), dbname="data/track_metadata.db")
dbWriteTable(con, "msd_lastfm_tags", all_tags)
dbSendQuery(con, "create index idx_var on msd_lastfm_tags(var);")
dbSendQuery(con, "create index idx_val on msd_lastfm_tags(val);")
dbSendQuery(con, "create index idx_tid on msd_lastfm_tags(tid);")