library("RSQLite")
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(Matrix)
library(data.table)
library(tidyr)
setwd("Courses/Data Analysis/Project/Music-Analyser/")
con = dbConnect(SQLite(), dbname="data/track_metadata.db")
all_tags_filter = dbGetQuery(con, "select * from msd_lastfm_tags order by tid limit 2500000;")

tags_group = all_tags_filter %>% 
  group_by(var) %>%
  summarise(count=n(), avg_val=mean(val), max_val=max(val)) %>%
  dplyr::filter(count > 100) %>%
  dplyr::filter((avg_val > 10) | (max_val > 40))

all_tags_filter = all_tags_filter %>% 
  dplyr::filter(var %in% tags_group$var)

map_to_integer <- function(v){
  as.integer(factor(v, levels = unique(v)))
}

data.sparse = sparseMatrix(map_to_integer(all_tags_filter$tid), map_to_integer(all_tags_filter$var), x=all_tags_filter$val)

pca.tag = prcomp(data.sparse, center = F, scale. = F)
save(pca.tag, file="data/pca.tag")
eigv = round(pca.tag$sdev^2/sum(pca.tag$sdev^2)*100, 2)
plot(eigv)
cumsum(eigv)


all_tags = dbGetQuery(con, "select * from msd_lastfm_tags;")
all_tags %<>% 
  dplyr::filter(var %in% tags_group$var)

data.sparse = sparseMatrix(map_to_integer(all_tags$tid), map_to_integer(all_tags$var), x=all_tags$val)
all_predict = predict(pca.tag, data.sparse)[,1:150]
all_predict %<>% 
  as.data.frame() %>%
  mutate(tid=unique(all_tags$tid))

dbWriteTable(con, "pca_tag", all_predict)
# kmean = kmeans(all_predict, centers = 100)

