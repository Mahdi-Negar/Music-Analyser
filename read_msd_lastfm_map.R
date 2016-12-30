library("RSQLite")
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr)
library(Matrix)
library(data.table)

x = fread("data/msd_lastfm_map.cls", sep = "\t", fill = TRUE, header = FALSE)
colnames(x) = c("tid", 'genre', 
                paste0(c('val', 'tag'), as.integer(c(seq_len(202)/2)))[2:201])
add_to_tag <- function(i, x){
  tmp=x[,(2*i+1):(2*i+2)]
  tmp=cbind(x[,1], tmp)
  colnames(tmp) = c("tid", "var", "val")
  tmp = na.omit(tmp)
}
all_tags = data.frame(colnames(c("tid", "var", "val")))

for(i in 1:100){
  all_tags = rbind(all_tags,
                   add_to_tag(i, x))
}
#all_tags %<>% filter(val > 10)
all_tags %<>% arrange(tid)
all_tags = all_tags[1:4000000,]

tags_group = all_tags %>% 
  group_by(var) %>%
  summarise(count=n(), avg_val=mean(val), max_val=max(val)) %>%
  dplyr::filter(count > 100) %>%
  dplyr::filter((avg_val > 10) | (max_val > 40))

all_tags_filter = all_tags %>% 
  dplyr::filter(var %in% tags_group$var)

#matrix.tag = all_tags_filter %>%
#  dcast(tid ~ var) %>%
#  na.omit()

map_to_integer <- function(v){
  as.integer(factor(v, levels = unique(v)))
}

data.sparse = sparseMatrix(map_to_integer(all_tags_filter$tid), map_to_integer(all_tags_filter$var), x=all_tags_filter$val)

pca.tag = prcomp(data.sparse, center = F, scale. = F)
plot(pca.tag, type = "l")
eigv = round(pca.tag$sdev^2/sum(pca.tag$sdev^2)*100, 2)
plot(eigv)
cumsum(eigv)
