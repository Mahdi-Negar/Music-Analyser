library(Matrix)
library(arules)
library(arulesViz)
library(colorspace)
map_to_integer <- function(v){
  as.integer(factor(v, levels = unique(v)))
}

data.sparse = sparseMatrix(map_to_integer(data_360k[1:100000,]$usersha1id), map_to_integer(data_360k[1:100000,]$mbartid), x=data_360k[1:100000,]$plays)
conv.data.sparse = as(data.sparse, "ngCMatrix")

rules = apriori(conv.data.sparse, parameter = list(support = 0.001,confidence = 0.01))

