library(Matrix)
library(arules)
library(arulesViz)
library(colorspace)
map_to_integer <- function(v){
  as.integer(factor(v, levels = unique(v)))
}

transactions = as.data.frame(data_360k[,c(1,3)])
transactions <- as(split(transactions[,"artname"], transactions[,"usersha1id"]), "transactions")
itemFrequencyPlot(transactions, topN=40)
rules = apriori(transactions, parameter = list(support = 0.001,confidence = 0.01, maxtime = 1000))

inspect(rules[1:10])

