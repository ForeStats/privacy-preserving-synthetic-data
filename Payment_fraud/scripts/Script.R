library(dplyr)
library(infotheo)
library(rjson)

# load data
load("/Users/santhoshnarayanan/Documents/Turing/Paper_SN_MB/Scripts/all_data.RData")
setwd("~/Documents/Turing/DiffPrivacy")
all_data <- all_data[all_data$Day %in% c(31:58),]
all_data <- all_data[,c(5, 2:3, 6:12)]

all_data$TransAmtInt <- cut(all_data$TransAmt, breaks = quantile(all_data$TransAmt, probs = seq(0,1,0.2)), include.lowest = T)
all_data$TransAmtDisc <- cut(all_data$TransAmt, breaks = quantile(all_data$TransAmt, probs = seq(0,1,0.2)), 
                             labels = 1:5, include.lowest = T)

final_data <- data.frame(apply(all_data[,c(12, 2:10)], 2, FUN = function(z) as.numeric(as.factor(z))))
final_data <- final_data - 1
write.table(final_data, file = 'data.csv', row.names = F, sep = ",")

myjson <- toJSON(list(sapply(final_data, max) + 1), indent = 0)
write( myjson, "domain.json")

library(readr)
library(igraph)

FindCycles = function(g) {
  Cycles = NULL
  for(v1 in V(g)) {
    if(degree(g, v1, mode="in") == 0) { next }
    GoodNeighbors = neighbors(g, v1, mode="out")
    GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
    for(v2 in GoodNeighbors) {
      TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
      TempCyc = TempCyc[which(sapply(TempCyc, length) > 3)]
      TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
      Cycles  = c(Cycles, TempCyc)
    }
  }
  Cycles
}

baseline <- read_csv("baseline.csv")
colnames(baseline) <- c('from', 'to', 'error')

nodesnames <- sort(unique(c(baseline$from, baseline$to)))
baseline$fromN <- factor(baseline$from, levels = nodesnames, labels = 1:10)
baseline$toN <- factor(baseline$to, levels = nodesnames, labels = 1:10)

node_df <- data.frame(Names = nodesnames, Node = 1:10)
library(gridExtra)
pdf("figures/node_df.pdf", height=3.5, width=3)
grid.table(node_df)
dev.off()

fraud_df <- baseline[baseline$from == 'isFraud' | baseline$to == 'isFraud', ]
write.table(fraud_df[, 1:2], file = 'fraud_marginals.csv', row.names = F, sep = ",")
graph <- graph_from_data_frame(fraud_df[,c(4,5)], directed = F, vertices = 1:10)
pdf("figures/fraud_graph.pdf", height=5, width=5)
plot(graph)
dev.off()

graph <- graph_from_data_frame(tail(baseline[,c(4,5)],3), directed = F, vertices = 1:10)
plot(graph)

N = 45
nedges <- 3
counter <- 3

while(nedges <= 9){
  g <- graph %>%
    add_edges(baseline[N-counter,c(4,5)])
  
  plot(g, main = paste0('Iteration: ', counter))
  Cycles = FindCycles(g)
   
  if(length(Cycles)==0){
    nedges <- nedges + 1
    graph <- g
  }
  
  counter <- counter + 1
  
  if(nedges == 9)
    break
}

plot(graph)

mst_edges <- as_data_frame(graph)
colnames(mst_edges) <- c('fromN', 'toN')
df1 <- merge(baseline, mst_edges)

mst_edges <- as_data_frame(graph)
colnames(mst_edges) <- c('toN', 'fromN')
df2 <- merge(baseline, mst_edges)

mst_df <- rbind(df1, df2)
mst_df <- mst_df[order(mst_df$error),]
write.table(mst_df, file = 'mst_marginals.csv', row.names = F, sep = ",")
