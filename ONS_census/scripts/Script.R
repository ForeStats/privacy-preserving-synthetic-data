library(readr)
library(readxl)
library(rjson)
library(igraph)
library(gridExtra)

setwd("~/Documents/Turing/DiffPrivacy/ONS/")

Census_Data <- read_csv("2011 Census Microdata Teaching File.csv", skip = 1)
str(Census_Data)

Meta_Data <- read_excel("meta.xls")

set.seed(123)
final_data <- data.frame(apply(Census_Data[sample(1:nrow(Census_Data), 100000),-1], 
                               2, FUN = function(z) as.numeric(as.factor(z))))
final_data <- final_data - 1
write.table(final_data, file = 'data.csv', row.names = F, sep = ",")

myjson <- toJSON(list(sapply(final_data, max) + 1), indent = 0)
write( myjson, "domain.json")

table(final_data$Occupation, final_data$Sex)

####

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
baseline$fromN <- factor(baseline$from, levels = nodesnames, labels = 1:17)
baseline$toN <- factor(baseline$to, levels = nodesnames, labels = 1:17)

node_df <- data.frame(Names = nodesnames, Node = 1:17)

pdf("figures/node_df.pdf", height=5.5, width=3.5)
grid.table(node_df)
dev.off()

graph <- graph_from_data_frame(tail(baseline[,c(4,5)],3), directed = F, vertices = 1:17)
plot(graph)

N = 136
nedges <- 3
counter <- 3

while(nedges <= 16){
  g <- graph %>%
    add_edges(baseline[N-counter,c(4,5)])
  
  plot(g, main = paste0('Iteration: ', counter))
  Cycles = FindCycles(g)
  
  if(length(Cycles)==0){
    nedges <- nedges + 1
    graph <- g
  }
  
  counter <- counter + 1
  
  if(nedges == 16)
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
