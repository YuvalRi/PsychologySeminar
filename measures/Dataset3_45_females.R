library(tidyverse)
library(dplyr)
library(igraph)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "W788"){
        data[i,j] <- "1"
      } else if(data[i,j] == "W789") {
        data[i,j] <- "2"
      } else if(data[i,j] == "W790") {
        data[i,j] <- "3"
      } else if(data[i,j] == "W791") {
        data[i,j] <- "4"
      } else if(data[i,j] == "W792") {
        data[i,j] <- "5"
      } else if(data[i,j] == "W793") {
        data[i,j] <- "6"
      } else if(data[i,j] == "W794") {
        data[i,j] <- "7"
      } else if(data[i,j] == "W795") {
        data[i,j] <- "8"
      } else if(data[i,j] == "W796") {
        data[i,j] <- "9"  
      } else if(data[i,j] == "W797") {
        data[i,j] <- "10"
      }
    }
  }
  return(data)
}


# input - data frame, output - vector of characters
creating_edges <- function(data){
  edges_vec <- c()
  for (i in 1:nrow(data)) {
    if (data[i,3] == 1) {
      edges_vec <- append(edges_vec, data[i,1])
      edges_vec <- append(edges_vec, data[i,2])
    }
  }
  return(edges_vec)
}


# Dataset 3 - 45 females df
females_45 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\more datasets\\45females_subset.csv")
females_45 <- name_to_number(females_45)
edges <- creating_edges(females_45)
females_45_graph <- graph(edges, directed = TRUE)
females_45_plot <- plot(females_45_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="pink", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "Figure 4")

## CC - Clustering Coefficient (calculated for undirected graph only)
transitivity(females_45_graph, type = c("global"))
transitivity(females_45_graph, type = c("local"))

## ASPL - average shortest path length
# in directed graph
mean_distance(females_45_graph, directed = TRUE)
# in undirected graph
mean_distance(females_45_graph, directed = FALSE)

## <k>
mean(degree(females_45_graph))

## Diameter
# in directed graph
diameter(females_45_graph, directed = TRUE)
# in undirected graph
diameter(females_45_graph, directed = FALSE)

## Modularity (calculated for undirected graph only)
modularity(females_45_graph, membership = c(1:10), directed = FALSE)
