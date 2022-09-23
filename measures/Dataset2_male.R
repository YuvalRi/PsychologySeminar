library(tidyverse)
library(dplyr)
library(igraph)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M419"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M485") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M599") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M620") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M626") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M665") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M670") {
        data[i,j] <- "7"
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

# Dataset 2- male df
male <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
male <- male[c(1:42), c(1,2,16)]
# sorted male df
male <- arrange(male, ï..Participant)
male <- name_to_number(male)
edges <- creating_edges(male)
#directed graph
male_graph <- graph(edges, directed = TRUE)
male_plot <- plot(male_graph_directed, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "Figure 2")

## CC - Clustering Coefficient (calculated for undirected graph only)
transitivity(male_graph, type = c("global"))
transitivity(male_graph, type = c("local"))

## ASPL - average shortest path length
# in directed graph
mean_distance(male_graph, directed = TRUE)
# in undirected graph
mean_distance(male_graph, directed = FALSE)

## <k>
mean(degree(male_graph))

## Diameter
# in directed graph
diameter(male_graph, directed = TRUE)
# in undirected graph
diameter(male_graph, directed = FALSE)

## Modularity (calculated for undirected graph only)
modularity(male_graph, membership = c(1:7), directed = FALSE)
