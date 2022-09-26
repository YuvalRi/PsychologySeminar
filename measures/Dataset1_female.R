library(tidyverse)
library(dplyr)
library(igraph)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "W396"){
        data[i,j] <- "1"
      } else if(data[i,j] == "W515") {
        data[i,j] <- "2"
      } else if(data[i,j] == "W617") {
        data[i,j] <- "3"
      } else if(data[i,j] == "W622") {
        data[i,j] <- "4"
      } else if(data[i,j] == "W623") {
        data[i,j] <- "5"
      } else if(data[i,j] == "W674") {
        data[i,j] <- "6"
      } else if(data[i,j] == "W682") {
        data[i,j] <- "7"
      } else if(data[i,j] == "W764") {
        data[i,j] <- "8"
      } else if(data[i,j] == "W776") {
        data[i,j] <- "9"  
      } else if(data[i,j] == "W778") {
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

# Dataset 1- female df
# females df
female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
# sub df - relevant columns
female <- female[,c(1,2,25)]
# sorted Women df
female <- arrange(female, Subject)
female <- name_to_number(female)
edges <- creating_edges(female)
female_graph <- graph(edges, directed = TRUE)
female_plot <- plot(female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "Figure 1")


## CC - Clustering Coefficient (calculated for undirected graph only)
transitivity(female_graph, type = c("global"))
transitivity(female_graph,vids = V(female_graph), type = c("local"))

vertex_names = function (g) {
  vert = V(g)
  if (! is.null(names(vert))) names(vert) else as.vector(vert)
}

trans = data.frame(Vertex = vertex_names(female_graph),
                   Transitivity = transitivity(female_graph, type = 'local', vids = V(female_graph)))

## ASPL - average shortest path length
# in directed graph
mean_distance(female_graph, directed = TRUE)
# in undirected graph
mean_distance(female_graph, directed = FALSE)

## <k>
mean(degree(female_graph))

## Diameter
# in directed graph
diameter(female_graph, directed = TRUE)
# in undirected graph
diameter(female_graph, directed = FALSE)

## Modularity (calculated for undirected graph only)
modularity(female_graph, membership = c(1:10), directed = FALSE)
