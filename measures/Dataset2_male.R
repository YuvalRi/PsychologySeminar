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

# function which return a data frame that represent an undirected graph (removing duplicated edges)
# Q1: version == TRUE
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 43){
        {break}
      }
      if( df[i,1] == df[j,2] & df[i,2] == df[j,1] ){
        if (df[i,3] == 1 & df[j,3] == 1){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == 0 & df[j,3] == 0){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == version & df[j,3] == !version){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == !version & df[j,3] == version){
          df_undirected <- rbind(df_undirected, c(df[j,]))
        }
      }
    }
  }
  return(df_undirected)
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
male <- arrange(male, Participant)
male <- name_to_number(male)
edges <- creating_edges(male)
#directed graph
male_graph <- graph(edges, directed = TRUE)
male_plot <- plot(male_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "Figure 2")

## CC - Clustering Coefficient (calculated for undirected graph only)
transitivity(male_graph, type = c("global"))
transitivity(male_graph,vids = V(male_graph), type = c("local"))


vertex_names = function (g) {
  vert = V(g)
  if (! is.null(names(vert))) names(vert) else as.vector(vert)
}

# local cc per vertex
trans = data.frame(Vertex = vertex_names(male_graph),
                   Transitivity = transitivity(male_graph, type = 'local', vids = V(male_graph)))

## ASPL - average shortest path length
# in directed graph
mean_distance(male_graph, directed = TRUE)
# in undirected graph
mean_distance(male_graph, directed = FALSE)

## average amount of edges a node in the network has 
# in directed graph
mean(degree(male_graph))

# in undirected graph
male <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
male <- male[c(1:42), c(1,2,16)]
male <- name_to_number(male)
male <- directed_to_undirected_q3(male, TRUE)
edges <- creating_edges(male)
male_graph <- graph(edges)
mean(degree(male_graph))

## Diameter
male <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
male <- male[c(1:42), c(1,2,16)]
# sorted male df
male <- arrange(male, Participant)
male <- name_to_number(male)
edges <- creating_edges(male)
#directed graph
male_graph <- graph(edges, directed = TRUE)
# in directed graph
diameter(male_graph, directed = TRUE)
# in undirected graph
diameter(male_graph, directed = FALSE)

## Modularity (calculated for undirected graph only)
modularity(male_graph, membership = c(1:7), directed = FALSE)
