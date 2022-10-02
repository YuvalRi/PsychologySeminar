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
      } else if(data[i,j] == "W686") {
        data[i,j] <- "3"
      } else if(data[i,j] == "W717") {
        data[i,j] <- "4"
      } else if(data[i,j] == "W755") {
        data[i,j] <- "5"
      } else if(data[i,j] == "W756") {
        data[i,j] <- "6"
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
      if (j == 29){
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

# small female df
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
edges <- creating_edges(small_female)
small_female_graph <- graph(edges, directed = TRUE)
small_female_plot <- plot(small_female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black")

## CC - Clustering Coefficient (calculated for undirected graph only)
transitivity(small_female_graph, type = c("global"))
transitivity(small_female_graph,vids = V(small_female_graph), type = c("local"))


vertex_names = function (g) {
  vert = V(g)
  if (! is.null(names(vert))) names(vert) else as.vector(vert)
}

trans = data.frame(Vertex = vertex_names(small_female_graph),
                   Transitivity = transitivity(small_female_graph, type = 'local', vids = V(small_female_graph)))

## ASPL - average shortest path length
# in directed graph
mean_distance(small_female_graph, directed = TRUE)
# in undirected graph
mean_distance(small_female_graph, directed = FALSE)

## average amount of edges a node in the network has 
# in directed graph
mean(degree(small_female_graph))

# in undirected graph
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
small_female <- directed_to_undirected_q3(small_female, TRUE)
edges <- creating_edges(small_female)
small_female_graph <- graph(edges)
mean(degree(small_female_graph))

## Diameter
# in directed graph
diameter(small_female_graph, directed = TRUE)
# in undirected graph
diameter(small_female_graph, directed = FALSE)

## Modularity (calculated for undirected graph only)
modularity(small_female_graph, membership = c(1:6), directed = FALSE)

