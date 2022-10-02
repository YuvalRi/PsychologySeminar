library(tidyverse)
library(dplyr)
library(igraph)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M629"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M634") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M646") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M647") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M648") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M650") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M651") {
        data[i,j] <- "7"
      } else if(data[i,j] == "M652") {
        data[i,j] <- "8"
      } else if(data[i,j] == "M653") {
        data[i,j] <- "9"  
      } else if(data[i,j] == "M654") {
        data[i,j] <- "10"
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
      if (j == 91){
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

# Dataset4 - 45 males df
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
edges <- creating_edges(males_45)
males_45_graph <- graph(edges, directed = TRUE)
males_45_plot <- plot(males_45_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black")

## CC - Clustering Coefficient (calculated for undirected graph only)
transitivity(males_45_graph, type = c("global"))
transitivity(males_45_graph,vids = V(males_45_graph), type = c("local"))


vertex_names = function (g) {
  vert = V(g)
  if (! is.null(names(vert))) names(vert) else as.vector(vert)
}

trans = data.frame(Vertex = vertex_names(males_45_graph),
                   Transitivity = transitivity(males_45_graph, type = 'local', vids = V(males_45_graph)))

## ASPL - average shortest path length
# in directed graph
mean_distance(males_45_graph, directed = TRUE)
# in undirected graph
mean_distance(males_45_graph, directed = FALSE)

## average amount of edges a node in the network has 
# in directed graph
mean(degree(males_45_graph))

# in undirected graph
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
males_45 <- directed_to_undirected_q3(males_45, TRUE)
edges <- creating_edges(males_45)
males_45_graph <- graph(edges)
mean(degree(males_45_graph))

## Diameter
# in directed graph
diameter(males_45_graph, directed = TRUE)
# in undirected graph
diameter(males_45_graph, directed = FALSE)

## Modularity (calculated for undirected graph only)
modularity(males_45_graph, membership = c(1:10), directed = FALSE)

