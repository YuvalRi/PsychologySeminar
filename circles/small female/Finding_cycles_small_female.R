library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

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
# for Q1
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

# check if edge is found in df (the df should include only edges that appear in the graph)

is_edge <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i,1] == edge[1] && df[i,2] == edge[2]){
      return(TRUE)
    }
  }
  return(FALSE)
}

#v2, v3 are neighbours of v1
is_circle <- function(v1, v2, v3, df){
  if (is_edge(edge = c(v1, v2), df) && is_edge(edge = c(v1, v3), df) && (is_edge(edge = c(v2, v3), df) | is_edge(edge = c(v3, v2), df))){
    return(TRUE)
  }
  return(FALSE)
}

# counting circles in the graph (directed graph)
count_circles <- function(df){
  count <- 0 
  vec <- c(1,2,3,4,5,7) # the vertices in the graph
  for (v in 1:6){ 
    for(w in 1:5){
      if (w == v){
        next
      }
      for(z in (w+1):6){
        if(z == v | z == w){
          next
        }
        if(is_circle(vec[v], vec[w], vec[z], df)){
          count <- count + 1
        }
      }
    }
  }
  return(count)
}

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v

neighbors <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
    if( df[i,3] == "1" && df[i,2] == v){
      vec[i] <- df[i,1]
    }
  }
  if (is.null(vec)){ #if v has no neighbors -> return "0"
    return("0")
  }
  return(unique(vec[!is.na(vec)]))
}

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v in directed graph

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  if (all(df[df$ï..Participant == v,3]) == "0"){
    vec[v] <- 0
  }
  return(unique(vec[!is.na(vec)]))
}

# for Q3
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 19){
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

# small female df
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
edges <- creating_edges(small_female)
small_female_graph <- graph(edges, directed = TRUE)
small_female_plot <- plot(small_female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black")

svg("figure5.svg")
plot(small_female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "nodesGraphDataset5")
dev.off()


## Q1 - 11 circles
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
small_female <- directed_to_undirected_q3(small_female, TRUE)
edges <- creating_edges(small_female)
small_female_graph <- graph(edges, directed = FALSE)
graph.motifs(small_female_graph,size=3)[length(graph.motifs(small_female_graph,size=3))]

## Q2 - 5 circles
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
# '6' has onle 1 neighbor
small_female <- small_female[-which(small_female[,1] == "6" | small_female[,2] == "6"),]
# creating df with edges only
vec1 <- ifelse(small_female[,3] == "1", small_female[,1],"0")
vec1_nozero <- vec1[vec1 != "0"]
vec2 <- ifelse(small_female[,3] == "1", small_female[,2],"0")
vec2_nozero <- vec2[vec2 != "0"]
d <- data.frame(vec1_nozero, vec2_nozero)
count_circles(d)

## Q3 - 0 circles
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
# '6' has onle 1 neighbor
small_female <- small_female[-which(small_female[,1] == "6" | small_female[,2] == "6"),]
small_female <- directed_to_undirected_q3(small_female, FALSE)
edges <- creating_edges(small_female)
graph <- graph(edges, directed = F)
graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] 



