library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

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

# check if edge is found in df

is_edge <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i,1] == edge[1] && df[i,2] == edge[2]){
      return(TRUE)
    }
  }
  return(FALSE)
}

# circle is defined as stated in question

is_circle <- function(v1, v2, v3, df){
  if (is_edge(edge = c(v1, v2), df) && is_edge(edge = c(v1, v3), df) && (is_edge(edge = c(v2, v3), df) | is_edge(edge = c(v3, v2), df))){
    return(TRUE)
  }
  return(FALSE)
}

# counting circles in the graph (directed graph)
count_circles <- function(df){
  count <- 0 
  vec <- c(1,2,3,4,5,6,7,8,9) # the vertices in the graph
  for (v in 1:9){ 
    for(w in 1:8){
      if (w == v){
        next
      }
      for(z in (w+1):9){
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

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  return(unique(vec[!is.na(vec)]))
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
  return(vec[!is.na(vec)])
}

# function which return a data frame that represent an undirected graph (removing duplicated edges)
# Q3: version == FALSE
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 74){
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


## Figure
# 45 males df
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
edges <- creating_edges(males_45)
males_45_graph <- graph(edges, directed = TRUE)
males_45_plot <- plot(males_45_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black")

svg("nodesGraphDataset4.svg")
plot(males_45_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "nodesGraphDataset4")
dev.off()

## Q1 - 60 circles
# 45 males df
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
males_45 <- directed_to_undirected_q3(males_45, TRUE)
edges <- creating_edges(males_45)
males_45_graph <- graph(edges, directed = FALSE)
graph.motifs(males_45_graph,size=3)[length(graph.motifs(males_45_graph,size=3))]


## Q2 - 108 circles
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
# '10' has no neighbors
males_45 <- males_45[-which(males_45[,1] == "10" | males_45[,2] == "10"),] #removing '10' participant
# creating df with edges only
vec1 <- ifelse(males_45[,3] == "1", males_45[,1],"0")
vec1_nozero <- vec1[vec1 != "0"]
vec2 <- ifelse(males_45[,3] == "1", males_45[,2],"0")
vec2_nozero <- vec2[vec2 != "0"]
d <- data.frame(vec1_nozero, vec2_nozero)
count_circles(d)


## Q3 - 10 circles
# 45 males df
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
# '10' has no neighbors
males_45 <- males_45[-which(males_45[,1] == "10" | males_45[,2] == "10"),] #removing '10' participant
males_45 <- directed_to_undirected_q3(males_45, FALSE)
edges <- creating_edges(males_45)
graph <- graph(edges, directed = F)
graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] 

