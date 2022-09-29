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
  vec <- c(1,2,4,5,7,8,9,10) # the vertices in the graph
  for (v in 1:8){ 
    for(w in 1:7){
      if (w == v){
        next
      }
      for(z in (w+1):8){
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

# function which return a data frame that represent an undirected graph (removing duplicated edges)
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 57){
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

## Figure 
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

svg("figure1.svg")
plot(female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "nodesGraphDataset1")
dev.off()

## Q1 - 49 circles
# female df
female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
# sub df - relevant columns
female <- female[,c(1,2,25)]
# sorted Women df
female <- arrange(female, Subject)
female <- name_to_number(female)
female <- directed_to_undirected_q3(female, TRUE)
edges <- creating_edges(female)
female_graph <- graph(edges, directed = FALSE)
graph.motifs(female_graph,size=3)[length(graph.motifs(female_graph,size=3))]

## Q2 - 57 circles
female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
# sub df - relevant columns
female <- female[,c(1,2,25)]
# sorted Women df
female <- arrange(female, Subject)
female <- name_to_number(female)
# '6' & '3' have only one neighbors 
female <- female[-which(female[,1] == "6" | female[,2] == "6"),] #removing '6' participant
female <- female[-which(female[,1] == "3" | female[,2] == "3"),] #removing '3' participant
# creating df with edges only
vec1 <- ifelse(female[,3] == "1", female[,1],"0")
vec1_nozero <- vec1[vec1 != "0"]
vec2 <- ifelse(female[,3] == "1", female[,2],"0")
vec2_nozero <- vec2[vec2 != "0"]
d <- data.frame(vec1_nozero, vec2_nozero)
count_circles(d)

## Q3 - 6 circles
female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
# sub df - relevant columns
female <- female[,c(1,2,25)]
# sorted Women df
female <- arrange(female, Subject)
female <- name_to_number(female)
# '6' & '3' have only one neighbors 
female <- female[-which(female[,1] == "6" | female[,2] == "6"),] #removing '6' participant
female <- female[-which(female[,1] == "3" | female[,2] == "3"),] #removing '3' participant
female <- directed_to_undirected_q3(female, FALSE)
edges <- creating_edges(female)
graph <- graph(edges, directed = F)
graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] 
