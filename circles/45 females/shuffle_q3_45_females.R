library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

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

# function which return a data frame that represent an undirected graph (removing duplicated edges)
# Q3: version == FALSE
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 73){
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

# return a permutation of 0,1 where 1 represent an edges and 0 represent no edge
sample_values <- function(data){
  # original vector
  g_vec <- data$click0no1yes
  n <- length(g_vec)
  new_vec <- c()
  # sample permutations between [0,1] with length n
  seq_vec <- sample(c(1:n), n, replace = FALSE)
  
  for (i in 1:length(seq_vec)){
    new_vec[i] <- g_vec[seq_vec[i]]
  }
  return(new_vec)
}

# function for getting pvalue 
pvalue <- function(data, real_value){
  vec <- ifelse(data[,1] < real_value, 1,0)
  return(1-mean(vec))
}


# simulation 
B <- 10000
circles <- c()
sim_3 <- function(data){
  data <- name_to_number(data)
  # '4' has only one neighbor
  data <- data[-which(data[,1] == "4" | data[,2] == "4"),] #removing '4' participant
  data <- directed_to_undirected_q3(data, version = FALSE)
  for (i in 1:B){
    if ( i %% 100 == 0) 
      cat("Progress: ", i / 100, "%\n")
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    graph <- graph(edges, directed = F)
    circles[i] <-graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] #calculate number of circles in our graph 
  }
  return(circles)
}


#45 males df
females_45 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\more datasets\\45females_subset.csv")
sim_results <- data.frame("number of circles" = c(sim_3(females_45)))

# saving simulation results in excel file
library(rio)
export(sim_results, "sim_3_shuffle_45_females_q3.xlsx")

sim_results_df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//circles//sim_3_shuffle_45_females_q3.csv")

# histogram
p <- ggplot(sim_results_df, 
            aes(x=ï..number.of.circles,
                fill= factor(ifelse(ï..number.of.circles=="0","Highlighted","Normal"))
            )
) + 
  scale_fill_manual(name = "0", 
                    values=c("dodgerblue2","gray63")) +
  geom_histogram(bins = 15, 
                 aes(y= after_stat(count / sum(count))), 
                 colour= "black") +
  theme_bw() +
  xlab("Number of circles") + 
  ylab("Frequency") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks=c(0:3),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0,0.8), xlim = c(-0.5, 3)) +
  ggtitle("Frequency of number of circles in the shuffle") +
  theme(legend.position = "none")  
p 

# pvalue, 0 - number of circles in the real graph
pvalue(sim_results_df,0)

## UNSIGNIFICANT 