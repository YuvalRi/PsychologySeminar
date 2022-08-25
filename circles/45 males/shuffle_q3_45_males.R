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
  g_vec <- data$click_1yes_0no
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
  p_vec <- ifelse(data[,1] <= real_value, 0, 1)
  return(mean(p_vec))
}


# simulation 
B <- 10000
circles <- c()
sim_3 <- function(data){
  data <- name_to_number(data)
  data <- data[-c(4,6,12, 15,24, 25, 32,34,41, 44, 51,59, 63,69, 75,83, 85),] # 10 has no neighbors  data <- directed_to_undirected_q3(data, version = FALSE)
  data <- directed_to_undirected_q3(data, version = FALSE)
  for (i in 1:B){
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    graph <- graph(edges, directed = F)
    circles[i] <-graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] #calculate number of circles in our graph 
  }
  return(circles)
}


#45 males df
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
sim_results <- data.frame("number of circles" = c(sim_3(males_45)))

# saving simulation results in excel file
library(rio)
export(sim_results, "sim_3_shuffle_45_males_q3.xlsx")

sim_results_df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//circles//sim_3_shuffle_45_males_q3.csv")

# histogram
p <- ggplot(sim_results_df, aes(x=ï..number.of.circles,
                                fill= factor(ifelse(ï..number.of.circles== "10","Highlighted","Normal")))) + 
  scale_fill_manual(name = "10", values=c("tan1","white")) +
  geom_histogram(bins = 18, aes(y= after_stat(count / sum(count))), colour= "black")+
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(sim_results_df$ï..number.of.circles), sd = sd(sim_results_df$ï..number.of.circles)), 
    lwd = 0.65, 
    col = 'midnightblue'
  ) +
  theme_bw() +
  xlab("Number of Circles") +
  ylab("Frequency") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = seq(1,18,1)) + 
  ggtitle("Frequency of Number of circles in the shuffle")
p 


#pvalue
# 10 is the number of required circles
10 > quantile(sim_results_df$ï..number.of.circles, 0.95)
pvalue(sim_results_df, 10)

## UNSIGNIFICANT 