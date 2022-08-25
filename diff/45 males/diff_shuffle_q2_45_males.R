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

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  if (all(df[df$Subject == v,3] == "0")){
    vec[v] <- 0
  }
  return(unique(vec[!is.na(vec)]))
}

# input: edge - pair of vertices of df - dataframe
# output: TRUE - if there is no edge in the graph for a pair of vertices,
# FALSE - if there is an edge in the graph for a pair of vertices

#the function check if there are no edges in the graph for a pair of vertices

is_edge_2 <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i, 1] == edge[1] && df[i, 2] == edge[2] && df[i, 3] == "0"){
      return(TRUE)
    }
  }
  return(FALSE)
}

# counting the potential edges which could exist in our actual graph
is_any_edge <-  function(df, v){
  count <- 0
  new_vector <- neighbors_directed(df, v)
  for(j in 1:length(new_vector)){
    for(k in 1:length(new_vector)){
      if(is_edge_2(edge = c(new_vector[j], new_vector[k]), df)){
        count <- count + 1
      }
    }
  }
  return(count)
}

#input: df - data frame, n - number of vertices

diff_rates <- function(df, n){
  l <- list()
  for (i in 1:n){
    l[i] <- is_any_edge(df, i)
  }
  return(l)
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
#pvalue <- function(data, real_value){
  #p_vec <- ifelse(data[,8] > real_value, 0, 1)
  #return(mean(p_vec))
#}

# shuffle simulation 
# n - number of vertices 
# no participant removed from the analysis
B <- 10000
diff_mat_2 <- list()
sim_2_diff <- function(data, n){
  data <- name_to_number(data)
  for (i in 1:B) {
    if ( i %% 100 == 0)
      cat("progress: ", i / 100, "%\n")
    data[, 3] <- sample_values(data)
    diff_mat_2[[i]] <- diff_rates(data, n)
  }
  sim_2_diff_results <- as.data.frame(do.call(rbind, diff_mat_2))
  return(sim_2_diff_results)
}

males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
sim_res_2 <- sim_2_diff(males_45, 10)

# saving simulation results in excel file
library(rio)
export(sim_res_2, "sim_2_diff_q2_45_males.xlsx")

# the differences in our graph
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
males_45 <- name_to_number(males_45)
males_45 <- arrange(males_45, ï..Participant)
diff_rates(males_45, 10)
mean(as.numeric(diff_rates(males_45, 10))) #9.1

# diff data

diff_data_q2_45_males <- read_csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//diff//sim_2_diff_q2_45_males.csv")

# adding column - mean of the differences of each row

diff_data_q2_45_males$mean <- rowMeans(diff_data_q2_45_males, na.rm=TRUE)

# histogram
diff_hist_q2_45_males <- ggplot(diff_data_q2_45_males, aes(x=mean)) + 
  geom_vline(aes(xintercept= mean(as.numeric(diff_rates(males_45, 10)))),
             color="tan1", linetype="dashed", size=1) +
  geom_histogram(bins = 30, aes(y= after_stat(count / sum(count))), colour= "black")+
  ylab("Frequency") +
  xlab("Differences") +
  theme_bw() +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = seq(6.9,14.1,0.3)) +
  geom_text(x=9.3, y=0.12, label="9.1") +  ggtitle("Frequency of differences in the shuffle")
diff_hist_q2_45_males


#pvalue
mean(as.numeric(diff_rates(males_45, 10))) < quantile(diff_data_q2_45_males$mean, 0.05)
#pvalue(diff_data_q1_45_males, mean(as.numeric(diff_rates(males_45, 10)))) #talk with inbal about calculating pvalues

## SIGNIFICANT  


