library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

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
# Q3: version == FALSE
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

# input: edge - pair of vertices of df - dataframe
# output: TRUE - if there is no edge in the graph for a pair of vertices,
# FALSE - if there is an edge in the graph for a pair of vertices

#the function check if there are no edges in the graph for a pair of vertices

is_edge_2 <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i,1] == edge[1] && df[i,2] == edge[2] && df[i,3] == "0"){
      return(TRUE)
    }
  }
  return(FALSE)
}

# counting the potential edges which could exist in our actual graph
is_any_edge <-  function(df,v){
  count <- 0
  new_vec <- neighbors(df,v)
  for(j in 1:length(new_vec)){
    for(k in 1:length(new_vec)){
      if(is_edge_2(edge = c(new_vec[j], new_vec[k]), df)){
        count <- count + 1
      }
    }
  }
  return(count)
}

#input: df - data frame (need to be an undirected graph), n - number of vertices

diff_rates <- function(df,n){
  l <- list()
  for (i in 1:n){
    l[i] <- is_any_edge(df,i)
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
pvalue <- function(data, real_value){
  p_vec <- ifelse(data[,8] >= real_value, 0, 1)
  return(mean(p_vec))
}

#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42), c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, ï..Participant)

# simulation 
B <- 20000
diff_mat_3 <- list()
sim_3_diff <- function(data, n){
  data <- name_to_number(data)
  data <- directed_to_undirected_q3(data, version = FALSE)
  for (i in 1:B){
    data[, 3] <- sample_values(data)
    if (  all(data[data$ï..Participant == "1",3] == "0") | all(data[data$ï..Participant == "2",3] == "0")
          | all(data[data$ï..Participant == "3",3] == "0") | all(data[data$ï..Participant == "4",3] == "0") 
          | all(data[data$ï..Participant == "5",3] == "0") | all(data[data$ï..Participant == "6",3] == "0") 
          | all(data[data$ï..Participant == "7",3] == "0"))
    {
      next
    }
    diff_mat_3[[i]] <- diff_rates(data, n)
  }
  sim_3_diff_results <- as.data.frame(do.call(rbind, diff_mat_3))
  return(sim_3_diff_results)
}

sim_res_3 <- sim_3_diff(Clicks_sorted_men, 7)

# saving simulation results in excel file
library(rio)
export(sim_res_3, "sim_3_diff_q3.xlsx")

# the differences in our graph
Clicks_sorted_men <- name_to_number(Clicks_sorted_men)
Clicks_sorted_men <- directed_to_undirected_q3(Clicks_sorted_men, FALSE)
diff_rates(Clicks_sorted_men, 7)
mean(as.numeric(diff_rates(Clicks_sorted_men, 7)))

# diff data

diff_data_q3_male <- read_csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//diff//sim_3_diff_q3_male.csv")

# adding column - mean of the differences of each row

diff_data_q3_male$mean <- rowMeans(diff_data_q3_male, na.rm=TRUE)

# histogram
diff_hist_q3_male <- ggplot(diff_data_q3_male, aes(x=mean)) + 
  geom_vline(aes(xintercept= mean(as.numeric(diff_rates(Clicks_sorted_men, 7)))),
             color="tan1", linetype="dashed", size=1) +
  geom_histogram(bins = 60, aes(y= after_stat(count / sum(count))), colour= "black")+
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(diff_data_q3_male$mean), sd = sd(diff_data_q3_male$mean)), 
    lwd = 0.65, 
    col = 'black')  +
  ylab("Frequency") +
  xlab("Differences") +
  theme_bw() +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks = seq(0,3,0.1)) +
  ylim(0,1.6) +
  geom_text(x=1.02, y=0.8, label="1", color = "black") +  
  ggtitle("Frequency of differences in the shuffle")
diff_hist_q3_male

#pvalue
mean(as.numeric(diff_rates(Clicks_sorted_men, 7))) < quantile(diff_data_q3_male$mean, 0.95)*(-1)
pvalue(diff_data_q3_male, mean(as.numeric(diff_rates(Clicks_sorted_men, 7))))

## Unsiginificant 




