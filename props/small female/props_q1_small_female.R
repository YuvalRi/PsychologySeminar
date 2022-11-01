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
  if (all(df[df$Subject == v,3]) == "0"){
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
      if( is_edge_2(edge = c(new_vec[j], new_vec[k]), df)){
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

#proportions
get_prop <- function(data,n){
  total <- c()
  prop <- c()
  for (i in 1:n){
    total[i] <- choose(length(neighbors(data,i)), 2)
    prop[i] <- (total[i] - diff_rates(data, n)[[i]])/ total[i]
  }
  if(any(is.na(prop) == TRUE)){
    prop[which(is.na(prop))] <- mean(prop, na.rm = TRUE) 
  }
  return(prop)
}

#Data Frame
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- arrange(small_female, Subject)
small_female <- name_to_number(small_female)
small_female <- directed_to_undirected_q3(small_female, TRUE)
small_female <- arrange(small_female, Subject)

get_prop(small_female, 6)


#Normality test - change to total number of observation (42 in the male dataset)

prop_vec_Q1_female <- get_prop(small_female, 6)
shapiro.test(prop_vec_Q1_female)

#according to the results we can imply that the distribution of the data are not significantly different from normal distribution (p value > 0.05)

#two sided proportions Z-test for 2 populations 
alpha <- 0.05

# Question 1 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by one edge where each v_j,v_k connects to v_i by one edge? 
n1 <- nrow(small_female)
n2 <- nrow(small_female)
Q1_p1_hat <- (sum(small_female$click0no1yes == "1"))/n1 #proportion of number of the actual number of edges in the given graph
Q1_p2_hat <- mean(prop_vec_Q1_female) #mean of proportions vector 
Q1_p_hat <- (n1*Q1_p1_hat + n2*Q1_p2_hat)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2  !=  p1 - research hypothesis
z_observed_Q1 = ((Q1_p1_hat- Q1_p2_hat) - 0)/sqrt(Q1_p_hat*(1-Q1_p_hat)*((1/n1)+(1/n2)))
z_observed_Q1
z_observed_Q1 <= -qnorm(1-(alpha/2)) | z_observed_Q1 >= qnorm(1-(alpha/2))

p_val_Q1 = 2*pnorm(z_observed_Q1,lower.tail = TRUE)
p_val_Q1

## UNSIGNIFICANT
