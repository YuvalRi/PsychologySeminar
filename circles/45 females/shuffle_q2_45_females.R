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
  vec <- c(1,2,3,4,5,6,7,8,9,10) # the vertices in the graph
  for (v in 1:10){ 
    for(w in 1:9){
      if (w == v){
        next
      }
      for(z in (w+1):10){
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
  p_vec <- ifelse(data[,1] <= real_value, 0, 1)
  return(mean(p_vec))
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

#simulation 
B <- 10000 #number of iterations
circles <- c() #empty vector which will get the number of circles in each iteration (vector of length - 10000)
sim_2 <- function(data){
  data <- name_to_number(data) # changing participants name to number
  for (i in 1:B){
    if ( i %% 100 == 0) 
      cat("Progress: ", i / 100, "%\n")
    data[,3] <- sample_values(data) 
    vec1 <- ifelse(data[,3] == "1", data[,1],"0")
    vec1_nozero <- vec1[vec1 != "0"]
    vec2 <- ifelse(data[,3] == "1", data[,2],"0")
    vec2_nozero <- vec2[vec2 != "0"]
    d <- data.frame(vec1_nozero, vec2_nozero) #getting the data with edges only
    circles[i] <- count_circles(d) #counting circles in each iteration
  }
  return(circles)
}


#  45 males df
females_45 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\more datasets\\45females_subset.csv")
sim_results <- data.frame("number of circles" = c(sim_2(females_45)))

# saving simulation results in excel file
library(rio)
export(sim_results, "sim_2_shuffle_45_females_q2.xlsx")

sim_results_df <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\circles\\sim_2_shuffle_45_females_q2.csv")

# histogram
p <- ggplot(sim_results_df, aes(x=ï..number.of.circles,
                                fill= factor(ifelse(ï..number.of.circles== "72","Highlighted","Normal")))) + 
  scale_fill_manual(name = "72", values=c("tan1","white")) +
  geom_histogram(bins = 40, aes(y= after_stat(count / sum(count))), colour= "black")+
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(sim_results_df$ï..number.of.circles), sd = sd(sim_results_df$ï..number.of.circles)), 
    lwd = 0.65, 
    col = 'midnightblue'
  ) +
  theme_bw() +
  xlab("Number of Circles") + ylab("Frequency") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks=seq(min(sim_results_df),max(sim_results_df),2)) + 
  ggtitle("Frequency of Number of circles in the shuffle")
p 


#pvalue
72 > quantile(sim_results_df$ï..number.of.circles, 0.95)
pvalue(sim_results_df, 72)


## SIGNIFICANT 