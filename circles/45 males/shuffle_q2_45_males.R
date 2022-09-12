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
  vec <- ifelse(data[,1] < real_value, 1,0)
  return(1-mean(vec))
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
  data <-  data[-which(data[,1] == "10" | data[,2] == "10"),] #removing '10' participant # 10 has no neighbors
  for (i in 1:B){ 
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
males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset.csv")
sim_results <- data.frame("number of circles" = c(sim_2(males_45)))

# saving simulation results in excel file
library(rio)
export(sim_results, "sim_2_shuffle_45_males_q2.xlsx")

sim_results_df <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//sim_2_shuffle_45_males_q2.csv")

# histogram
p <- ggplot(sim_results_df, aes(x=ï..number.of.circles,
                             fill= factor(ifelse(ï..number.of.circles== "108","Highlighted","Normal")))) + 
  scale_fill_manual(name = "108", values=c("tan1","white")) +
  geom_histogram(bins = 70, aes(y= after_stat(count / sum(count))), colour= "black")+
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
  scale_x_continuous(breaks = seq(77,138,5)) + 
  ggtitle("Frequency of Number of circles in the shuffle")
p 


#pvalue, 108 - number of circles in the real graph
pvalue(sim_results_df, 108)

## UNSIGNIFICANT

