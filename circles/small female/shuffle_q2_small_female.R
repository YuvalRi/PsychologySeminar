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

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  return(unique(vec[!is.na(vec)]))
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

# pvalue function
pvalue <- function(data, real_value){
  vec <- ifelse(data[,1] > real_value, 1,0)
  return(1-mean(vec))
}

# simulation 
B <- 10000 #number of iterations
circles <- c() #empty vector which will get the number of circles in each iteration (vector of length - 10000)
sim_2 <- function(data){
  data <- name_to_number(data) # changing participants name to number
  data <- data[-which(data[,1] == "6" | data[,2] == "6"),]
  # removing 6 participant which had only 1 neighbor
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

# df
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")

sim_results <- data.frame("number of circles" = c(sim_2(small_female))) 

# saving simulation results in excel file
library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//circles//sim_2_shuffle_small_female_q2.xlsx")


# simulation data set 
data_small_female_q2 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//circles//sim_2_shuffle_small_female_q2.csv")

# histogram
p <- ggplot(data_small_female_q2, 
            aes(x=ï..number.of.circles,
                fill= factor(ifelse(ï..number.of.circles=="5","Highlighted","Normal"))
            )
            ) + 
  scale_fill_manual(name = "5", 
                    values=c("dodgerblue2","gray63")) +
  geom_histogram(bins = 25, 
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
  scale_x_continuous(breaks=c(0:10),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0,0.4), xlim = c(-1, 10)) +
  ggtitle("Frequency of number of circles in the shuffle") +
  theme(legend.position = "none")  
p 

# pvalue, 11 - number of circles in the real graph
pvalue(data_small_female_q2,5)

## UNSIGNIFICANT 




