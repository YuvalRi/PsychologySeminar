library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M567"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M597") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M607") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M614" | data[i,j] == "M614 ") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M615") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M632") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M633") {
        data[i,j] <- "7"
      }
    }
  }
  return(data)
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

# for Q3
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 3){
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


# simulation 
B <- 10000
circles <- c()
sim_3 <- function(data){
  data <- name_to_number(data)
  data <- data[-which(data[,1] == "4" | data[,2] == "4"),] # '4' participant has no neighbors, so it removed from analysis
  data <- data[-which(data[,1] == "2" | data[,2] == "2"),] # '2' participant had only 1 neighbor, so it removed from analysis
  data <- data[-which(data[,1] == "6" | data[,2] == "6"),] # '6' participant had only 1 neighbor, so it removed from analysis
  data <- data[-which(data[,1] == "7" | data[,2] == "7"),] # '7' participant had only 1 neighbor, so it removed from analysis
  data <- directed_to_undirected_q3(data, version = FALSE)
  for (i in 1:B){
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    graph <- graph(edges, directed = F)
    circles[i] <-graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] ##??adding 
  }
  return(circles)
}

# df
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")

sim_results <- data.frame("number of circles" = c(sim_3(small_male))) 

# saving simulation results in excel file
library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//circles//sim_3_shuffle_small_male_q3.xlsx")

# simulation data set 
data_small_male_q3 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//circles//sim_3_shuffle_small_male_q3.csv")

# histogram - the number of all circles in each iteration equals to 0 
p <- ggplot(data_small_male_q3, 
            aes(x=ï..number.of.circles,
                fill= factor(ifelse(ï..number.of.circles=="0","Highlighted","Normal"))
            )
) + 
  scale_fill_manual(name = "0", 
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
  coord_cartesian(ylim = c(0,1.2), xlim = c(-1, 2)) +
  ggtitle("Frequency of number of circles in the shuffle") +
  theme(legend.position = "none")  
p 

# pvalue, 0 - number of circles in the real graph
pvalue(data_small_male_q3, 0)

## UNSIGNIFICANT 


