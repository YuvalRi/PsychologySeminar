library(tidyverse)
library(dplyr)
library(igraph)

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

# pvalue for cc and modularity
pvalue_1 <- function(data, real_value){
  vec <- ifelse(data < real_value, 1,0)
  return(1-mean(vec))
}

#pvalue for aspl (continuous values)
pvalue_2 <- function(data, real_value){
  vec <- ifelse(data >= real_value, 0,1)
  return(mean(vec))
}

#pvalue for  diameter (discrete values)
pvalue_3 <- function(data, real_value){
  vec <- ifelse(data >= real_value, 1,0)
  return(1-mean(vec))
}


#shuffle 
B <- 10000
cc <- c()
aspl_directed <- c()
aspl_undirected <- c()
diameter_directed <- c()
mod <- c()
sim_1 <- function(data){
  data <- name_to_number(data)
  for (i in 1:B){
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    g <- graph(edges, directed = TRUE)
    cc[i] <- transitivity(g, type = c("global"))
    aspl_directed[i] <- mean_distance(g, directed = TRUE)
    aspl_undirected[i] <- mean_distance(g, directed = FALSE)
    diameter_directed[i] <- diameter(g, directed = TRUE)
    mod[i] <- modularity(g, membership = V(g), directed = FALSE)
  }
  df <- data.frame(cc, aspl_directed, aspl_undirected, diameter_directed, mod)
  colnames(df) <- c("CCrand","ASPL_directed_rand", "ASPL_undirected_rand", "Diameter_directed_rand", "Modularityrand")
  return(df)
}

# df
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//Dataset6Corrected3columns.csv")
sim_results <- sim_1(small_male)

library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//measures//shuffle_dataset6_small_male_new.xlsx")

# simulation data set 
sim_res_dataset6 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset6_small_male_new.csv")

# cc hist
cc_hist <- ggplot(sim_res_dataset6,
                  aes(x= CCrand)
) +
  geom_histogram(bins = 10,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Clustering Coefficient (CC)") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(0,1,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.5),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=0.53,
            y=0.27,
            label="0.5") + 
  coord_cartesian(ylim = c(0, 0.6), xlim = c(-0.1, 1.1)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Clustering Coefficient (CC) in the shuffle") 
cc_hist

pvalue_1(sim_res_dataset6$CCrand, 0.6)

# aspl directed hist
aspl_directed_hist <- ggplot(sim_res_dataset6,
                             aes(x= ASPL_directed_rand)
) +
  geom_histogram(bins = 10,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - directed graph") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(1,1.7,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.4),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=1.42,
            y=0.3,
            label="1.4") + 
  coord_cartesian(ylim = c(0, 0.35), xlim = c(1, 1.7)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Average shortest path length (ASPL) in directed graph") 
aspl_directed_hist

pvalue_2(sim_res_dataset6$ASPL_directed_rand, 1.571)

# aspl undirected hist
aspl_undirected_hist <- ggplot(sim_res_dataset6,
                               aes(x= ASPL_undirected_rand)
) +
  geom_histogram(bins = 7,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - undirected graph") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(0.9,1.65,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.333),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=1.36,
            y=0.4,
            label="1.333") + 
  coord_cartesian(ylim = c(0, 0.6), xlim = c(0.9, 1.65)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Average shortest path length (ASPL) in undirected graph") 
aspl_undirected_hist

pvalue_2(sim_res_dataset6$ASPL_undirected_rand, 1.286)


# diameter directed hist
diameter_directed_hist<- ggplot(sim_res_dataset6,
                                  aes(x= Diameter_directed_rand)
) +
  geom_histogram(bins = 5,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Diameter - directed graph") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(0,4,1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 3),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=3.1,
            y=0.95,
            label="3") + 
  coord_cartesian(ylim = c(0, 0.75), xlim = c(0, 4)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of diameter in undirected graph") 
diameter_directed_hist

pvalue_3(sim_res_dataset6$Diameter_directed_rand, 3)

# modularity hist
modularity_hist <- ggplot(sim_res_dataset6,
                          aes(x= Modularityrand)
) +
  geom_histogram(bins = 14,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Modularity") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(-0.25,-0.12, 0.02),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = -0.165),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=-0.159,
            y=0.4,
            label="-0.165") + 
  coord_cartesian(ylim = c(0, 0.5), xlim = c(-0.25, -0.12)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Clustering Coefficient (CC) in the shuffle") 
modularity_hist

pvalue_2(sim_res_dataset6$Modularityrand, -0.152)
