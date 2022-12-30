library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)
library(httpgd)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for (j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i, j] == "W396") {
        data[i, j] <- "1"
      } else if (data[i, j] == "W515") {
        data[i, j] <- "2"
      } else if (data[i, j] == "W686") {
        data[i, j] <- "3"
      } else if (data[i, j] == "W717") {
        data[i, j] <- "4"
      } else if (data[i, j] == "W755") {
        data[i, j] <- "5"
      } else if (data[i, j] == "W756") {
        data[i,j] <- "6"
      }
    }
  }
  return(data)
}

# input - data frame, output - vector of characters
creating_edges <- function(data) {
  edges_vec <- c()
  for (i in 1:nrow(data)) {
    if (data[i, 3] == 1) {
      edges_vec <- append(edges_vec, data[i, 1])
      edges_vec <- append(edges_vec, data[i, 2])
    }
  }
  return(edges_vec)
}

# return a permutation of 0,1 where 1 represent an edges and 0 represent no edge
sample_values <- function(data) {
  # original vector
  g_vec <- data$click0no1yes
  n <- length(g_vec)
  new_vec <- c()
  # sample permutations between [0,1] with length n
  seq_vec <- sample(c(1:n), n, replace = FALSE)
  for (i in 1:length(seq_vec)) {
    new_vec[i] <- g_vec[seq_vec[i]]
  }
  return(new_vec)
}

pv_right_tail <- function(data, real_value) {
  vec <- ifelse(data < real_value, 1, 0)
  return(1 - mean(vec))
}

pv_left_tail <- function(data, real_value) {
  vec <- ifelse(data > real_value, 0, 1)
  return(mean(vec))
}

#shuffle
n_sim <- 120000
cc <- c()
aspl_directed <- c()
aspl_undirected <- c()
diameter_directed <- c()
mod <- c()
sim_1 <- function(data) {
  data <- name_to_number(data)
  for (i in 1:n_sim){
    data[, 3] <- sample_values(data)
    edges <- creating_edges(data)
    g <- graph(edges, directed = TRUE)
    if (are.connected(g, 1, 3) || are.connected(g, 3, 1)) {
      next
    }
    cc[i] <- transitivity(g, type = c("global")) # calculating global CC
    aspl_directed[i] <- mean_distance(g, directed = TRUE)
    aspl_undirected[i] <- mean_distance(g, directed = FALSE)
    diameter_directed[i] <- diameter(g, directed = TRUE)
    mod[i] <- modularity(g, membership = V(graph = g), directed = FALSE) # calculating modularity
  }
  df <- data.frame(cc, aspl_directed, aspl_undirected, diameter_directed, mod)
  colnames(df) <- c("CCrand","ASPL_directed_rand", "ASPL_undirected_rand", "Diameter_directed_rand", "Modularityrand")
  return(df)
}

# df
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
sim_results <- sim_1(small_female)

library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//measures//shuffle_dataset5_small_female_new_2.xlsx")

# simulation data set
sim_res_dataset5 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset5_small_female_new_2.csv")

hgd()
hgd_browse()
cc_hist <- ggplot(sim_res_dataset5,
                  aes(x = CCrand)) +
  geom_histogram(bins = 14,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Clustering Coefficient (CC)") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0.1, 1, 0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.846, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = 0.93,
            y = 0.3,
            label = "0.846",
            color = "black",
            size = 5) +
  labs(x = "Global CC",
         y = "Frequency",
         size = 34,
         family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.35), xlim = c(0.09, 1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
cc_hist

pv_right_tail(sim_res_dataset5$CCrand, 0.846)

aspl_directed_hist <- ggplot(sim_res_dataset5,
                             aes(x = ASPL_directed_rand)) +
  geom_histogram(bins = 10,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - directed graph") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(1, 2.1, 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.4, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = 1.47,
            y = 0.42,
            label = "1.4",
            color = "black",
            size = 5) +
  labs(x = "ASPL - directed graph",
         y = "Frequency",
         size = 34,
         family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0.99, 2.1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
aspl_directed_hist

pv_left_tail(sim_res_dataset5$ASPL_directed_rand, 1.400)

aspl_undirected_hist <- ggplot(sim_res_dataset5,
                               aes(x = ASPL_undirected_rand)) +
  geom_histogram(bins = 10,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - undirected graph") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(1, 1.7, 0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.2, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = 1.25,
            y = 0.45,
            label = "1.2",
            color = "black",
            size = 5) +
  labs(x = "ASPL - undirected graph",
         y = "Frequency",
         size = 34,
         family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.51), xlim = c(0.99, 1.7)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
aspl_undirected_hist

pv_left_tail(sim_res_dataset5$ASPL_undirected_rand, 1.200)

diameter_directed_hist <- ggplot(sim_res_dataset5,
                                aes(x = Diameter_directed_rand)) +
  geom_histogram(bins = 4,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Diameter - directed graph") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(1, 6, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 2, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = 2.2,
            y = 0.85,
            label = "2",
            color = "black",
            size = 5) +
  labs(x = "Diameter - directed graph",
       y = "Frequency",
       size = 34,
      family = "Helvetica") +
  coord_cartesian(ylim = c(0, 1), xlim = c(1, 6)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
diameter_directed_hist

pv_left_tail(sim_res_dataset5$Diameter_directed_rand, 2)

modularity_hist <- ggplot(sim_res_dataset5,
                          aes(x = Modularityrand)) +
  geom_histogram(bins = 14,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Modularity") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(-0.22, -0.16, 0.01),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = -0.180, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = -0.173,
            y = 0.29,
            label = "-0.180",
            color = "black",
            size = 5) +
  labs(x = "Modularity",
       y = "Frequency",
       size = 34,
       family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.31), xlim = c(-0.215, -0.16)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
modularity_hist

pv_left_tail(sim_res_dataset5$Modularityrand, -0.180)