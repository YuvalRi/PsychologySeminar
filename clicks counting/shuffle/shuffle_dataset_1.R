source("measures/shuffle/measure_shuffle_dataset1_female.R")
source("clicks counting/dataset1_female.R")
library(tidyverse)
library(dplyr)
library(igraph)
library(writexl)
library(ggplot2)
library(httpgd)

#' The simulation creates random graphs.
#' In each graph, for each participant
#' both number of mutual clicks and
#' number of neighbors were calculated.
#' The simulation returns a data frame
#' with the results of the calculations.
simulation <- function(dataset) {
  n_sim <- 10000
  mutual_random <- rep(0, 100000)
  neighbors_random <- rep(0, 100000)
  dataset <- name_to_number(dataset)
  for (i in 1:n_sim) {
    if (i %% 1000 == 0) 
      cat("Progress: ", i / 100, "%\n")
    dataset[, 3] <- sample_values(dataset)
    edges <- creating_edges(dataset)
    graph <- graph(edges, directed = TRUE)
    num_of_vertices <- gorder(graph)
    for (v in 1:num_of_vertices) {
      index <- (i - 1) * num_of_vertices + v
      mutual_random[index] <- mutual_clicks(dataset, v)
      neighbors_random[index] <- length(neighbors_directed(dataset, v))
    }
  }
  df <- data.frame(mutual_random, neighbors_random)
  colnames(df) <- c("mutual clicks", "participant's clicks")
  return(df)
}

# Origin dataset
origin_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)

dataset_1 <- origin_dataset[, c(1, 2, 25)]
dataset_1 <- arrange(dataset_1, Subject)
sim_results <- simulation(dataset_1)

write_xlsx(sim_results, "C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset1.xlsx")
sim_results_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset1.csv")

# Analysis 1 - mean of the required proportion (prop1 mean of every 10 rows)
mean_prop_vec <- sim_results_dataset[1: 10000, 4]
pv_right_tail(mean_prop_vec, 0.644)

data_for_hists <- sim_results_dataset[1:10000, ]

hgd()
hgd_browse()
# Analysis 1 - Histogram
hist_1 <- ggplot(data_for_hists,
                 aes(x = as.numeric(mean.prop))) +
  geom_histogram(bins = 20,
                 fill = "gray63",
                 colour = "black",
                 aes(y = after_stat(count / sum(count))))+
  theme_bw() +
  ylab("Frequency") +
  xlab("Hav to complete") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.644, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
          x = 0.7,
          y = 0.18,
          label = "0.644",
          color = "black",
          size = 5) +
  labs(x = "", y = "Frequency", size = 34, family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.2), xlim = c(0.1, 1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
hist_1

# Analysis 2 - mean of the required proportion (mutual clicks mean of every 10 rows)
mean_prop_vec <- sim_results_dataset[1: 10000, 5]
pv_right_tail(mean_prop_vec, 2.8)

hist_2 <- ggplot(data_for_hists,
                 aes(x = as.numeric(mean.mutual.clicks))) +
  geom_histogram(bins = 14,
                 fill = "gray63",
                 colour = "black",
                 aes(y = after_stat(count / sum(count))))+
  theme_bw() +
  ylab("Frequency") +
  xlab("Hav to complete") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 2.8, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
          x = 3,
          y = 0.25,
          label = "2.8",
          color = "black",
          size = 5) +
  labs(x = "", y = "Frequency", size = 34, family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.3), xlim = c(0.5, 4)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
hist_2