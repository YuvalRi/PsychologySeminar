source("measures/shuffle/measure_shuffle_dataset3_45_females.R")
source("clicks counting/dataset3_45_females.R")
library(tidyverse)
library(dplyr)
library(igraph)
library(writexl)
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

dataset_3 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45females_subset.csv")

sim_results <- simulation(dataset_3)

write_xlsx(sim_results, "C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset3.xlsx")
sim_results_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset3.csv")

# Analysis 1 - mean of the required proportion (prop1 mean of every 10 rows)
mean_prop_vec <- sim_results_dataset[1: 10000, 4]
pv_right_tail(mean_prop_vec, 0.462)

data_for_hists <- sim_results_dataset[1:10000, ]

hgd()
hgd_browse()
# Analysis 1 - Histogram
hist_1 <- ggplot(data_for_hists,
                 aes(x = as.numeric(mean.prop.1))) +
  geom_histogram(bins = 20,
                 fill = "gray63",
                 colour = "black",
                 aes(y = after_stat(count / sum(count)))) +
  theme_bw() +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.462, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
          x = 0.57,
          y = 0.16,
          label = "0.462",
          color = "black",
          size = 5) +
  labs(x = "Ratio of mutual clicks to\n clicking experiences", y = "Frequency", size = 34, family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.2), xlim = c(0.1, 1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
hist_1

# Analysis 2 - mean of the required proportion (mutual clicks mean of every 10 rows)
mean_prop_vec <- sim_results_dataset[1: 10000, 5]
pv_right_tail(mean_prop_vec, 1.8)

hist_2 <- ggplot(data_for_hists,
                 aes(x = as.numeric(mean.mutual.clicks))) +
  geom_histogram(bins = 9,
                 fill = "gray63",
                 colour = "black",
                 aes(y = after_stat(count / sum(count)))) +
  theme_bw() +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0.5, 3.5, 0.5),
                    expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.8, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
          x = 1.5,
          y = 0.42,
          label = "1.8",
          color = "black",
          size = 5) +
  labs(x = "Mutual clicks", y = "Frequency", size = 34, family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.45), xlim = c(0.4, 3.5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
hist_2

