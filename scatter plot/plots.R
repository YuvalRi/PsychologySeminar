library(ggplot2)
library(svglite)
library(httpgd)

## Scatter Plots for self reports

data_for_scatter_plot <- function(data, column) {
  #'Creating a data frame that represents the (x,y) point
  #' in the required scatter plot
  x <- c()
  y <- c()
  z <- c()
  for (i in 1:length(column)) {
    if (i %% 2 != 1) {
      x <- c(x, column[i])
    }
    else {
      y <- c(y, column[i])
    }
  }
  return(data.frame(x,y))
}

sum_consecutive_yuval <- function(column) {
  #'sum every two rows in the original data set
  #' in order to classify the type of click (none/one sided/two sided)
  result <- vector(mode = "numeric", length = (length(column) / 2))
  for (i in 1:(length(result))) {
    result[i] <- column[2 * i - 1] + column[2 * i]
    #cat("index is",i ,"Calculation is:", column[2 * i - 1], "+", column[2 * i], "=", result[i], "\n")
  }
  return(result)
}
 
# original data set- self reports for all dyads
data <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\analysis\\scatter plot\\all_datasets_together.csv")

creating_scatter_plots <- function(data, column) {
  #' Creating scatter plot for a specific column in data
  df_for_plot <- data_for_scatter_plot(data, column)
  sum_vector <- sum_consecutive_yuval(data[, 19])
  df_for_plot$color <- sum_vector
  graph <- ggplot(data = df_for_plot, aes(x = x,
                                y = y)) +
    geom_point(aes(color = as.character(color)), shape=19) +
    #scale_color_brewer(palette = 1,
      #labels = c("No click", "One sided click",
                                  #"Mutual click")) +
    scale_color_manual(values = c("lightskyblue", "dodgerblue1", "dodgerblue4"),
                       labels = c("No click", "One sided click",
                                  "Mutual click"))+
    theme_bw() +
    theme(
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      text = element_text(size = 17)
    ) +
    labs(x = "Subject", y = "Partner") +
    labs(colour = "")
  return(graph)
}

hgd()
hgd_browse()
creating_scatter_plots(data, data$liking)

all_plots <- function(data) {
  #' Creating scatter plots and save them as svg
  #' for all the required columns in the data
  for (i in 3:(ncol(data) - 2)) {
    col_name <- data[, i]
    g <- creating_scatter_plots(data, data[, i])
    ggsave(g, file=paste0("Rplot_",names(data)[i],".svg"), width = 6, height = 4)
  }
}

# activating all_plots
all_plots(data)