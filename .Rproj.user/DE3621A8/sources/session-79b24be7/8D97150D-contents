library(ggplot2)
library(dplyr)
library(plotly)
library(scales)


clustering <- function(data, k) {
  
  # Step 1: Group data by age and calculate total spending
  clustering_data <- data %>%
    group_by(age) %>%
    summarize(total_spending = sum(total, na.rm = TRUE))
  
  # Step 2: Scale the data
  scaling_means <- colMeans(clustering_data[, c("age", "total_spending")])
  scaling_sds <- apply(clustering_data[, c("age", "total_spending")], 2, sd)
  scaled_data <- scale(clustering_data[, c("age", "total_spending")])
  
  # Step 3: Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = k)
  
  
  # Step 4: Add cluster assignments to the data
  clustering_data$cluster <- as.factor(kmeans_result$cluster)
  
  # Step 5: Reverse scaling for centroids
  centroids <- as.data.frame(kmeans_result$centers)
  centroids$age <- centroids$age * scaling_sds["age"] + scaling_means["age"]
  centroids$total_spending <- centroids$total_spending * scaling_sds["total_spending"] + scaling_means["total_spending"]
  centroids$cluster <- factor(1:k)
  
  # Step 6: Plot the data
  plot <- ggplot(clustering_data, aes(x = age, y = total_spending, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_point(data = centroids, aes(x = age, y = total_spending, color = cluster), size = 4) +
    labs(
      title = paste("Clustering of Customers (", k, " Groups)", sep = ""),
      x = "Age",
      y = "Total Spending",
      color = "Cluster"
    ) +
    scale_x_continuous(breaks = seq(min(clustering_data$age), max(clustering_data$age), by = 2)) +
    scale_y_continuous(
      limits = c(0, max(clustering_data$total_spending) * 1.1),  # Ensure y-axis covers all points with padding
      breaks = seq(0, max(clustering_data$total_spending) * 1.1, by = 200000),
      labels = label_comma(),
      expand = c(0, 0)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(ggplotly(plot))
  
  
  
}


# data <- read.csv("grc.csv")
# clustering(data,3)
