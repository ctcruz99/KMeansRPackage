compute_kmeans <- function(data, k) {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (!is.numeric(k) || k <= 0) stop("Clusters (k) must be a positive number")

  kmeans(data, centers = k)
}


plot_kmeans <- function(data, kmeans_model) {
  library(ggplot2)

  data$cluster <- as.factor(kmeans_model$cluster)

  ggplot(data, aes(x = data[,1], y = data[,2], color = cluster)) +
    geom_point(size = 4) +
    labs(title = "K-Means Clustering", x = colnames(data)[1], y = colnames(data)[2]) +
    theme_minimal()
}

