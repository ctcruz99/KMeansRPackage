library(testthat)
library(KMeansRPackage)

test_that("compute_kmeans works correctly", {
  data <- iris[, 1:2]
  result <- compute_kmeans(data, 3)

  expect_s3_class(result, "kmeans")
  expect_equal(length(unique(result$cluster)), 3)
})

test_that("plot_kmeans generates a ggplot", {
  data <- iris[, 1:2]
  model <- compute_kmeans(data, 3)
  plot <- plot_kmeans(data, model)

  expect_s3_class(plot, "ggplot")
})
