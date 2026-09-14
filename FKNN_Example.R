# Load required libraries
library(magrittr)  # For the pipe operator %>%
library(Matrix)    # For sparse matrices (optional)

# Generate example data
set.seed(123)

fknn <- function(train_features, train_labels, test_features, k, m) {

  n_train <- nrow(train_features)
  n_test <- nrow(test_features)
  d <- ncol(train_features) # == ncol(test_features)
  c <- ncol(train_labels)

  do.call(rbind, lapply(1:n_test, function(obs_ind) {
    distances_squared <- apply((train_features - matrix(rep(test_features[obs_ind, ], each = n_train), nrow = n_train))^2, 1, sum)
    nearest_inds <- order(distances_squared, decreasing = TRUE)[1:k]
    distances_squared <- distances_squared[nearest_inds] ^ (- 1 / (m - 1))
    matrix(distances_squared, ncol = k) %*% as.matrix(train_labels[nearest_inds, ]) / sum(distances_squared)
  }))
}

# Training data
n_train <- 10
n_features <- 2
train_features <- matrix(runif(n_train * n_features), ncol = n_features)
train_labels <- matrix(sample(0:1, n_train, replace = TRUE), ncol = 1)

# Test data
n_test <- 5
test_features <- matrix(runif(n_test * n_features), ncol = n_features)

# Parameters for fknn function
k <- 3
m <- 2

# Apply fknn function
predicted_labels <- fknn(train_features, train_labels, test_features, k, m)

# Display results
print("Test Features:")
print(test_features)
print("Predicted Labels:")
print(predicted_labels)
