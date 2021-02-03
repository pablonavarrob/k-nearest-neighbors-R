# K-nearest neighbours: Graded Task
library(datasets)
library(dplyr)
library(ggfortify)
library(rdist)

######## LOAD TEST DATA SET AND PREPARE IT FOR ANALYSIS #######
# Use the Iris data for testing: has four dimensions of features and also
# comes labeled with the species. 
data("iris")
iris_raw <- select(iris,  Sepal.Length, Sepal.Width,
               Petal.Length, Petal.Width, Species)
# Convert the categorical species column to numerical and rewrite the data set
label <- as.numeric(iris_raw$Species)
categories <- iris_raw$Species
iris <- cbind(iris_raw, label)
# Remove categorical column to avoid problems
iris$Species <- NULL
amount_features <- length(iris[,1])

# The labeled or training data will represent around 70% of the data set, 
# and the unlabeled the remaining. To do so, we perform a random
# permutation of the Iris data set
random_permutation_iris <- iris[sample(nrow(iris)), ]
training_set <- random_permutation_iris[1:100,]
target_set <- random_permutation_iris[101:150,]
# label_permuted_iris <- random_permutation_iris$label
# labels_train <- label_permuted_iris[1:100]
# labels_target <- label_permuted_iris[101:150]
  
############## VARIABLES ##############
K = 5

############## k-NEAREST NEIGHBORS FUNCTION ##############
knnClassify <- function(train_set, k,
                        target_set, metric = 'euclidean') {
  
  # Rearrange input data 
  train_features <- train_set[,-5]
  train_labels <- train_set[,5]
  target_features <- target_set[,-5]
  target_real_labels <- target_set[,5]
  
  indexes <- list()
  predicted_labels <- list ()
  for (i in 1:length(target_set[,1])){
    # Compute distances from point i to all training points
    # and sort them ascending to select the k-nearest
    distances <- cdist(target_features[i,], train_features, metric=metric)
    
    # This orders the indexes of the closest elements 
    k_nearest_neighbors_indexes <- order(distances)[1:K]
    indexes[[i]] <- k_nearest_neighbors_indexes
    predicted_labels[i] <- sort(train_labels[k_nearest_neighbors_indexes],decreasing=TRUE)[1]
    # With the indexes, now I have to figure out the most 
    # repeated label for those indexes
    
  }
  indexes <- do.call(rbind, indexes)
  predicted_labels <- do.call(rbind, predicted_labels)
  return(predicted_labels)
}

labels = knnClassify(training_set, K, target_set)

###### TEST WITH PCA ########
pca_iris <- prcomp(iris[,-5], scale=TRUE)
pca_iris_pcs <- pca_iris$x

plot(pca_iris_pcs[,1], pca_iris_pcs[,2], pch=20, col=label, main="Real Labels",
                                         xlab="PC1", ylab="PC2")


plot(pca_iris_pcs[,1][1:100], pca_iris_pcs[,2][1:100],
     pch=20, col=training_set[,5], main="Predicted Labels", xlab="PC1", ylab="PC2")
points(pca_iris_pcs[,1][101:150], pca_iris_pcs[,2][101:150], pch=20, 
       col=labels)







