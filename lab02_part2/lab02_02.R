####### Data Analytics Fall 2024 Lab 02 Part 02######

library(ggplot2)

### set working directory
setwd("/Users/henryliu/Documents/GitHub/DataAnalytics2024_Henry_Liu/lab02_part2/")

### read in data
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old')) 

### Exercise 1
# Feature Subsets
feature_set_1 <- abalone[, c("length", "diameter", "height")]
feature_set_2 <- abalone[, c("whole_weight", "shucked_wieght", "shell_weight")]
feature_set_3 <- abalone[, c("length", "whole_weight", "viscera_wieght")]

# Train Naive Bayes Classifier for each feature subset
install.packages("e1071")
library(e1071)
model_1 <- naiveBayes(feature_set_1, abalone$age.group)
model_2 <- naiveBayes(feature_set_2, abalone$age.group)
model_3 <- naiveBayes(feature_set_3, abalone$age.group)

# Make predictions
pred_1 <- predict(model_1, feature_set_1)
pred_2 <- predict(model_2, feature_set_2)
pred_3 <- predict(model_3, feature_set_3)

# Compare models using contingency tables
cat("Contingency Table for Feature Set 1:\n")
table(Predicted = pred_1, Actual = abalone$age.group)
cat("Contingency Table for Feature Set 2:\n")
table(Predicted = pred_2, Actual = abalone$age.group)
cat("Contingency Table for Feature Set 3:\n")
table(Predicted = pred_3, Actual = abalone$age.group)

# Plot the distribution of classes along 3 different features
# Choose "length", "whole_weight", and "shell_weight" for plotting
ggplot(abalone, aes(x=length, fill=age.group)) +
  geom_histogram(binwidth=0.05, position="dodge") +
  labs(title="Distribution of Age Groups by Length", x="Length", y="Count") +
  theme_minimal()

ggplot(abalone, aes(x=whole_weight, fill=age.group)) +
  geom_histogram(binwidth=0.05, position="dodge") +
  labs(title="Distribution of Age Groups by Whole Weight", x="Whole Weight", y="Count") +
  theme_minimal()

ggplot(abalone, aes(x=shell_weight, fill=age.group)) +
  geom_histogram(binwidth=0.05, position="dodge") +
  labs(title="Distribution of Age Groups by Shell Weight", x="Shell Weight", y="Count") +
  theme_minimal()

###Exercise 2
# Load necessary libraries
library(class)
library(ggplot2)

# Load the iris dataset
iris <- read.csv("iris.csv")

# Normalize function
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# Remove the index column "X"
iris <- iris[, -1]

# Features and labels
iris.features <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
iris.labels <- iris[, "Species"]

# Normalize the feature columns
iris.features.norm <- as.data.frame(lapply(iris.features, normalize))

# Splitting data into training and test sets
set.seed(123)
sample.index <- sample(nrow(iris), 0.7 * nrow(iris))  # 70% for training, 30% for testing
iris.train <- iris.features.norm[sample.index, ]
iris.test <- iris.features.norm[-sample.index, ]
iris.train.labels <- iris.labels[sample.index]
iris.test.labels <- iris.labels[-sample.index]

# Subset 1: Sepal Length and Sepal Width
subset_1.train <- iris.train[, c("Sepal.Length", "Sepal.Width")]
subset_1.test <- iris.test[, c("Sepal.Length", "Sepal.Width")]

# Subset 2: Petal Length and Petal Width
subset_2.train <- iris.train[, c("Petal.Length", "Petal.Width")]
subset_2.test <- iris.test[, c("Petal.Length", "Petal.Width")]

# Define values for k
ks <- c(3, 5, 7, 9, 11, 13)

# Initialize accuracy vectors
accuracy_1 <- c()
accuracy_2 <- c()

# kNN analysis for Subset 1: Sepal Length and Sepal Width
for (k in ks) {
  KNNpred_1 <- knn(train = subset_1.train, test = subset_1.test, cl = iris.train.labels, k = k)
  cm_1 <- table(Actual = KNNpred_1, Predicted = iris.test.labels)
  accuracy_1 <- c(accuracy_1, sum(diag(cm_1)) / length(iris.test.labels))
}

# kNN analysis for Subset 2: Petal Length and Petal Width
for (k in ks) {
  KNNpred_2 <- knn(train = subset_2.train, test = subset_2.test, cl = iris.train.labels, k = k)
  cm_2 <- table(Actual = KNNpred_2, Predicted = iris.test.labels)
  accuracy_2 <- c(accuracy_2, sum(diag(cm_2)) / length(iris.test.labels))
}

# Plot accuracy for both feature subsets
plot(ks, accuracy_1, type = "b", col = "blue", ylim = c(min(accuracy_1, accuracy_2), max(accuracy_1, accuracy_2)),
     xlab = "k", ylab = "Accuracy", main = "kNN Accuracy for Feature Subsets")
lines(ks, accuracy_2, type = "b", col = "red")
legend("bottomright", legend = c("Subset 1: Sepal Length & Width", "Subset 2: Petal Length & Width"), 
       col = c("blue", "red"), lty = 1, cex = 0.8)

# Contingency Tables for best k value
best_k_1 <- ks[which.max(accuracy_1)]
best_k_2 <- ks[which.max(accuracy_2)]

cat("Best k for Subset 1: ", best_k_1, "\n")
KNNpred_best_1 <- knn(train = subset_1.train, test = subset_1.test, cl = iris.train.labels, k = best_k_1)
table(Actual = KNNpred_best_1, Predicted = iris.test.labels)

cat("Best k for Subset 2: ", best_k_2, "\n")
KNNpred_best_2 <- knn(train = subset_2.train, test = subset_2.test, cl = iris.train.labels, k = best_k_2)
table(Actual = KNNpred_best_2, Predicted = iris.test.labels)

###Exercise 3
### IRIS
# Load necessary libraries
library(ggplot2)

# Load iris dataset
iris <- read.csv("iris.csv")
iris <- iris[, -1]  # Remove the index column "X"

# Set seed for reproducibility
set.seed(123)

# Try k-means with different values of k
wss_iris <- c()  # Store within-cluster sum of squares
ks_iris <- c(2, 3, 4, 5)

for (k in ks_iris) {
  iris.km <- kmeans(iris[, -5], centers = k)  # Exclude the Species column
  wss_iris <- c(wss_iris, iris.km$tot.withinss)  # Total within-cluster sum of squares
}

# Plot WSS to determine the optimal k
plot(ks_iris, wss_iris, type = "b", col = "blue", xlab = "Number of Clusters", ylab = "WSS", main = "WSS for Different k (Iris Dataset)")

# Perform k-means with the best k (assume k=3)
iris.km <- kmeans(iris[, -5], centers = 3)
assigned.clusters_iris <- as.factor(iris.km$cluster)

# Plot Petal Length vs Petal Width, colored by assigned clusters
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters_iris)) +
  geom_point() +
  labs(title = "Iris Clustering (k = 3)")

# Evaluate clustering by comparing to actual species
labeled.clusters_iris <- as.character(assigned.clusters_iris)
labeled.clusters_iris[labeled.clusters_iris == 1] <- "setosa"
labeled.clusters_iris[labeled.clusters_iris == 2] <- "versicolor"
labeled.clusters_iris[labeled.clusters_iris == 3] <- "virginica"
table(labeled.clusters_iris, iris$Species)

###ABALONE
# Load abalone dataset
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE)
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", "Whole_Weight", "Shucked_Weight", 
                       "Viscera_Weight", "Shell_Weight", "Rings")
abalone <- abalone[, -1]  # Remove the "Sex" column for k-means clustering

# Normalize the numeric columns for clustering
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
abalone.norm <- as.data.frame(lapply(abalone[, -8], normalize))  # Exclude "Rings"

# Set seed for reproducibility
set.seed(123)

# Try k-means with different values of k
wss_abalone <- c()  # Store within-cluster sum of squares
ks_abalone <- c(2, 3, 4, 5)

for (k in ks_abalone) {
  abalone.km <- kmeans(abalone.norm, centers = k)
  wss_abalone <- c(wss_abalone, abalone.km$tot.withinss)
}

# Plot WSS to determine the optimal k
plot(ks_abalone, wss_abalone, type = "b", col = "red", xlab = "Number of Clusters", ylab = "WSS", main = "WSS for Different k (Abalone Dataset)")

# Perform k-means with the best k (assume k=3)
abalone.km <- kmeans(abalone.norm, centers = 3)
assigned.clusters_abalone <- as.factor(abalone.km$cluster)

# Plot Length vs Whole Weight, colored by assigned clusters
ggplot(abalone, aes(x = Length, y = Whole_Weight, colour = assigned.clusters_abalone)) +
  geom_point() +
  labs(title = "Abalone Clustering (k = 3)")
