##########################################
###             Lab 5                  ###
##########################################

library(ggfortify)
library(e1071)
library(class)
library(psych)
library(caret)
wine <- read.csv("wine.data", header = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

# Part 1
selected_features <- c("Alcohol", "Malic acid", "Flavanoids", "Proanthocyanins", "Hue")
X <- wine[, selected_features]
y <- as.factor(wine$Type)

set.seed(42)
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# SVM Classifier (Linear)
linear_tune <- tune.svm(x = X_train, y = y_train,
                        kernel = "linear", 
                        cost = 10^seq(-2, 2, 1))

best_linear_model <- linear_tune$best.model
best_linear_params <- linear_tune$best.parameters

linear_predictions <- predict(best_linear_model, X_test)

# Evaluate the linear model's performance
linear_conf_matrix <- confusionMatrix(data = linear_predictions, reference = y_test)
linear_metrics <- linear_conf_matrix$byClass
cat("Linear Kernel Results:\n")
print(linear_conf_matrix)

# SVM Classifier (Radial)
radial_tune <- tune.svm(x = X_train, y = y_train,
                        kernel = "radial", 
                        cost = 10^seq(-2, 2, 1), 
                        gamma = 10^seq(-2, 2, 1))

best_radial_model <- radial_tune$best.model
best_radial_params <- radial_tune$best.parameters

radial_predictions <- predict(best_radial_model, X_test)

# Evaluate the radial model's performance
radial_conf_matrix <- confusionMatrix(data = radial_predictions, reference = y_test)
radial_metrics <- radial_conf_matrix$byClass
cat("\nRadial Kernel Results:\n")
print(radial_conf_matrix)

# kNN Classifier
X_train_knn <- scale(X_train)
X_test_knn <- scale(X_test)

k_values <- seq(1, 15, 2)
knn_accuracies <- sapply(k_values, function(k) {
  knn_predictions <- knn(train = X_train_knn, test = X_test_knn, cl = y_train, k = k)
  mean(knn_predictions == y_test)
})
best_k <- k_values[which.max(knn_accuracies)]

knn_predictions <- knn(train = X_train_knn, test = X_test_knn, cl = y_train, k = best_k)

# Evaluate the kNN model's performance
knn_conf_matrix <- confusionMatrix(data = knn_predictions, reference = y_test)
knn_metrics <- knn_conf_matrix$byClass
cat("\nkNN Results (k =", best_k, "):\n")
print(knn_conf_matrix)

# plot
library(ggplot2)
# Scatter plot colored by wine type
ggplot(wine, aes(x = Alcohol, y = `Malic acid`, color = as.factor(Type))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Alcohol vs Malic Acid (Wine Types Colored)",
       x = "Alcohol",
       y = "Malic Acid",
       color = "Wine Type") +
  theme_minimal()

# Split the data into training and test sets
set.seed(42)
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- scale(X[train_indices, ])
X_test <- scale(X[-train_indices, ])
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Compute kNN accuracy for different values of k
k_values <- seq(1, 20, 2)
accuracies <- sapply(k_values, function(k) {
  knn_predictions <- knn(train = X_train, test = X_test, cl = y_train, k = k)
  mean(knn_predictions == y_test)
})

# Plot kNN accuracy
accuracy_data <- data.frame(k = k_values, Accuracy = accuracies)
ggplot(accuracy_data, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "kNN Accuracy vs Number of Neighbors (k)",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()

library(factoextra)
# Compute total within-cluster sum of squares (WSS) for different k
set.seed(42)
k_values <- 1:10
wss <- sapply(k_values, function(k) {
  kmeans(X, centers = k, nstart = 10)$tot.withinss
})

# Plot K-Means elbow plot
elbow_data <- data.frame(k = k_values, WSS = wss)
ggplot(elbow_data, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "K-Means Elbow Plot",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

#Part 2
library(e1071)
library(ggplot2)
library(caret)

ny_housing <- read.csv("NY-House-Dataset.csv")
colnames(ny_housing)[colnames(ny_housing) == "PROPERTYSQFT"] <- "Square_Footage"
ny_housing <- na.omit(ny_housing)
str(ny_housing)

# Split the dataset into training and testing sets
set.seed(42)
train_indices <- createDataPartition(ny_housing$PRICE, p = 0.7, list = FALSE)
train_data <- ny_housing[train_indices, ]
test_data <- ny_housing[-train_indices, ]

# Train the SVM regression model
svm_model <- svm(PRICE ~ Square_Footage, data = train_data, kernel = "radial", cost = 10, gamma = 0.1)

# Predict on the test set
svm_predictions <- predict(svm_model, test_data)

# Calculate RMSE for the SVM model
svm_rmse <- sqrt(mean((svm_predictions - test_data$PRICE)^2))
cat("SVM Regression RMSE:", svm_rmse, "\n")

# Plot predicted vs. real prices for the SVM model
ggplot(test_data, aes(x = PRICE, y = svm_predictions)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "SVM Regression: Predicted vs Real Prices",
       x = "Real Price",
       y = "Predicted Price") +
  theme_minimal()

# Train the linear regression model
lm_model <- lm(PRICE ~ Square_Footage, data = train_data)

# Predict on the test set
lm_predictions <- predict(lm_model, test_data)

# Calculate RMSE for the linear regression model
lm_rmse <- sqrt(mean((lm_predictions - test_data$PRICE)^2))
cat("Linear Regression RMSE:", lm_rmse, "\n")

# Plot predicted vs. real prices for the linear regression model
ggplot(test_data, aes(x = PRICE, y = lm_predictions)) +
  geom_point(color = "green", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Linear Regression: Predicted vs Real Prices",
       x = "Real Price",
       y = "Predicted Price") +
  theme_minimal()

# Print RMSE values for comparison
cat("\nModel Performance Comparison:\n")
cat("SVM Regression RMSE:", svm_rmse, "\n")
cat("Linear Regression RMSE:", lm_rmse, "\n")
