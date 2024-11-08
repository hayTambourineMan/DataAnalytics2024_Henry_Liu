##########################################
### Principal Component Analysis (PCA) ###
##########################################

# Original code
library(ggfortify)
library(e1071)
library(class)
library(psych)
wine <- read.csv("wine/wine.data", header = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)
wine$Type <- as.factor(wine$Type)
wine <- wine[,-c(4,5,10)]
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

#Compute the PCs and plot the dataset using the 1st and 2nd PC
wine_pca <- prcomp(wine[,-1], center = TRUE, scale. = TRUE)
# PCA summary 
summary(wine_pca)
# Plot
autoplot(wine_pca, data = wine, colour = 'Type', x = 1, y = 2, 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA of Wine Dataset (PC1 vs PC2)") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2")

#Identify the variables that contribute the most to the 1st PC.
loadings <- wine_pca$rotation
# Sort the variables by their absolute contribution to the 1st PC
loadings_pc1 <- sort(abs(loadings[,1]), decreasing = TRUE)
loadings_pc1

#Train a classifier model to predict wine type using the 13 attributes. (SVM)
library(e1071)
library(caret)
# Split data
set.seed(123)
index <- createDataPartition(wine$Type, p = 0.7, list = FALSE)
train_data <- wine[index, ]
test_data <- wine[-index, ]
# Train SVM
svm_model <- svm(Type ~ ., data = train_data, kernel = "linear", cost = 1, scale = TRUE)
svm_pred <- predict(svm_model, test_data)
# Evaluate SVM performance
svm_conf_matrix <- confusionMatrix(svm_pred, test_data$Type)
print(svm_conf_matrix)

#Train a classifier model to predict wine type using the data projected into the first 3PCs.
library(ggfortify)
wine_pca <- prcomp(wine[,-1], center = TRUE, scale. = TRUE)
# first 3 PCs
pca_data <- as.data.frame(wine_pca$x[, 1:3])
pca_data$Type <- wine$Type
# Split the data 
set.seed(123)
index <- createDataPartition(pca_data$Type, p = 0.7, list = FALSE)
train_data <- pca_data[index, ]
test_data <- pca_data[-index, ]
# Train SVM
svm_model <- svm(Type ~ ., data = train_data, kernel = "linear", cost = 1, scale = TRUE)
svm_pred <- predict(svm_model, test_data)
# Evaluate SVM performance
svm_conf_matrix <- confusionMatrix(svm_pred, test_data$Type)
print(svm_conf_matrix)

#Drop the variables least contributing to the 1st PC and rerun PCA.
#Train a classifier model to predict wine type using the data projected into the first 3 PCs after rerunning PCA.
# Drop the least contributing variables to the 1st PC
wine_reduced <- wine[, !(names(wine) %in% c("Color Intensity", "Alcohol", "Magnesium"))]
# Perform PCA
wine_pca_reduced <- prcomp(wine_reduced[,-1], center = TRUE, scale. = TRUE)
# Extract the first 3 principal components from the reduced PCA
pca_data_reduced <- as.data.frame(wine_pca_reduced$x[, 1:3])
pca_data_reduced$Type <- wine$Type
# Split
set.seed(123)
index <- createDataPartition(pca_data_reduced$Type, p = 0.7, list = FALSE)
train_data <- pca_data_reduced[index, ]
test_data <- pca_data_reduced[-index, ]
# Train SVM
svm_model <- svm(Type ~ ., data = train_data, kernel = "linear", cost = 1, scale = TRUE)
svm_pred <- predict(svm_model, test_data)
# Evaluate SVM performance
svm_conf_matrix <- confusionMatrix(svm_pred, test_data$Type)
print(svm_conf_matrix)

#Compare the 3 classification models using contingency tables and prevision/recall/f1 metrics
# Function to calculate precision, recall, and F1 for each class and average them
calculate_multiclass_metrics <- function(conf_matrix) {
  precision <- mean(conf_matrix$byClass[, "Pos Pred Value"], na.rm = TRUE)
  recall <- mean(conf_matrix$byClass[, "Sensitivity"], na.rm = TRUE)
  f1 <- mean(conf_matrix$byClass[, "F1"], na.rm = TRUE)
  return(c(precision, recall, f1))
}
metrics_orig <- calculate_multiclass_metrics(conf_matrix_orig)
metrics_pca <- calculate_multiclass_metrics(conf_matrix_pca)
metrics_reduced <- calculate_multiclass_metrics(conf_matrix_reduced)
comparison <- data.frame(
  Model = c("Original 13 Attributes", "First 3 PCs", "Reduced Dataset's First 3 PCs"),
  Precision = c(metrics_orig[1], metrics_pca[1], metrics_reduced[1]),
  Recall = c(metrics_orig[2], metrics_pca[2], metrics_reduced[2]),
  F1 = c(metrics_orig[3], metrics_pca[3], metrics_reduced[3])
)
# contingency tables for each model
cat("Contingency Tables:\n")
cat("Original 13 Attributes Model:\n")
print(conf_matrix_orig$table)
cat("\nFirst 3 PCs Model:\n")
print(conf_matrix_pca$table)
cat("\nReduced Dataset's First 3 PCs Model:\n")
print(conf_matrix_reduced$table)
cat("\nComparison of Precision, Recall, and F1 Scores:\n")
print(comparison)