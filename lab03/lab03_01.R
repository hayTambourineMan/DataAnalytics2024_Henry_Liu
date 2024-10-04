####### Data Analytics Fall 2024 Lab 03 Part 01######

library(ggplot2)
library(dplyr)

### set working directory
setwd("/Users/henryliu/Documents/GitHub/DataAnalytics2024_Henry_Liu/lab03/")

# Load the dataset
epi_data <- read.csv("epi2024results_DA_F24_lab03.csv")

# 2 subsets for "Southern Asia" and "Eastern Europe"
southern_asia <- epi_data %>% filter(region == "Southern Asia")
eastern_europe <- epi_data %>% filter(region == "Eastern Europe")

# 1.1
# Plot for Southern Asia
ggplot(southern_asia, aes(x = EPI)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  ggtitle("EPI Distribution - Southern Asia") +
  xlab("EPI") +
  ylab("Density")

# Plot for Eastern Europe
ggplot(eastern_europe, aes(x = EPI)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "green", alpha = 0.5) +
  geom_density(color = "blue", size = 1) +
  ggtitle("EPI Distribution - Eastern Europe") +
  xlab("EPI") +
  ylab("Density")

# 1.2
# QQ plot for Southern Asia
qqnorm(southern_asia$EPI, main = "QQ Plot - Southern Asia (EPI)")
qqline(southern_asia$EPI)

# QQ plot for Eastern Europe
qqnorm(eastern_europe$EPI, main = "QQ Plot - Eastern Europe (EPI)")
qqline(eastern_europe$EPI)

### 2.1
# Choose 5 variables: ECO, BDH, MKP, MHP, and CDF.
linear_model <- lm(EPI ~ ECO + BDH + MKP + MHP + CDF, data = epi_data)
summary(linear_model)

# According to the summary, ECO is the most significant variable (lowest p-value)
ggplot(epi_data, aes(x = ECO, y = BDH)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter plot of ECO vs BDH with fitted line") +
  xlab("ECO") +
  ylab("BDH")

# 2.2
# Subset the data for Eastern Europe
eastern_europe <- epi_data %>% filter(region == "Eastern Europe")

# Fit the linear model for Eastern Europe
linear_model_eastern_europe <- lm(EPI ~ ECO + BDH + MKP + MHP + CDF, data = eastern_europe)

summary_eastern_europe <- summary(linear_model_eastern_europe)
print(summary_eastern_europe)

ggplot(eastern_europe, aes(x = ECO, y = BDH)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter plot of ECO vs BDH with fitted line (Eastern Europe)") +
  xlab("ECO") +
  ylab("BDH")

# 2.2 Answer:
# The model for Eastern Europe provides a better fit.
# Because its data points are more tightly clustered around the regression line
# Also there is less variability compared to the overall dataset.

### 3.1
install.packages("caret")
library(caret)
install.packages("class")
library(class)

# a subset of 5 variables and 3 regions
subset1 <- epi_data %>% 
  filter(region %in% c("Eastern Europe", "Latin America & Caribbean", "Southern Asia")) %>%
  select(ECO, BDH, MKP, MHP, CDF, region)

subset1 <- na.omit(subset1)
subset1$region <- as.factor(subset1$region)
set.seed(123)
trainIndex1 <- createDataPartition(subset1$region, p = 0.7, list = FALSE)
train1 <- subset1[trainIndex1, ]
test1 <- subset1[-trainIndex1, ]

# Train a kNN model (k = 5)
knn_model1 <- knn(train = train1[, -6], test = test1[, -6], cl = train1$region, k = 5)

# Evaluate the model using a contingency matrix
confusion_matrix1 <- confusionMatrix(knn_model1, test1$region)
print(confusion_matrix1)

# Calculate accuracy
accuracy1 <- confusion_matrix1$overall['Accuracy']
print(paste("Accuracy for Model 1:", accuracy1))
# Accuracy for Model 1: 0.583333333333333

# 3.2: Repeat
subset2 <- epi_data %>% 
  filter(region %in% c("Western Europe", "Sub-Saharan Africa", "Greater Middle East")) %>%
  select(ECO, BDH, MKP, MHP, CDF, region)

subset2 <- na.omit(subset2)
subset2$region <- as.factor(subset2$region)
trainIndex2 <- createDataPartition(subset2$region, p = 0.7, list = FALSE)
train2 <- subset2[trainIndex2, ]
test2 <- subset2[-trainIndex2, ]

# Train a kNN model (k = 5)
knn_model2 <- knn(train = train2[, -6], test = test2[, -6], cl = train2$region, k = 5)

# Evaluate the model using a contingency matrix
confusion_matrix2 <- confusionMatrix(knn_model2, test2$region)
print(confusion_matrix2)

# Calculate accuracy
accuracy2 <- confusion_matrix2$overall['Accuracy']
print(paste("Accuracy for Model 2:", accuracy2))
# Accuracy for Model 2: 0.6

# 3.2 Answer:
# Model 2 has a slightly higher accuracy and a higher Kappa value, 
# which means better performance in distinguishing between the regions. 
# I think because the variables chosen for Model 2 provide a slightly clearer distinction between the selected regions.

# 4.1
# Subset 1: "Eastern Europe", "Latin America & Caribbean", "Southern Asia"
subset1 <- epi_data %>%
  filter(region %in% c("Eastern Europe", "Latin America & Caribbean", "Southern Asia")) %>%
  select(ECO, BDH, MKP, MHP, CDF)

# Subset 2: "Western Europe", "Sub-Saharan Africa", "Greater Middle East"
subset2 <- epi_data %>%
  filter(region %in% c("Western Europe", "Sub-Saharan Africa", "Greater Middle East")) %>%
  select(ECO, BDH, MKP, MHP, CDF)

subset1 <- na.omit(subset1)
subset2 <- na.omit(subset2)

set.seed(123)
kmeans_subset1 <- kmeans(subset1, centers = 3, nstart = 25)
kmeans_subset2 <- kmeans(subset2, centers = 3, nstart = 25)

wcss_subset1 <- kmeans_subset1$tot.withinss
wcss_subset2 <- kmeans_subset2$tot.withinss

print(paste("WCSS for Subset 1 (Eastern Europe, Latin America & Caribbean, Southern Asia):", wcss_subset1))
print(paste("WCSS for Subset 2 (Western Europe, Sub-Saharan Africa, Greater Middle East):", wcss_subset2))

# 1.2
# Define a function to compute WCSS for a range of k values
wcss_k <- function(data, max_k) {
  wcss <- numeric(max_k)
  for (k in 2:max_k) {
    model <- kmeans(data, centers = k, nstart = 25)
    wcss[k] <- model$tot.withinss
  }
  return(wcss)
}

max_k <- 10
wcss_values_subset1 <- wcss_k(subset1, max_k)
wcss_values_subset2 <- wcss_k(subset2, max_k)

wcss_df <- data.frame(
  k = 2:max_k,
  Subset1_WCSS = wcss_values_subset1[2:max_k],
  Subset2_WCSS = wcss_values_subset2[2:max_k]
)

# Plot the WCSS for both subsets across different k values
ggplot(wcss_df, aes(x = k)) +
  geom_line(aes(y = Subset1_WCSS, color = "Subset 1 (Eastern Europe, Latin America, Southern Asia)")) +
  geom_line(aes(y = Subset2_WCSS, color = "Subset 2 (Western Europe, Sub-Saharan Africa, Greater Middle East)")) +
  labs(title = "WCSS for different values of k",
       x = "Number of clusters (k)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  scale_color_manual(values = c("blue", "red"), 
                     name = "Region Groups") +
  theme_minimal()

# Answer:
# Based on the WCSS values, Subset 1 has a slightly lower WCSS compared to Subset 2
# means the clusters in Subset 1 are more compact. 
# So, Subset 1 provides a slightly better clustering fit.