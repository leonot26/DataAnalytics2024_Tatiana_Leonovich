wine <- read.csv("wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)
wine <- na.omit(wine)
library(tidyverse)
library(ggplot2)
library(e1071)    
library(class)    
library(caret)    
library(datasets) 
library(MASS)     

set.seed(123)

# Split the dataset into training and testing sets
split <- createDataPartition(wine$Type, p = 0.7, list = FALSE)
train_data <- wine[split, ]
test_data <- wine[-split, ]

# Feature subset based on previous analysis (e.g., first 5 features)
selected_features <- c("Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium")
train_x <- train_data[, selected_features]
test_x <- test_data[, selected_features]
train_y <- train_data$Type
test_y <- test_data$Type

# Step 1: Train SVM with linear kernel
svm_linear <- tune.svm(
  Type ~ ., data = train_data[, c(selected_features, "Type")],
  kernel = "linear", cost = 10^(-1:2)
)
best_svm_linear <- svm_linear$best.model

# Step 2: Train SVM with radial kernel
svm_radial <- tune.svm(
  Type ~ ., data = train_data[, c(selected_features, "Type")],
  kernel = "radial", cost = 10^(-1:2), gamma = 10^(-2:1)
)
best_svm_radial <- svm_radial$best.model

# Step 3: Train a kNN classifier
k <- 3  # Choose k based on experimentation
knn_pred <- knn(
  train = scale(train_x),
  test = scale(test_x),
  cl = train_y,
  k = k
)

# Evaluate SVM (linear and radial) and kNN
svm_linear_pred <- predict(best_svm_linear, test_x)
svm_radial_pred <- predict(best_svm_radial, test_x)

# Combine predictions for evaluation
predictions <- list(
  SVM_Linear = svm_linear_pred,
  SVM_Radial = svm_radial_pred,
  kNN = knn_pred
)

# Calculate performance metrics
# Ensure levels are consistent between test_y and predictions
results <- lapply(predictions, function(pred) {
  pred <- factor(pred, levels = levels(factor(test_y)))  # Match levels to test_y
  confusionMatrix(pred, factor(test_y), mode = "prec_recall")
})


# Display performance
for (model in names(results)) {
  cat("\nPerformance metrics for", model, ":\n")
  print(results[[model]]$byClass)  # Precision, Recall, F1
}

# Step 4: Compare models
# Summarize performance metrics for easier comparison
comparison <- do.call(rbind, lapply(results, function(x) x$byClass))
print(comparison)

#NY HOUSE data
house <- read.csv('NY-House-Dataset.csv')
head(house)
house<- na.omit(house)
head(house)
# Data Cleaning: Ensure PRICE and PROPERTYSQFT are numeric
house$PRICE <- as.numeric(house$PRICE)
house$PROPERTYSQFT <- as.numeric(house$PROPERTYSQFT)
house <- house[house$PRICE > 0 & house$PROPERTYSQFT > 0, ]

# Split the dataset into training and testing sets
set.seed(123)
split <- createDataPartition(house$PRICE, p = 0.7, list = FALSE)
train_data <- house[split, ]
test_data <- house[-split, ]

#Train SVM regression model
svm_model <- svm(
  PRICE ~ PROPERTYSQFT, data = train_data,
  kernel = "radial", cost = 10, gamma = 0.1
)

# Predict prices using the SVM model
svm_predictions <- predict(svm_model, test_data)

# Train linear regression model
linear_model <- lm(PRICE ~ PROPERTYSQFT, data = train_data)

# Predict prices using the linear model
linear_predictions <- predict(linear_model, test_data)

# Plot predicted price vs real price for SVM
ggplot(data = NULL, aes(x = test_data$PRICE, y = svm_predictions)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "SVM: Predicted Price vs Real Price",
    x = "Real Price",
    y = "Predicted Price"
  ) +
  theme_minimal()

# Plot predicted price vs real price for Linear Regression
ggplot(data = NULL, aes(x = test_data$PRICE, y = linear_predictions)) +
  geom_point(color = "green") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Linear Regression: Predicted Price vs Real Price",
    x = "Real Price",
    y = "Predicted Price"
  ) +
  theme_minimal()

