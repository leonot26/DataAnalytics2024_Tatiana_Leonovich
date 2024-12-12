facebook_economy <- read.csv("Facebook_Economy.csv")
head(facebook_economy)

# Replacing -1 with NA to handle missing data
facebook_economy[facebook_economy == -1] <- NA
# Calculate summary statistics for the dataset
facebook_economy <- na.omit(facebook_economy)
summary_stats <- summary(facebook_economy)
print(summary_stats)
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)

library(cluster)
library(dplyr)

# Plotting histograms for the first few time series columns (TS1, TS2, TS3)
ggplot(facebook_economy, aes(x = TS1)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of TS1", x = "TS1 Value", y = "Frequency")

# Boxplot for TS1 to visualize outliers
ggplot(facebook_economy, aes(y = TS1)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Boxplot for TS1", y = "TS1 Value")

# Outlier detection using IQR for TS1
Q1 <- quantile(facebook_economy$TS1, 0.25)
Q3 <- quantile(facebook_economy$TS1, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

nrow(facebook_economy %>% filter(IDLink == 1))
unique(facebook_economy$IDLink)


# Removing outliers for TS1
facebook_economy_no_outliers <- facebook_economy[facebook_economy$TS1 >= lower_bound & facebook_economy$TS1 <= upper_bound, ]

# Check the structure of the data
str(facebook_economy_no_outliers)

# Summary statistics for numerical variables
summary(facebook_economy_no_outliers)

# Check for missing values
colSums(is.na(facebook_economy_no_outliers))

names(facebook_economy_no_outliers)
# Creating a categorical variable from TS1
facebook_economy_no_outliers$category_var <- factor(facebook_economy_no_outliers$TS1)



# Scale numerical variables
facebook_economy_no_outliers_scaled <- facebook_economy_no_outliers %>%
  mutate(across(where(is.numeric), scale))

# Convert categorical variables 
 facebook_economy_no_outliers$category_var <- as.factor(facebook_economy_no_outliers$category_var)
 
 head(facebook_economy_no_outliers)
 

 model_data <- facebook_economy_no_outliers %>%
   select(starts_with("TS"), category_var)  # Use time series and category variables
 
 # Split into training and test datasets
 set.seed(123)
 trainIndex <- createDataPartition(model_data$category_var, p = 0.8, list = FALSE)
 trainData <- model_data[trainIndex, ]
 testData <- model_data[-trainIndex, ]

 # Linear Regression model
 lm_model <- lm(TS1 ~ TS2 + TS3 + TS4, data = trainData)
 summary(lm_model)
 
 # Make predictions on test data
 lm_predictions <- predict(lm_model, testData)
 
 # Evaluate performance (using RMSE)
 RMSE(lm_predictions, testData$TS1)
 
 # Scatter plot for TS2 vs. TS1 with regression line
 library(ggplot2)
 
 ggplot(trainData, aes(x = TS2, y = TS1)) +
   geom_point() +
   geom_smooth(method = "lm", color = "blue") +
   labs(title = "Scatter plot of TS2 vs. TS1", x = "TS2", y = "TS1")
 
 # Scatter plot for TS3 vs. TS1 with regression line
 ggplot(trainData, aes(x = TS3, y = TS1)) +
   geom_point() +
   geom_smooth(method = "lm", color = "blue") +
   labs(title = "Scatter plot of TS3 vs. TS1", x = "TS3", y = "TS1")
 
 # Scatter plot for TS4 vs. TS1 with regression line
 ggplot(trainData, aes(x = TS4, y = TS1)) +
   geom_point() +
   geom_smooth(method = "lm", color = "blue") +
   labs(title = "Scatter plot of TS4 vs. TS1", x = "TS4", y = "TS1")
 
 # Residuals vs Fitted plot
 plot(lm_model, which = 1, main = "Residuals vs Fitted")
 
 # Q-Q plot to check for normality of residuals
 plot(lm_model, which = 2, main = "Normal Q-Q")
 
 # Predicted vs Actual plot
 predicted_values <- predict(lm_model, newdata = trainData)
 ggplot(data.frame(Actual = trainData$TS1, Predicted = predicted_values), aes(x = Actual, y = Predicted)) +
   geom_point() +
   geom_abline(slope = 1, intercept = 0, color = "red") +
   labs(title = "Actual vs Predicted", x = "Actual Values", y = "Predicted Values")
 
 
 
 
 # Random Forest Classifier
 rf_model <- randomForest(category_var ~ TS1 + TS2 + TS3 + TS4, data = trainData)
 print(rf_model)
 
 # Make predictions on test data
 rf_predictions <- predict(rf_model, testData)
 
 # Evaluate classification performance using confusion matrix
 confusionMatrix(rf_predictions, testData$category_var)
 
 
 install.packages("pheatmap")
 library(pheatmap)
 
 # Create confusion matrix as a table
 conf_matrix <- confusionMatrix(rf_predictions, testData$category_var)
 
 # Extract confusion matrix as a table
 conf_matrix_table <- as.table(conf_matrix)
 
 # Plot the confusion matrix as a heatmap
 pheatmap(conf_matrix_table, display_numbers = TRUE, main = "Confusion Matrix Heatmap")
 
 # Plot feature importance
 importance_rf <- importance(rf_model)
 importance_df <- data.frame(Feature = rownames(importance_rf), Importance = importance_rf[, 1])
 
 # Plot importance
 library(ggplot2)
 
 ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   coord_flip() +
   labs(title = "Feature Importance from Random Forest Model", x = "Features", y = "Importance") +
   theme_minimal()
 
 
 

