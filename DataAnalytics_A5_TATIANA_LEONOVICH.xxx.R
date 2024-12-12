library(tidyverse)
library(ggplot2)

calendar <- read.csv("w2pb-icbu_version_46.csv")
head(calendar)

# Convert relevant columns to numeric (remove commas)
calendar$land_square_feet <- as.numeric(gsub(",", "", calendar$land_square_feet))
calendar$gross_square_feet <- as.numeric(gsub(",", "", calendar$gross_square_feet))
calendar$sale_price <- as.numeric(gsub(",", "", calendar$sale_price))

# Filter out rows with missing or zero sale_price
calendar_clean <- calendar %>% filter(!is.na(sale_price) & sale_price > 0)

summary(calendar_clean$sale_price)
summary(calendar_clean$gross_square_feet)

# Subset data for Manhattan
borough_data <- calendar_clean %>% filter(borough == "MANHATTAN")
summary(borough_data$sale_price)
summary(borough_data$gross_square_feet)

library(ggplot2)

# Histogram of Sale Prices for Manhattan
ggplot(borough_data, aes(x = sale_price)) +
  geom_histogram(binwidth = 100000, fill = "blue", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Sale Prices in Manhattan", x = "Sale Price", y = "Count")

# Boxplot for Sale Price Outliers in Manhattan
ggplot(borough_data, aes(y = sale_price)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sale Price Outliers in Manhattan", y = "Sale Price")

# Scatterplot: Sale Price vs. Gross Square Feet in Manhattan
ggplot(borough_data, aes(x = gross_square_feet, y = sale_price)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sale Price vs. Gross Square Feet in Manhattan", x = "Gross Square Feet", y = "Sale Price")

# Calculate the IQR for Sale Price
iqr_sale_price <- IQR(calendar_clean$sale_price, na.rm = TRUE)

# Calculate the first (Q1) and third (Q3) quartiles
q1 <- quantile(calendar_clean$sale_price, 0.25, na.rm = TRUE)
q3 <- quantile(calendar_clean$sale_price, 0.75, na.rm = TRUE)

# Display the IQR, Q1, and Q3
cat("IQR for Sale Price:", iqr_sale_price, "\n")
cat("Q1 for Sale Price:", q1, "\n")
cat("Q3 for Sale Price:", q3, "\n")

# Calculate the lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr_sale_price
upper_bound <- q3 + 1.5 * iqr_sale_price

cat("Lower bound for outliers:", lower_bound, "\n")
cat("Upper bound for outliers:", upper_bound, "\n")

# Identifying outliers
outliers <- calendar_clean$sale_price[calendar_clean$sale_price < lower_bound | calendar_clean$sale_price > upper_bound]

# Display the outliers
cat("Outliers in Sale Price:", outliers, "\n")


# Fit a linear regression model for Manhattan
model_manhattan <- lm(sale_price ~ gross_square_feet + land_square_feet + year_built + building_class_category, data = borough_data)

# Summary of the model
summary(model_manhattan)

# Subset for Office Buildings in Manhattan
office_data_manhattan <- borough_data %>% filter(building_class_category == "21 OFFICE BUILDINGS")

# Subset for Residential Units in Manhattan
residential_data_manhattan <- borough_data %>% filter(building_class_category == "01 ONE FAMILY DWELLINGS")

# Regression for Office Buildings in Manhattan
office_model_manhattan <- lm(sale_price ~ gross_square_feet + year_built, data = office_data_manhattan)
summary(office_model_manhattan)

# Regression for Residential Units in Manhattan
residential_model_manhattan <- lm(sale_price ~ gross_square_feet + year_built, data = residential_data_manhattan)
summary(residential_model_manhattan)


# Plot Predicted vs Actual
ggplot(borough_data, aes(x = predicted_sale_price, y = sale_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  labs(title = "Predicted vs Actual Sale Prices in Manhattan", x = "Predicted Sale Price", y = "Actual Sale Price")


# Predicted vs. Actual for Office Buildings in Manhattan
office_data_manhattan$predicted <- predict(office_model_manhattan, office_data_manhattan)
ggplot(office_data_manhattan, aes(x = predicted, y = sale_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  labs(title = "Predicted vs. Actual Sale Prices (Office Buildings in Manhattan)", x = "Predicted Sale Price", y = "Actual Sale Price")

# Predicted vs. Actual for Residential Units in Manhattan
residential_data_manhattan$predicted <- predict(residential_model_manhattan, residential_data_manhattan)
ggplot(residential_data_manhattan, aes(x = predicted, y = sale_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  labs(title = "Predicted vs. Actual Sale Prices (Residential Units in Manhattan)", x = "Predicted Sale Price", y = "Actual Sale Price")
# For Office Buildings in Manhattan
office_data_manhattan$predicted <- predict(office_model_manhattan, office_data_manhattan)

# Calculate R-squared for office model
office_r_squared <- summary(office_model_manhattan)$r.squared

# Calculate MAE for office model
office_mae <- mean(abs(office_data_manhattan$sale_price - office_data_manhattan$predicted))

# Calculate RMSE for office model
office_rmse <- sqrt(mean((office_data_manhattan$sale_price - office_data_manhattan$predicted)^2))

# For Residential Units in Manhattan
residential_data_manhattan$predicted <- predict(residential_model_manhattan, residential_data_manhattan)

# Calculate R-squared for residential model
residential_r_squared <- summary(residential_model_manhattan)$r.squared

# Calculate MAE for residential model
residential_mae <- mean(abs(residential_data_manhattan$sale_price - residential_data_manhattan$predicted))

# Calculate RMSE for residential model
residential_rmse <- sqrt(mean((residential_data_manhattan$sale_price - residential_data_manhattan$predicted)^2))

# Print the results
cat("Office Model Performance:\n")
cat("R-squared:", office_r_squared, "\n")
cat("MAE:", office_mae, "\n")
cat("RMSE:", office_rmse, "\n\n")

cat("Residential Model Performance:\n")
cat("R-squared:", residential_r_squared, "\n")
cat("MAE:", residential_mae, "\n")
cat("RMSE:", residential_rmse, "\n")


# Drop any rows with missing values in the categorical target variable (e.g., building_class_category or neighborhood)
borough_data_clean <- borough_data %>% drop_na(building_class_category)

# Convert relevant columns to factors for classification
borough_data_clean$building_class_category <- as.factor(borough_data_clean$building_class_category)

# You can also convert other columns to factors if they are categorical and not numeric
borough_data_clean$neighborhood <- as.factor(borough_data_clean$neighborhood)


set.seed(123)  # Set seed for reproducibility
train_index <- sample(1:nrow(borough_data_clean), 0.8 * nrow(borough_data))

train_data <- borough_data_clean[train_index, ]
test_data <- borough_data_clean[-train_index, ]

library(e1071)

# Train a Naive Bayes model
nb_model <- naiveBayes(building_class_category ~ gross_square_feet + land_square_feet + year_built + residential_units, data = train_data)

# Predict on the test data
nb_pred <- predict(nb_model, test_data)

# Confusion Matrix and Evaluation Metrics
table(nb_pred, test_data$building_class_category)

# Calculate accuracy
accuracy_nb <- sum(nb_pred == test_data$building_class_category) / nrow(test_data)
accuracy_nb


# k-NN

head(train_data)
# Remove rows with missing target variable 'building_class_category'
train_data_clean <- train_data %>% filter(!is.na(building_class_category))
test_data_clean <- test_data %>% filter(!is.na(building_class_category))

# Remove rows with missing values in key numeric columns (e.g., gross_square_feet, land_square_feet)
numeric_columns <- c("gross_square_feet", "land_square_feet", "sale_price", "year_built", "residential_units")

train_data_clean <- train_data_clean %>% filter(complete.cases(train_data_clean[, numeric_columns]))
test_data_clean <- test_data_clean %>% filter(complete.cases(test_data_clean[, numeric_columns]))

# Use the cleaned data with no missing values in numeric columns
knn_train <- train_data_clean[, numeric_columns]
knn_test <- test_data_clean[, numeric_columns]

# Ensure that target labels are selected for the train and test datasets
train_labels <- train_data_clean$building_class_category
test_labels <- test_data_clean$building_class_category

# Run k-NN on cleaned data
library(class)
knn_pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 5)
# Evaluate performance
library(caret)
confusionMatrix(knn_pred, test_labels)


# Make predictions on the full dataset using the residential model
predictions_residential <- predict(residential_model_manhattan, newdata = calendar_clean)

# Add predictions and residuals to the dataset
calendar_clean$predicted_sale_price <- predictions_residential
calendar_clean$residuals <- calendar_clean$sale_price - calendar_clean$predicted_sale_price

# Plot predictions vs actual sale price
ggplot(calendar_clean, aes(x = sale_price, y = predicted_sale_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  labs(title = "Predicted vs. Actual Sale Prices", x = "Actual Sale Price", y = "Predicted Sale Price")

# Plot residuals
ggplot(calendar_clean, aes(x = predicted_sale_price, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs. Predicted Sale Prices", x = "Predicted Sale Price", y = "Residuals")

# Calculate R-squared
rsq <- summary(residential_model_manhattan)$r.squared
cat("R-squared: ", rsq, "\n")

# Remove NA values from residuals before calculating MAE and RMSE
calendar_clean_no_na <- calendar_clean %>% filter(!is.na(residuals))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(calendar_clean_no_na$residuals))
cat("MAE: ", mae, "\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(calendar_clean_no_na$residuals^2))
cat("RMSE: ", rmse, "\n")

# Train the Naive Bayes model on the cleaned dataset
nb_model <- naiveBayes(building_class_category ~ gross_square_feet + land_square_feet + year_built + residential_units, data = calendar_clean)

# Predict on the entire dataset
nb_pred <- predict(nb_model, calendar_clean)

# Confusion Matrix for Naive Bayes Model
conf_matrix_nb <- table(Predicted = nb_pred, Actual = calendar_clean$building_class_category)

# Calculate accuracy
accuracy_nb <- sum(nb_pred == calendar_clean$building_class_category) / nrow(calendar_clean)

# Print results
print(conf_matrix_nb)
print(paste("Accuracy for Naive Bayes: ", accuracy_nb))

