epi_results <- epi2024results_DA_F24_lab03
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("class")
library(class)

head(epi_results)
                              ##PART 1

#subsetting the data for each country
pakistan_data <- subset(epi_results, country == "Pakistan")
india_data <- subset(epi_results, country== "India")

#histograms

hist(pakistan_data$ECO, 
     prob = TRUE, 
     main = "Pakistan vs India - Histogram with Density", 
     xlab = "ECO", 
     col = "lightblue", 
     border = "black", 
     breaks = 30,
     xlim = c(0, max(c(pakistan_data$ECO, india_data$ECO)) + 20), 
     ylim = c(0, 0.125) 
)


# Overlay India's histogram
hist(india_data$ECO, 
     prob = TRUE, 
     col = rgb(1, 0, 0, 0.5),    # Semi-transparent red for India
     border = "black", 
     breaks = 30, 
     add = TRUE
)

# Add density line
lines(density(india_data$ECO, pakistan_data$ECO), col = "red", lwd = 2)

# For Pakistan
pakistan_data <- pakistan_data[!is.na(pakistan_data$ECO), ]

# 1. QQ Plot for Pakistan's ECO compared to a normal distribution
qqnorm(pakistan_data$ECO, main = "QQ Plot of Pakistan's ECO vs Normal Distribution")
qqline(pakistan_data$ECO, col = "blue", lwd = 2)

# For India
india_data <- india_data[!is.na(india_data$ECO), ]

# 2. QQ Plot for India's ECO compared to a normal distribution
qqnorm(india_data$ECO, main = "QQ Plot of India's ECO vs Normal Distribution")
qqline(india_data$ECO, col = "red", lwd = 2)



                               ##PART 2
#STEP 2.1 

# Select the five variables and EPI
model_data <- epi_results %>%
  select(EPI, ECO, BDH, MKP, MHP, MPE)

# Fit the linear model
lm_model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = model_data)

# Summary of the model to check significance
summary(lm_model)

# Identify the most significant variable (look for the smallest p-value)
# smallest p value belongs to ECO so it is the most signficant
most_significant_var <- "ECO"  # Change this if needed based on results

# Plot the most significant variable against EPI with the fitted line
ggplot(model_data, aes_string(x = most_significant_var, y = "EPI")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = paste("Scatter Plot of EPI vs", most_significant_var),
       x = most_significant_var,
       y = "EPI")

#calulating r squared
summary_lm_model <- summary(lm_model)
r_squared_full <- summary_lm_model$r.squared
print(paste("R-squared for Model 2.1 (Full Dataset):", r_squared_full))

#STEP 2.2
# Display the unique regions to know what to filter by
unique_regions <- unique(epi_results$region)
print(unique_regions)

# Select a specific region (e.g., "South Asia")
south_asia_data <- epi_results %>%
  filter(region == "Southern Asia")

# Display the head of the filtered dataset to check
head(south_asia_data)

# Fit the linear model for the specific region
lm_region_model <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = south_asia_data)

# Summary of the regional model to check significance
summary(lm_region_model)

#graph
ggplot(south_asia_data, aes(x = ECO, y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "EPI vs. ECO in Southern Asia",
       x = "ECO",
       y = "EPI",
       xlim= c(0,65),
       ylim= c(0,50))
# calculating r squared
summary_lm_region_model <- summary(lm_region_model)
r_squared_full <- summary_lm_region_model$r.squared
print(paste("R-squared for Model 2.1 (Full Dataset):", r_squared_full))

# the region model is a better fit because r=1, higher than the 2.1 model


                                #STEP 3
# step 3.1
selected_variables <- c("ECO", "MKP", "MHP", "SPI", "TBN")

# Filter the dataset for the chosen regions, e.g., Southern Asia, Eastern Europe, and Western Europe
subset_data1 <- epi_results %>%
  filter(region %in% c("Southern Asia", "Eastern Europe", "Global West")) %>%
  select(all_of(selected_variables), region)

# Step 2: Convert the region into a factor
subset_data1$region <- as.factor(subset_data1$region)
subset_data1 <- na.omit(subset_data1)

# Step 3: Split the data into training and testing sets (e.g., 70% train, 30% test)
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(subset_data1), 0.7 * nrow(subset_data1))
train_data1 <- subset_data1[train_index, ]
test_data1 <- subset_data1[-train_index, ]

# Step 4: Train the kNN model
k <- 3  # You can choose an appropriate k value
predicted1 <- knn(train = train_data1[, selected_variables],
                  test = test_data1[, selected_variables],
                  cl = train_data1$region,
                  k = k)

# Step 5: Create a contingency matrix and calculate accuracy
confusion_matrix1 <- table(Predicted = predicted1, Actual = test_data1$region)
accuracy1 <- sum(diag(confusion_matrix1)) / sum(confusion_matrix1)
print(confusion_matrix1)
print(paste("Accuracy for Model 1:", accuracy1))




#step 3.2
# Step 1: Filter the dataset for the new set of regions, e.g., North America, Latin America, and Africa
subset_data2 <- epi_results %>%
  filter(region %in% c("Former Soviet States", "Latin America & Caribbean", "Sub-Saharan Africa")) %>%
  select(all_of(selected_variables), region)

# Convert the region into a factor
subset_data2$region <- as.factor(subset_data2$region)
subset_data2 <- na.omit(subset_data2)

# Step 2: Split the data into training and testing sets (70% train, 30% test)
set.seed(123)  # For reproducibility
train_index2 <- sample(1:nrow(subset_data2), 0.7 * nrow(subset_data2))
train_data2 <- subset_data2[train_index2, ]
test_data2 <- subset_data2[-train_index2, ]

# Step 3: Train the kNN model
predicted2 <- knn(train = train_data2[, selected_variables],
                  test = test_data2[, selected_variables],
                  cl = train_data2$region,
                  k = k)

# Step 4: Create a contingency matrix and calculate accuracy
confusion_matrix2 <- table(Predicted = predicted2, Actual = test_data2$region)
accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)
print(confusion_matrix2)
print(paste("Accuracy for Model 2:", accuracy2))

#model 3.1 has a higher accuracy value from the confusion matrix than the accuracy value from 3.2 meaning it is a more accurate model


                                              #STEP 4
selected_variables4 <- c("EPI", "ECO", "MKP", "MHP", "BDH") 

# Filter for two different groups of regions
group1 <- epi_results %>% filter(region %in% c("Southern Asia", "Eastern Europe", "Asia-Pacific"))
group2 <- epi_results %>% filter(region %in% c("Greater Middle East", "Global West", "Sub-Saharan Africa"))

# Remove rows with NA values
group1_clean <- na.omit(group1[selected_variables])
group2_clean <- na.omit(group2[selected_variables])


# Convert selected variables to numeric
group1_clean <- group1_clean %>%
  mutate(across(everything(), as.numeric))

group2_clean <- group2_clean %>%
  mutate(across(everything(), as.numeric))

# Check for infinite values
if (!is.data.frame(group1_clean) || !is.data.frame(group2_clean)) {
  stop("One of the groups is not a data frame after filtering.")
}
cat("Infinite values in group1:", sum(is.infinite(group1_clean)), "\n")
cat("Infinite values in group2:", sum(is.infinite(group2_clean)), "\n")

# Remove infinite values
group1_clean <- group1_clean[!is.infinite(rowSums(group1_clean)), ]
group2_clean <- group2_clean[!is.infinite(rowSums(group2_clean)), ]

# Function to fit k-means and return WCSS
fit_kmeans <- function(data, k) {
  kmeans_result <- kmeans(data, centers = k)
  return(kmeans_result$tot.withinss)  # WCSS
}

# Set range for k
k_values <- 1:10  # You can adjust this range

# Store WCSS values
wcss_group1 <- sapply(k_values, function(k) fit_kmeans(group1[selected_variables], k))
wcss_group2 <- sapply(k_values, function(k) fit_kmeans(group2[selected_variables], k))

