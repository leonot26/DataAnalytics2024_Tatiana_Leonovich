install.packages("ggfortify")
install.packages("class")
install.packages("psych")
library(ggfortify)
library(e1071)
library(class)
library(psych)

wine <- read.csv("wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

#removing na values
wine_clean <- na.omit(wine)
#remove non numeric values
wine_numeric <- wine_clean[, sapply(wine_clean, is.numeric)]
# Scale the data (important for PCA)
wine_scaled <- scale(wine_numeric)

# Perform PCA
pca <- prcomp(wine_scaled)

str(wine_clean$WineType)
colnames(wine_clean)
# Plot the first two principal components
plot(pca$x[, 1], pca$x[, 2], col = wine_clean$Type, 
     xlab = "PC1", ylab = "PC2", main = "PCA Plot (PC1 vs PC2)")

# Add a legend to the plot
legend("topright", legend = unique(wine_clean$Type), 
       col = unique(wine_clean$Type), pch = 1)

#Identifying the variables that contribute the most to the first PC
# Get the loadings (contributions of each variable to the PCs)
loadings <- pca$rotation

# Look at the loadings for the first principal component (PC1)
loadings_pc1 <- loadings[, 1]
print(loadings_pc1)

## Flavanoids, Total phenols, and Od280/od315 of diluted wines are the most negatively contributing variables to PC1,
#suggesting that higher values of these variables are associated with lower values of PC1.
## Malic acid and Nonflavanoid phenols contribute positively to PC1, 
#suggesting that higher values of these variables are associated with higher values of PC1.

# Train a Random Forest model using all 13 attributes
install.packages("randomForest")
library(randomForest)
# Check column names
colnames(wine_clean)

# Remove spaces in column names
colnames(wine_clean) <- gsub("/", "_", colnames(wine_clean))

# Check again
colnames(wine_clean)
model_full <- randomForest(Type ~ ., data = wine_clean)
print(model_full)

wine_data_pcs <- pca$x[, 1:3]

# Combine the first 3 PCs with the Type column
wine_data_with_pcs <- cbind(wine_clean$Type, wine_data_pcs)

# Name the columns appropriately
colnames(wine_data_with_pcs) <- c("Type", paste0("PC", 1:3))

# Ensure that wine_data_with_pcs is a data frame
wine_data_with_pcs <- as.data.frame(wine_data_with_pcs)

# Convert Type to a factor (classification task)
wine_data_with_pcs$Type <- as.factor(wine_data_with_pcs$Type)

# Train the Random Forest model for classification
model_pcs <- randomForest(Type ~ PC1 + PC2 + PC3, data = wine_data_with_pcs)
print(model_pcs)

# Identify the least contributing variables (smallest absolute loadings in PC1)
least_contrib_vars <- names(loadings_pc1[order(abs(loadings_pc1))[1:3]])  # e.g., drop 3 least contributing variables
wine_data_reduced <- wine_clean[, !names(wine_clean) %in% least_contrib_vars]

dim(wine_data_reduced)

# Scale the reduced data
wine_data_reduced_scaled <- scale(wine_data_reduced[, -which(names(wine_data_reduced) == "Type")])

# Perform PCA on the reduced data
pca_reduced <- prcomp(wine_data_reduced_scaled)

# Plot the first two PCs of the reduced dataset
plot(pca_reduced$x[, 1], pca_reduced$x[, 2], col = wine_data_reduced$Type, 
     xlab = "PC1", ylab = "PC2", main = "PCA Plot (Reduced Data, PC1 vs PC2)")
legend("topright", legend = unique(wine_data_reduced$Type), col = unique(wine_data_reduced$Type), pch = 1)

#Training classifier model of reduced data set
# Project the reduced data onto the first 3 PCs
wine_data_reduced_pcs <- pca_reduced$x[, 1:3]
wine_data_with_pcs <- cbind(wine_data_reduced$Type, wine_data_reduced_pcs)
wine_data_with_pcs <- as.data.frame(cbind(wine_data_reduced$Type, wine_data_reduced_pcs))
colnames(wine_data_with_pcs) <- c("Type", "PC1", "PC2", "PC3")
wine_data_with_pcs$Type <- as.factor(wine_data_with_pcs$Type)

# Train a Random Forest model using the first 3 PCs after reducing variables
model_reduced_pcs <- randomForest(Type ~ PC1 + PC2 + PC3, data = wine_data_with_pcs)
print(model_reduced_pcs)

#comparing three prediction models
pred_full <- predict(model_full, wine_clean)
pred_pcs <- predict(model_pcs, wine_data_with_pcs)
pred_reduced_pcs <- predict(model_reduced_pcs, wine_data_with_pcs)
#contingency tables
table_full <- table(Predicted = pred_full, Actual = wine_clean$Type)
table_pcs <- table(Predicted = pred_pcs, Actual = wine_data_with_pcs$Type)
table_reduced_pcs <- table(Predicted = pred_reduced_pcs, Actual = wine_data_with_pcs$Type)

install.packages('caret')
library(caret)


# Calculate precision, recall, and F1 score for each model
confusion_full <- confusionMatrix(table_full)
confusion_pcs <- confusionMatrix(table_pcs)
confusion_reduced_pcs <- confusionMatrix(table_reduced_pcs)

# Print full confusion matrix for each model
print(confusion_full)
print(confusion_pcs)
print(confusion_reduced_pcs)

# Print metrics for all models
true_labels <- wine_clean$Type
levels(pred_full)
levels(true_labels)
pred_full <- factor(pred_full, levels = unique(true_labels))
sum(is.na(pred_full))
sum(is.na(true_labels))
# If predictions are numeric, convert them to factors
pred_full <- factor(pred_full, levels = levels(true_labels))
pred_pcs <- factor(pred_pcs, levels = levels(true_labels))
table(pred_full)
table(pred_pcs)
# Check accuracy for the full model
accuracy_full <- sum(pred_full == true_labels) / length(true_labels)
print(accuracy_full)

# Check accuracy for the PCA model
accuracy_pcs <- sum(pred_pcs == true_labels) / length(true_labels)
print(accuracy_pcs)
# Assuming confusion_full, conn_pcs, and confusion_reduced_pcs are the confusion matrix results from the models.

# Calculate confusion matrix for the full model
confusion_full <- confusionMatrix(pred_full, true_labels)                                                        

# Create confusion matrix for the full model
cm_full <- table(pred_full, true_labels)

# Calculate precision, recall, F1 for each class manually for the full model
precision_class1_full <- cm_full[1, 1] / sum(cm_full[1, ])
recall_class1_full <- cm_full[1, 1] / sum(cm_full[, 1])
f1_class1_full <- 2 * (precision_class1_full * recall_class1_full) / (precision_class1_full + recall_class1_full)

precision_class2_full <- cm_full[2, 2] / sum(cm_full[2, ])
recall_class2_full <- cm_full[2, 2] / sum(cm_full[, 2])
f1_class2_full <- 2 * (precision_class2_full * recall_class2_full) / (precision_class2_full + recall_class2_full)

precision_class3_full <- cm_full[3, 3] / sum(cm_full[3, ])
recall_class3_full <- cm_full[3, 3] / sum(cm_full[, 3])
f1_class3_full <- 2 * (precision_class3_full * recall_class3_full) / (precision_class3_full + recall_class3_full)

# Print results for each class
cat("Full Model - Precision Class 1:", precision_class1_full, "\n")
cat("Full Model - Recall Class 1:", recall_class1_full, "\n")
cat("Full Model - F1 Class 1:", f1_class1_full, "\n")

cat("Full Model - Precision Class 2:", precision_class2_full, "\n")
cat("Full Model - Recall Class 2:", recall_class2_full, "\n")
cat("Full Model - F1 Class 2:", f1_class2_full, "\n")

cat("Full Model - Precision Class 3:", precision_class3_full, "\n")
cat("Full Model - Recall Class 3:", recall_class3_full, "\n")
cat("Full Model - F1 Class 3:", f1_class3_full, "\n")

# Create confusion matrix for the PCA model
cm_pcs <- table(pred_pcs, true_labels)

# Calculate precision, recall, F1 for each class manually for the PCA model
precision_class1_pcs <- cm_pcs[1, 1] / sum(cm_pcs[1, ])
recall_class1_pcs <- cm_pcs[1, 1] / sum(cm_pcs[, 1])
f1_class1_pcs <- 2 * (precision_class1_pcs * recall_class1_pcs) / (precision_class1_pcs + recall_class1_pcs)

precision_class2_pcs <- cm_pcs[2, 2] / sum(cm_pcs[2, ])
recall_class2_pcs <- cm_pcs[2, 2] / sum(cm_pcs[, 2])
f1_class2_pcs <- 2 * (precision_class2_pcs * recall_class2_pcs) / (precision_class2_pcs + recall_class2_pcs)

precision_class3_pcs <- cm_pcs[3, 3] / sum(cm_pcs[3, ])
recall_class3_pcs <- cm_pcs[3, 3] / sum(cm_pcs[, 3])
f1_class3_pcs <- 2 * (precision_class3_pcs * recall_class3_pcs) / (precision_class3_pcs + recall_class3_pcs)

# Print results for each class
cat("PCA Model - Precision Class 1:", precision_class1_pcs, "\n")
cat("PCA Model - Recall Class 1:", recall_class1_pcs, "\n")
cat("PCA Model - F1 Class 1:", f1_class1_pcs, "\n")

cat("PCA Model - Precision Class 2:", precision_class2_pcs, "\n")
cat("PCA Model - Recall Class 2:", recall_class2_pcs, "\n")
cat("PCA Model - F1 Class 2:", f1_class2_pcs, "\n")

cat("PCA Model - Precision Class 3:", precision_class3_pcs, "\n")
cat("PCA Model - Recall Class 3:", recall_class3_pcs, "\n")
cat("PCA Model - F1 Class 3:", f1_class3_pcs, "\n")

# Create confusion matrix for the reduced PCA model
cm_reduced_pcs <- table(pred_reduced_pcs, true_labels)

# Calculate precision, recall, F1 for each class manually for the reduced PCA model
precision_class1_reduced_pcs <- cm_reduced_pcs[1, 1] / sum(cm_reduced_pcs[1, ])
recall_class1_reduced_pcs <- cm_reduced_pcs[1, 1] / sum(cm_reduced_pcs[, 1])
f1_class1_reduced_pcs <- 2 * (precision_class1_reduced_pcs * recall_class1_reduced_pcs) / (precision_class1_reduced_pcs + recall_class1_reduced_pcs)

precision_class2_reduced_pcs <- cm_reduced_pcs[2, 2] / sum(cm_reduced_pcs[2, ])
recall_class2_reduced_pcs <- cm_reduced_pcs[2, 2] / sum(cm_reduced_pcs[, 2])
f1_class2_reduced_pcs <- 2 * (precision_class2_reduced_pcs * recall_class2_reduced_pcs) / (precision_class2_reduced_pcs + recall_class2_reduced_pcs)

precision_class3_reduced_pcs <- cm_reduced_pcs[3, 3] / sum(cm_reduced_pcs[3, ])
recall_class3_reduced_pcs <- cm_reduced_pcs[3, 3] / sum(cm_reduced_pcs[, 3])
f1_class3_reduced_pcs <- 2 * (precision_class3_reduced_pcs * recall_class3_reduced_pcs) / (precision_class3_reduced_pcs + recall_class3_reduced_pcs)

# Print results for each class
cat("Reduced PCA Model - Precision Class 1:", precision_class1_reduced_pcs, "\n")
cat("Reduced PCA Model - Recall Class 1:", recall_class1_reduced_pcs, "\n")
cat("Reduced PCA Model - F1 Class 1:", f1_class1_reduced_pcs, "\n")

cat("Reduced PCA Model - Precision Class 2:", precision_class2_reduced_pcs, "\n")
cat("Reduced PCA Model - Recall Class 2:", recall_class2_reduced_pcs, "\n")
cat("Reduced PCA Model - F1 Class 2:", f1_class2_reduced_pcs, "\n")

cat("Reduced PCA Model - Precision Class 3:", precision_class3_reduced_pcs, "\n")
cat("Reduced PCA Model - Recall Class 3:", recall_class3_reduced_pcs, "\n")
cat("Reduced PCA Model - F1 Class 3:", f1_class3_reduced_pcs, "\n")

