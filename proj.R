library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
data <- read.csv('Crop_recommendation (2).csv')
df <- data.frame(data)
head(df)

#data cleaning
sapply(df, function(x) sum(is.na(x)))
glimpse(df)
names(df)
#classes of target variable
table(df$label)

#removal of classes of crops not grown in Nigeria or surrounding regions
# Create a vector containing the names of classes to remove
classes_to_remove <- c("blackgram", "chickpea", "grapes", "jute", "lentil", "mothbeans", "mungbean", "muskmelon", "pomegranate")
# Create a logical index to identify rows with the specified classes in the target variable
logical_index <- !(df$label %in% classes_to_remove)

# Subset your data using the logical index
new_data <- df[logical_index, ]
df <- new_data

#feature engineering

# Define weights for each nutrient ratio
w_n <- 0.4  # Weight for nitrogen
w_p <- 0.3  # Weight for phosphorus
w_k <- 0.3  # Weight for potassium

# Calculate Soil Fertility Index (SFI) using weighted sum
df$Soil_Fertility_Index <- df$N * w_n + df$P * w_p + df$K * w_k

View(df)

#exploratory data analysis

#Plot histogram for Soil_Fertility_Index
ggplot(df, aes(x = Soil_Fertility_Index)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Soil Fertility Index", x = "Soil Fertility Index", y = "Frequency")

#rainfall
ggplot(df, aes(x = rainfall)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Rainfall", x = "Rainfall", y = "Frequency")


# Plot density plot for temperature
ggplot(df, aes(x = temperature)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Temperature", x = "Temperature", y = "Density")

library(ggplot2)

# Create box plots for each numerical feature grouped by label
library(ggplot2)
library(tidyr)


library(ggplot2)
library(tidyr)

# Assuming your dataset is named 'your_data' and categorical target variable is 'label'
# Reshape the data to long format
long_data <- gather(df, key = "feature", value = "value", -label)

palette <- c("blue", "red", "green", "orange", "purple", "yellow", "cyan", "magenta", "brown", "gray", "darkgreen", "pink", "lightblue")
# Plot box plots
ggplot(long_data, aes(x = label, y = value, fill = label)) +
  geom_boxplot(size = 1.5, width = 0.5) +  # Adjust size of boxplot
  scale_fill_manual(values = palette) +  # Customize fill colors
  facet_wrap(~ feature, scales = "free_y", strip.position = "bottom", labeller = labeller(feature = as_labeller(function(x) x))) +
  labs(title = "Box Plots of Numerical Features by Label", x = "Label", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


#correlation matrix
library(corrplot)
df_numeric <- subset (df, select = -c(label))
correlation_matrix <- cor(df_numeric)
corrplot(correlation_matrix, method = "color")

#SCATTER PLOTS
# Create a scatter plot
ggplot(df, aes(x = N, y = Soil_Fertility_Index, color = label)) +
  geom_point() + 
  labs(title = "Scatter Plot of Soil Fertility Index vs. Nitrogen Content (N)",
       x = "Nitrogen Content (N)",
       y = "Soil Fertility Index")

ggplot(df, aes(x = P, y = humidity, color = label)) +
  geom_point() + 
  labs(title = "Scatter Plot of Humidity vs. Phosphorus Content (P)",
       x = "Phosphorus Content (P)",
       y = "Humidity")

#bar chart


# Create a bar chart
ggplot(df, aes(x = label, fill = label)) +
  geom_bar() +  # Customize bar fill color
  labs(title = "Distribution of Categorical Target Variable", x = "Label", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



library(ggplot2)


# Building a Crop Recommendation Model for Precision Agriculture
#Normalizing numeric columns using min-max scaling
numeric_cols <- sapply(df, is.numeric)
df_normalized <- df
df_normalized[, numeric_cols] <- lapply(df_normalized[, numeric_cols], function(x) (x-min(x)) / (max(x) - min(x)))
head(df_normalized)
df <- df_normalized

#Label encoding target variable
df$label <- factor(df$label)
View(Y_Train)

#Split Dataset into training and testing
set.seed((100))
TrainingIndex <- createDataPartition(df$label, p=0.8, list = FALSE)
TrainingSet <- df[TrainingIndex, ]
TestingSet <- df[-TrainingIndex, ]

x_indices <- c(1:7, 9)
y_index <- 8

X_Train <- TrainingSet[, x_indices]
Y_Train <- TrainingSet[, y_index]

X_Test <- TestingSet[, x_indices]
Y_Test <- TestingSet[, y_index]

#Using Random Forest Algorithm
library(randomForest)
Model <- randomForest(x = X_Train,
                      y = Y_Train,
                      mtry = 4,
                      ntree = 501,
                      max_depth = 15,
                      min_samples_leaf = 200,
                      importance = TRUE)
print(Model)

#Making Recommendations
predictions <- predict(Model, newdata = X_Test)

#Accuracy Test
accuracy <- mean(predictions == Y_Test)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Compute evaluation metrics
library(caret)

# Accuracy
accuracy <- confusionMatrix(predictions, Y_Test)$overall["Accuracy"]

# Precision
precision <- confusionMatrix(predictions, Y_Test)$byClass["Precision"]

# Recall (Sensitivity)
recall <- confusionMatrix(predictions, Y_Test)$byClass["Sensitivity"]

# F1-score
f1_score <- confusionMatrix(predictions, Y_Test)$byClass["F1"]

# Print evaluation metrics
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
print(paste("Precision:", round(precision * 100, 2), "%"))
print(paste("Recall:", round(recall * 100, 2), "%"))
print(paste("F1-score:", round(f1_score * 100, 2), "%"))



# Create confusion matrix
conf_matrix <- confusionMatrix(predictions, Y_Test)

# Plot confusion matrix
library(ggplot2)
plot_conf_matrix <- function(conf_matrix) {
  ggplot(data = as.data.frame(conf_matrix$table), aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    labs(title = "Confusion Matrix",
         x = "Predicted Class",
         y = "True Class") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_conf_matrix(conf_matrix)



# Convert categorical predictions to numeric using label encoding
predictions_numeric <- as.numeric(factor(predictions, levels = levels(Y_Test)))

# Create ROC curve
library(pROC)
roc_curve <- roc(as.numeric(factor(Y_Test, levels = levels(Y_Test))), predictions_numeric)

# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve", auc.polygon = TRUE, grid = TRUE)

# Add AUC to plot
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)))






