# Load necessary libraries
library(dplyr)
library(caret)

# Load the dataset
data <- read.csv("D:/Project-VH/titanic_data_Cleaned.csv")

# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (70% training, 30% testing)
train_indices <- sample(nrow(data), 0.7* nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit a logistic regression model on the training data
model <- glm(Survived ~ ., data = train_data, family = binomial)

# Make predictions on the test data
test_predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to class labels (0 or 1)
predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)

# Compare predicted classes with actual response to calculate accuracy
accuracy <- mean(predicted_classes == test_data$Survived)
print(paste("Accuracy:", accuracy))

predicted_classes <- factor(predicted_classes, levels = c(0, 1))
test_data$Survived <- factor(test_data$Survived, levels = c(0, 1))

# Confusion Matrix
conf_matrix <- confusionMatrix(predicted_classes, test_data$Survived)
print(conf_matrix)