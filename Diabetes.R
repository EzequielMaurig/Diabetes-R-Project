# Install all requiered libraries

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(pROC)) install.packages("pROC")
if(!require(rpart)) install.packages("rpart")
if(!require(class)) install.packages("class")
if(!require(nnet)) install.packages("nnet")
if(!require(e1071)) install.packages("e1071")
if(!require(ada)) install.packages("ada")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(knitr)) install.packages("knitr")
if(!require(NeuralNetTools)) install.packages("NeuralNetTools")
if(!require(janitor)) install.packages("janitor")
if(!require(DT)) install.packages("DT")

# Load Libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(pROC)
library(rpart)
library(class)
library(nnet)
library(e1071)
library(ada)
library(ggthemes)
library(downloader)
library(rpart.plot)
library(knitr)
library("NeuralNetTools")
library(janitor)
library(DT)


theme_set(theme_fivethirtyeight())

##################
### Load Data ###
#################
# Data Load and Wrangling
## Download file from Kaggle "https://www.kaggle.com/datasets/mragpavank/diabetes?resource=download"

diabetes <- read.csv("C:/Users/Usuario/OneDrive/Documentos/Diabetes/Diabetes.csv", stringsAsFactors = FALSE)

#####################
### Data Wrangling ###
#######################

# Set seed for reproducibility
set.seed(15) 

# Split data into training and test set by 80%
diabetes_indices <- createDataPartition(diabetes$Outcome, p = 0.8, list = FALSE)
train_data <- diabetes[diabetes_indices, ]
test_data <- diabetes[-diabetes_indices, ]

# Convert into factors to avoid modeling errors
train_data$Outcome <- as.factor(train_data$Outcome)
test_data$Outcome <- as.factor(test_data$Outcome)

# Check Data Frames Length
if (nrow(test_data) == nrow(train_data)) {
  print("Both data frames have the same length.")
} else {
  print("Data frames have different lengths.")
}

# Check for Missing or Null Values
missing_values_test <- sum(is.na(test_data))
missing_values_train <- sum(is.na(train_data))

cat("Number of missing values in test_data:", missing_values_test, "\n")
cat("Number of missing values in train_data:", missing_values_train, "\n")


###################
### Data Analysis ##
#####################

# View the first rows of the data set
head(diabetes, 5)

# Data structure
str(diabetes)

# Summary 
summary(diabetes)

# Diabetes case count
table(diabetes$Outcome)

# Calculate proportions for each outcome
proportions <- diabetes %>%
  count(Outcome) %>%
  mutate(
    Proportion = n / sum(n),
    Percentage = paste0(round(Proportion * 100, 1), "%") # Add percentage as a label
  )

# Convert Outcome to a factor
proportions$Outcome <- as.factor(proportions$Outcome)

# Pie chart of proportions
ggplot(proportions, aes(x = "", y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", width = 1) + 
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5), size = 5) +
  labs(
    title = "Proportion of Diabetic vs Non-Diabetic Individuals",
    x = "",
    y = "Proportion"
  ) +
  scale_fill_manual(
    values = c("0" = "#219ebc", "1" = "#fb8500"),
    labels = c("0" = "Non-Diabetic", "1" = "Diabetic"),
    name = "Outcome"
  ) +
  coord_polar(theta = "y")+
  theme(plot.title = element_text(hjust = 0.5)) 


# Descriptive Statistics by Diabetes Outcome
summary_stats <- diabetes %>%
  group_by(Outcome) %>%
  summarise(
    Age_mean = mean(Age, na.rm = TRUE),
    Age_sd = sd(Age, na.rm = TRUE),
    Glucose_mean = mean(Glucose, na.rm = TRUE),
    Glucose_sd = sd(Glucose, na.rm = TRUE),
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    Insulin_mean = mean(Insulin, na.rm = TRUE),
    Insulin_sd = sd(Insulin, na.rm = TRUE)
  )

summary_stats

# Correlations
cor(diabetes[, sapply(diabetes, is.numeric)])

# Distribution of BMI among diabetic patients by age group
diabetic_data <- subset(diabetes, Outcome == 1)
ggplot(diabetic_data, aes(x = BMI)) +
  geom_histogram(fill = "#219ebc", bins = 20) + # Replace "#" with a valid color code
  facet_wrap(~ Age) +
  labs(
    title = "BMI Distribution Among Diabetic Patients by Age Group",
    x = "BMI",
    y = "Count"
  ) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )


# Proportion of diabetic patients with elevated insulin levels
diabetes$HighInsulin <- ifelse(diabetes$Insulin > 100, "High Insulin", "Normal Insulin")
diabetic_data <- subset(diabetes, Outcome == 1)

# Calculate proportions for each insulin level category
proportions <- as.data.frame(table(diabetic_data$HighInsulin))
proportions$prop <- proportions$Freq / sum(proportions$Freq)

# Creating the bar chart with proportions above each bar using geom_col
ggplot(proportions, aes(x=Var1, y=prop)) +
  geom_col(fill="#023047") +  # Use geom_col to plot the proportions
  geom_text(aes(label = scales::percent(prop)), vjust = -0.5, size=4) +  # Adding percentages above each bar
  labs(title="Proportion of Diabetic Patients with Elevated Insulin", 
       x="Insulin Level", 
       y="Proportion") + 
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

# Proportion of diabetes cases by age group
ggplot(diabetes, aes(x = Age, fill = factor(Outcome))) +
  geom_bar(aes(y = ..prop.., group = factor(Outcome)), position = "fill", stat = "count") +
  labs(title = "Proportion of Diabetes by Age Group", x = "Age Group", y = "Proportion") +
  scale_fill_manual(values = c("0" = "#219ebc", "1" = "#fb8500"), 
                    name = "Outcome", labels = c("0" = "No Diabetes", "1" = "Diabetes")) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

# Scatter Plot of Age vs Glucose
ggplot(diabetes, aes(x=Age, y=Glucose, color=factor(Outcome))) +
  geom_point(alpha=0.6) +
  labs(title="Age vs Glucose by Diabetes Outcome", x="Age", y="Glucose", color="Diabetes Outcome") +
  scale_color_manual(values=c("0"="#219ebc", "1"="#fb8500"), labels=c("0"="Non-Diabetic", "1"="Diabetic")) +
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color="gray40") +  # Adding a dashed 45-degree line
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

########################
########  Models ########
##########################

#############
## KNN Model ##
################

# Define the number of neighbors
k <- 17

# Train the KNN model with probabilities using knn3
knn_model <- knn3(Outcome ~ ., data = train_data, k = k)

# Predict probabilities for the positive class in the test set
probabilities_knn <- predict(knn_model, newdata = test_data, type = "prob")[, 2]

# Create the confusion matrix
predicted_labels <- ifelse(probabilities_knn > 0.5, 1, 0)
confusion_matrix <- table(Actual = test_data$Outcome, Predicted = predicted_labels)

print(confusion_matrix)

# Calculate the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)  # Accuracy: Proportion of correct predictions
precision <- confusionMatrix(confusion_matrix)$byClass["Pos Pred Value"]  # Precision: Proportion of positive predictions that are correct
recall <- confusionMatrix(confusion_matrix)$byClass["Sensitivity"]  # Recall: Proportion of actual positives correctly identified
f1_score <- confusionMatrix(confusion_matrix)$byClass["F1"]  # F1-Score: Harmonic mean of precision and recall
specificity <- confusionMatrix(confusion_matrix)$byClass["Neg Pred Value"]  # Specificity: Proportion of actual negatives correctly identified
balanced_accuracy <- (recall + specificity) / 2  # Balanced Accuracy: Average of recall and specificity

test_data$Predicted_Outcome <- as.factor(predicted_labels)  

ggplot(test_data, aes(x = Age, y = BMI, color = Predicted_Outcome)) +
  geom_point(size = 3) +
  labs(title = paste("Diabetes Classification with KNN (k =", k, ")"),
       x = "Age", y = "BMI (Body Mass Index)", color = "Prediction") +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )


# Display the results
metrics_knn <- data.frame(Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "Specificity", "Balanced Accuracy"),
                  Value = c(accuracy, precision, recall, f1_score, specificity, balanced_accuracy))  # Create a data frame for the metrics

print(metrics_knn)

# ROC Curve and AUC
roc_curve <- roc(test_data$Outcome, probabilities_knn)  # Compute the ROC curve for the KNN model

# Data for the ROC curve
roc_data <- data.frame(
  tpr = roc_curve$sensitivities,  # True Positive Rate (Sensitivity)
  fpr = 1 - roc_curve$specificities  # False Positive Rate (1 - Specificity)
)

# Plot the ROC curve
roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(color = "#023047", size = 1) +  # Draw the ROC curve line
  geom_abline(linetype = "dashed", color = "grey") +  # Add a dashed diagonal line (no discrimination line)
  labs(title = "ROC Curve - KNN Model",
       x = "1 - Specificity",  # Label for the x-axis
       y = "Sensitivity") +  # Label for the y-axis
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

print(roc_plot)  

# AUC
auc_value_knn <- auc(roc_curve)
cat("AUC:", auc_value_knn, "\n")

#########################
### Decision Tree Model ##
###########################

# Train the decision tree model
decision_tree_model <- rpart(Outcome ~ ., data = train_data, method = "class")

# Make predictions on the test set
predicted_probabilities_tree_model <- predict(decision_tree_model, test_data)[, 2]  # Positive class probabilities
predicted_labels <- ifelse(predicted_probabilities_tree_model > 0.5, 1, 0)

# Create the confusion matrix
confusion_matrix <- table(Actual = test_data$Outcome, Predicted = predicted_labels)
print(confusion_matrix)

# Calculate metrics
TP <- confusion_matrix[2, 2]
FP <- confusion_matrix[1, 2]
TN <- confusion_matrix[1, 1]
FN <- confusion_matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(confusion_matrix)

# Precision
precision <- TP / (TP + FP)

# Recall (Sensitivity)
recall <- TP / (TP + FN)

# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Specificity
specificity <- TN / (TN + FP)

# Balanced Accuracy
balanced_accuracy <- (recall + specificity) / 2

# Create summary table
metrics_decision_tree <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-score", "Specificity", "Balanced Accuracy"),
  Value = c(accuracy, precision, recall, f1_score, specificity, balanced_accuracy)
)

# Print summary table
print(metrics_decision_tree)

# ROC Curve and AUC
roc_curve <- roc(test_data$Outcome, predicted_probabilities_tree_model)
roc_data <- data.frame(
  tpr = roc_curve$sensitivities,
  fpr = 1 - roc_curve$specificities,
  thresholds = roc_curve$thresholds
)

# Plot ROC curve
roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(color = "#780000", size = 1) +
  geom_abline(linetype = "dashed", color = "grey") +
  labs(title = "ROC Curve - Decision Tree Model",
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme(plot.title = element_text(hjust = 0.5))

# Print the ROC plot
roc_plot

# AUC
auc_value_dt <- auc(roc_curve)
cat("AUC:", auc_value_dt, "\n")


# Plot decision tree
rpart.plot(decision_tree_model, main = "Decision Tree - Decision Tree Model",
           type = 3, extra = 101, fallen.leaves = TRUE,
           box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

################
### SVM Model ###
##################

# Train SVM model
svm_model <- svm(Outcome ~ Glucose + BMI, data = train_data, kernel = "linear", probability = TRUE)

# SVM Predictions
svm_pred <- predict(svm_model, test_data)

# Confusion Matrix
svm_cm <- confusionMatrix(svm_pred, test_data$Outcome)
svm_cm


# Calculate metrics
svm_accuracy <- svm_cm$overall["Accuracy"]
svm_precision <- svm_cm$byClass["Pos Pred Value"]
svm_recall <- svm_cm$byClass["Sensitivity"]
svm_f1 <- svm_cm$byClass["F1"]
svm_specificity <- svm_cm$byClass["Neg Pred Value"]
svm_balanced_accuracy <- (svm_recall + svm_specificity) / 2

# Create metrics dataframe
svm_metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "Specificity", "Balanced Accuracy"),
  Value = c(svm_accuracy, svm_precision, svm_recall, svm_f1, svm_specificity, svm_balanced_accuracy)
)
print(svm_metrics)

# ROC Curve
svm_probs <- attr(predict(svm_model, test_data, probability = TRUE), "probabilities")[, 2]
svm_roc <- roc(test_data$Outcome, svm_probs)
plot(svm_roc, main="SVM ROC Curve", col="#ff006e")

auc_value_svm <- auc(svm_roc)
cat("AUC:", auc_value_svm, "\n")

# Create grid of values for Glucose and BMI
svm_grid <- expand.grid(
  Glucose = seq(min(train_data$Glucose), max(train_data$Glucose), length.out = 100),
  BMI = seq(min(train_data$BMI), max(train_data$BMI), length.out = 100)
)

# Predict outcomes for the grid
svm_grid$Outcome <- predict(svm_model, newdata = svm_grid)

# Plot decision boundary
ggplot(svm_grid, aes(x = Glucose, y = BMI, color = Outcome)) +
  geom_tile(aes(fill = Outcome), alpha = 0.5) +
  geom_point(data = train_data, aes(x = Glucose, y = BMI, color = Outcome), size = 3) +
  labs(title = "SVM Decision Boundary", x = "Glucose", y = "BMI") +
  scale_color_manual(values = c("red", "blue"))


###########################
### Neural Network Model ###
#############################

# Preprocess the data by handling missing values and scaling the features
train_data[is.na(train_data)] <- lapply(train_data, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))
test_data[is.na(test_data)] <- lapply(test_data, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))

# Scale features to ensure that the neural network performs well
train_data_scaled <- scale(train_data[, c("Glucose", "BMI", "Age", "Insulin")])
test_data_scaled <- scale(test_data[, c("Glucose", "BMI", "Age", "Insulin")])

# Convert scaled data back to data frame format
train_data <- data.frame(train_data[, "Outcome"], train_data_scaled)
colnames(train_data) <- c("Outcome", "Glucose", "BMI", "Age", "Insulin")
test_data <- data.frame(test_data[, "Outcome"], test_data_scaled)
colnames(test_data) <- c("Outcome", "Glucose", "BMI", "Age", "Insulin")

# Train the neural network model with adjusted parameters
nnet_model <- nnet(Outcome ~ Glucose + BMI + Age + Insulin, 
                   data = train_data, 
                   size = 15,        
                   maxit = 1500,     
                   linout = FALSE,   
                   trace = FALSE,    
                   decay = 0.1)      

# Make predictions on the test set (probabilities)
predicted_probabilities <- predict(nnet_model, newdata = test_data, type = "raw")

# Convert probabilities to binary labels (1 or 0) based on a threshold of 0.5
predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Generate the confusion matrix to evaluate the model's performance
confusion_matrix <- table(Actual = test_data$Outcome, Predicted = predicted_labels)
print(confusion_matrix)

# Calculate metrics
nn_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
nn_precision <- confusionMatrix(confusion_matrix)$byClass["Pos Pred Value"]
nn_recall <- confusionMatrix(confusion_matrix)$byClass["Sensitivity"]
nn_f1_score <- confusionMatrix(confusion_matrix)$byClass["F1"]
nn_specificity <- confusionMatrix(confusion_matrix)$byClass["Neg Pred Value"]
nn_balanced_accuracy <- (nn_recall + nn_specificity) / 2

# Display metrics results
metrics_nn <- data.frame(Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "Specificity", "Balanced Accuracy"),
                         Value = c(nn_accuracy, nn_precision, nn_recall, nn_f1_score, nn_specificity, nn_balanced_accuracy))
print(metrics_nn)

# Generate the ROC curve for the neural network
nnet_roc <- roc(as.numeric(test_data$Outcome), predicted_probabilities)

# Create a data frame with the values of the ROC curve
nnet_roc_data <- data.frame(
  fpr = 1 - nnet_roc$specificities,  
  tpr = nnet_roc$sensitivities,      
  Model = "Neural Network"
)

# Plot ROC curve for NN model
ggplot(nnet_roc_data, aes(x = fpr, y = tpr, color = Model)) +
  geom_line(size = 1.2) +  # Curve line
  geom_abline(linetype = "dashed", color = "grey") +  # Reference line (random)
  labs(title = "ROC Curve - Neural Network",
       x = "1 - Specificity",
       y = "Sensitivity") +
  scale_color_manual(values = c("Neural Network" = "#fca311")) +  # Color of the curve
  theme(plot.title = element_text(hjust = 0.5))

# AUC
auc_value_nn <- auc(nnet_roc)
cat("AUC:", auc_value_nn, "\n")

# NN Plot
NN_plot <- ggplot(train_data, aes(x = Glucose, y = BMI, color = Outcome)) +
  geom_point(size = 3) +
  labs(title = "Glucose vs BMI for Decision Tree Model", x = "Glucose", y = "BMI") +
  scale_color_manual(values = c("red", "blue"))

print(NN_plot)

# Plot Neural Network Model
plotnet(nnet_model)

##################################
#######  Final Comparation  #####
####################################

# Create a dataframe with the metrics of the 4 models
metrics_comparison <- data.frame(
  Model = c("KNN", "Decision Tree", "SVM", "Neural Network"),
  Accuracy = c(metrics_knn$Value[1], metrics_decision_tree$Value[1], svm_metrics$Value[1], metrics_nn$Value[1]),
  Precision = c(metrics_knn$Value[2], metrics_decision_tree$Value[2], svm_metrics$Value[2], metrics_nn$Value[2]),
  Recall = c(metrics_knn$Value[3], metrics_decision_tree$Value[3], svm_metrics$Value[3], metrics_nn$Value[3]),
  F1_Score = c(metrics_knn$Value[4], metrics_decision_tree$Value[4], svm_metrics$Value[4], metrics_nn$Value[4]),
  Specificity = c(metrics_knn$Value[5], metrics_decision_tree$Value[5], svm_metrics$Value[5], metrics_nn$Value[5]),
  Balanced_Accuracy = c(metrics_knn$Value[6], metrics_decision_tree$Value[6], svm_metrics$Value[6], metrics_nn$Value[6])
)

# Show metrics comparison table
kable(metrics_comparison, caption = "Comparison of Model Metrics")

## Final comparision Plot

# Get FPR and TPR values for each model
roc_knn_data <- data.frame(fpr = 1 - roc_curve$specificities, tpr = roc_curve$sensitivities)
roc_dt_data <- data.frame(fpr = 1 - roc_curve$specificities, tpr = roc_curve$sensitivities)
roc_svm_data <- data.frame(fpr = 1 - svm_roc$specificities, tpr = svm_roc$sensitivities)
roc_nn_data <- data.frame(fpr = 1 - nnet_roc$specificities, tpr = nnet_roc$sensitivities)

# Create a combined dataframe for the ROC curves
roc_data <- rbind(
  data.frame(fpr = roc_knn_data$fpr, tpr = roc_knn_data$tpr, Model = "KNN"),
  data.frame(fpr = roc_dt_data$fpr, tpr = roc_dt_data$tpr, Model = "Decision Tree"),
  data.frame(fpr = roc_svm_data$fpr, tpr = roc_svm_data$tpr, Model = "SVM"),
  data.frame(fpr = roc_nn_data$fpr, tpr = roc_nn_data$tpr, Model = "Neural Network")
)

# Final Comparision plot
ggplot(roc_data, aes(x = fpr, y = tpr, color = Model)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +  
  geom_point(size = 2, alpha = 0.8) +  
  geom_abline(linetype = "dashed", color = "grey") +  
  labs(title = "ROC Curve Comparison",
       x = "1 - Specificity",
       y = "Sensitivity") +
  scale_color_manual(values = c("#023047", "#780000", "#ff006e", "#fca311"))
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

## Bar final comparision
auc_values <- data.frame(
  Model = c("KNN", "Decision Tree", "SVM", "Neural Network"),
  AUC = c(auc_value_knn, auc_value_dt, auc_value_svm, auc_value_nn)
)

ggplot(auc_values, aes(x = reorder(Model, -AUC), y = AUC, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +  
  geom_text(aes(label = round(AUC, 3)), vjust = -0.3, size = 4) +  
  labs(title = "Comparison of Model AUCs",
       x = "Model",
       y = "AUC") +
  scale_fill_manual(values = c("#023047", "#780000", "#ff006e", "#fca311")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")  
