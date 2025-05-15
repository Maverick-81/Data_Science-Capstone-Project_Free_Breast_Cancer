#title: "Data Science - Capstone - Project Free Breast Cancer"
#author: "JMMA"
#date: "Febrary 2025"

#########################################################
# 1.Create breast_train and breast_test sets 
#########################################################

# Note: this process could take a minutes. 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(vtable)) install.packages("vtable", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(ucimlrepo)) install.packages("ucimlrepo", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
# if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
# if(!require(ada)) install.packages("ada", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
# if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(NeuralNetTools)) install.packages("NeuralNetTools", repos = "http://cran.us.r-project.org")

# Libraries required to run the proyect
library(tidyverse)
library(caret)
library(kableExtra)
library(data.table)
library(vtable)
library(ggthemes)
library(corrr)
library(ggcorrplot)
library(factoextra)
library(ucimlrepo)
library(gam)
library(splines)
library(foreach)
library(randomForest)
library(pROC)
library(rpart)
# library(ada)
# library(rpart.plot)
# library(e1071)
library(nnet)
library(NeuralNetTools)

options(digits = 5)
options(timeout = 120)

# Check which datasets can be imported and choose breast cancer coimbra
# Create a complex HTML table using kableExtra for obtain list of data set of uciml
kable(list_available_datasets(search = "cancer"), caption = "Structure UC Irvine repository") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Palatino") %>%
  column_spec(1:3, bold = TRUE, color = "#27408B") %>%
  footnote(general = "This table lists dataset of cancer (UC Irvine machine learning repository)")

#Save dataset with data and metadata
data_set_uciml = fetch_ucirepo(id=451)

#Save data to study in a data frame breast_file
breast_data <- data.frame(data_set_uciml$data$original)

#Do a backup data frame breast cancer in a csv
write.csv2(breast_data,"breast_cancer.csv")

set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = breast_data$Classification, times = 1, p = 0.2, list = FALSE)
#Breast test set will be 20% of breast cancer data
breast_train <- breast_data[-test_index,]
#Breast data will be 80 % of breast cancer data
breast_test <- breast_data[test_index,]

# # Check for Missing or Null Values
# missing_values_train <- sum(is.na(breast_train))
# missing_values_test <- sum(is.na(breast_test))
# 
# cat("Number of missing values in train_data:", missing_values_train, "\n")
# cat("Number of missing values in test_data:", missing_values_test, "\n")

#########################################################
# 2. ANALYSIS
#########################################################

#########################################################
# I. Data structure
#########################################################

# Check the dimensions of the breast train data
dim(breast_train)

# Create a complex HTML table using kableExtra for obtain 
# five first registers of data structure breast train 
kable(head(breast_train, 5), caption = "Breast cancer") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Tahoma" ) %>%
  column_spec(9:10, bold = TRUE, color = "#27408B") %>%
  footnote(general = "This table lists head of dataset cancer.")

#Check the structure of the data
str(breast_train)


#########################################################
# II. Summaries data
#########################################################

#Calculates the following summary statistics for the data frame
summary_breast_train <- summary(breast_train)

#The important statics is rating min, 1st Quantile, median, mean, 3rd Quantil, max
kable(summary_breast_train, caption = "Statics breast cancer") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Tahoma") %>%
  row_spec(3:4, bold = TRUE, color = "#B22222") %>%
  footnote(general = "This table lists(Age, BMI, Glucose, Insulin, HOMA, Leptin,
           Adiponectin, Resistin, MCP.1, Classification, min, 1st Quantile, median, mean, 3rd Quantiles, max.")

# Correlations
#cor(breast_train[, sapply(breast_train, is.numeric)])

#########################################################
# III. Study graphics statistics
#########################################################

#########################################################
# a. Patients by amount of glucose by health status
#########################################################

#Convert to breast train and breast test field classification in factor value to distinct 1- Healthy 2- Cancer
breast_train <- breast_train %>% mutate(Classification = as.factor(Classification))
breast_test <- breast_test %>% mutate(Classification = as.factor(Classification)) 
levels(breast_train$Classification) <- c("Healthy","Cancer")
levels(breast_test$Classification) <- c("Healthy","Cancer")

ggplot(breast_train, aes(x=Classification, y=Glucose, fill=Classification)) +
  geom_boxplot(outlier.colour = "#FF3030") +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  scale_fill_manual(breaks = waiver(),
                    values = c("#27408B", "#B22222")) +
  labs(title="Patients by amount of glucose by health status",
       caption="Source data: https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra",
       y="Glucose (mg 10^-3/dL 10^-1)",
       x="Status of healthy") +
  scale_y_continuous(breaks = seq(50, 250, by = 10)) +
  theme_gdocs()

#########################################################
# b. Patients by amount of insulin by health status
#########################################################

ggplot(breast_train, aes(x=Classification, y=Insulin, fill=Classification)) +
  geom_boxplot(outlier.colour = "#FF3030") +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  scale_fill_manual(breaks = waiver(),
                    values = c("#27408B", "#B22222")) +
  labs(title="Patients by amount of insulin by health status",
       caption="Source data: https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra",
       y="Insulin (µU 10^-6/mL 10^-3)",
       x="Status of healthy") +
  scale_y_continuous(breaks = seq(0, 60, by = 2)) +
  theme_gdocs()

#########################################################
# c. Patients by amount of HOMA by health status
#########################################################

ggplot(breast_train, aes(x=Classification, y=HOMA, fill=Classification)) +
  geom_boxplot(outlier.colour = "#FF3030") +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  scale_fill_manual(breaks = waiver(),
                    values = c("#27408B", "#B22222")) +
  labs(title="Patients by amount of HOMA by health status",
       caption="Source data: https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra",
       y="HOMA",
       x="Status of healthy") +
  scale_y_continuous(breaks = seq(0, 30, by = 1)) +
  theme_gdocs()

#########################################################
# d. Patients by amount of resistin by health status
#########################################################

ggplot(breast_train, aes(x=Classification, y=Resistin, fill=Classification)) +
  geom_boxplot(outlier.colour = "#FF3030") +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  scale_fill_manual(breaks = waiver(),
                    values = c("#27408B", "#B22222")) +
  labs(title="Patients by amount of resistin by health status",
       caption="Source data: https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra",
       y="Resistin (ng 10^-9/mL 10^-3)",
       x="Status of healthy") +
  scale_y_continuous(breaks = seq(0, 90, by = 2)) +
  theme_gdocs()

#########################################################
# e. Patients by amount of MCP.1 by health status
#########################################################

ggplot(breast_train, aes(x=Classification, y=MCP.1, fill=Classification)) +
  geom_boxplot(outlier.colour = "#FF3030") +
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  scale_fill_manual(breaks = waiver(),
                    values = c("#27408B", "#B22222")) +
  labs(title="Patients by amount of MCP.1 by health status",
       caption="Source data: https://archive.ics.uci.edu/dataset/451/breast+cancer+coimbra",
       y="MCP.1 (pg 10^-12/dL 10^-1)",
       x="Status of healthy") +
  scale_y_continuous(breaks = seq(0, 1800, by = 75)) +
  theme_gdocs()

#########################################################
# 3. MACHINE LEARNING
#########################################################

#########################################################
# I. PCA (principal component analysis)
#########################################################

#Convert to breast train and breast test field age and glucose in numeric value to scale
breast_train <- breast_train %>% mutate(Age = as.numeric(Age))
breast_train <- breast_train %>% mutate(Glucose = as.numeric(Glucose))

breast_test <- breast_test %>% mutate(Age = as.numeric(Age))
breast_test <- breast_test %>% mutate(Glucose = as.numeric(Glucose)) 


PCA <- breast_train %>% select(-Classification)

#check is not null values in PCA data   
colSums(is.na(PCA))

#normalization using function scale 
data_normalized <- scale(PCA)

#calculate correlation of matrix
corr_matrix <- cor(data_normalized)

#Correlation plot
ggcorrplot(corr_matrix,
           method = "square",
           hc.order = TRUE, 
           type = "full",
           lab = TRUE,
           title = "Matrix correlation of predictors",
           ggtheme = ggplot2::theme_light(),
           colors = c("#00688B", "white", "#8B1A1A"))

#summary importance of components
data.pca <- princomp(corr_matrix)
summary(data.pca)

#Scree plot
fviz_eig(data.pca, 
         addlabels = TRUE,
         barfill = "#00688B",
         barcolor = "#00688B", 
         linecolor = "black") +
      theme_gdocs()

#Graphic pca var
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "#00BFFF", "#8B1A1A"),
             repel = TRUE) +
    theme_gdocs()

#########################################################
# II. Glm (logistic regression)
#########################################################

#Create train component x with all predictors except Classification that is a factor use en component y to function train 
breast_train_x <- breast_train %>% select(-Classification)

#Normalization train predictors
breast_train_x <- scale(breast_train_x)

#Create train component y to function train to healthy status 1-healthy 2-cancer
breast_train_y <- breast_train$Classification

#Create test component x with all predictors except Classification that is a factor use en component y to function train 
breast_test_x <- breast_test %>% select(-Classification)

#Normalization test predictors
breast_test_x <- scale(breast_test_x)

#Create test component y to function train to healthy status 1-healthy 2-cancer
breast_test_y <- breast_test$Classification

#Calculate accuracy to detect cancer with 9 predictors to use glm
set.seed(1, sample.kind = "Rounding") 

#Use train function to method glm, pass 9 predictors x argument
#and Classification train healthy status 1-healthy 2-cancer y argument Lda svmLinear gamboost kknn ranger wsfr avNNet mlp monmlp
train_glm <- train(breast_train_x, breast_train_y, method = "glm")

#Use predict function to pass train_glm and 9 predictors to test data
glm_preds <- predict(train_glm, breast_test_x)

#To obtain accuracy compare mean to glm predictions results to Classification test 1-healthy 2-cancer
mean(glm_preds == breast_test_y)


#########################################################
# III. Loess (local polynomial regression fitting)
#########################################################

#Calculate accuracy to detect cancer with 9 predictors to use loess
set.seed(2, sample.kind = "Rounding")

#Use train function to method gamLoess, pass 9 predictors x argument
#and Classification train  healthy status 1-healthy 2-cancer y argument
train_loess <- train(breast_train_x, breast_train_y, method = "gamLoess")

#Use predict function to pass train_loess and 9 predictors to test data
loess_preds <- predict(train_loess, breast_test_x)

#To obtain accuracy compare mean to loess predictions results to Classification test 1-healthy 2-cancer
mean(loess_preds == breast_test_y)

#########################################################
# IV. K nearest neihbours 
#########################################################

#Calculate accuracy to detect cancer with 9 predictors to use knn
set.seed(3, sample.kind = "Rounding")

#Use train function to method knn, pass 9 predictors x argument
#and Classification train healthy status 1-healthy 2-cancer y argument
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(breast_train_x, breast_train_y,
                   method = "knn",
                   tuneGrid = tuning)

#obtain the best parameter of sequence of tuning in this case 3
#train_knn$bestTune

#Use predict function to pass train_knn and 9 predictors to test data
knn_preds <- predict(train_knn, breast_test_x)

#To obtain accuracy compare mean to knn predictions results to Classification test 1-healthy 2-cancer
mean(knn_preds == breast_test_y)


#########################################################
# . K nearest neihbours 
#########################################################

# #Define the number of neighbors
# k <- 9
# 
# #Train the KNN model with probabilities using knn3
# knn_model <- knn3(Classification ~ ., breast_train, k = k)
# 
# # Predict probabilities for the positive class in the test set
# 
# probabilities_knn <- predict(knn_model,breast_test, type = "prob")[, 2]
# 
# #Convert predicted_labels in factor value 1- Healthy 2- Cancer
# probabilities_knn1 <- as.factor(ifelse(probabilities_knn > 0.5, 0, 1))
# levels(probabilities_knn1) <- c("Healthy","Cancer")
# levels(probabilities_knn1) <- c("Healthy","Cancer")
# 
# 
# # Create the confusion matrix
# confusionMatrix(probabilities_knn1, breast_test$Classification)
# confusionMatrix(probabilities_knn1, breast_test$Classification)$byClass["F1"]
# 
# breast_test$Classification_Predicted <- probabilities_knn1
# 
# ggplot(breast_test, aes(x = Age, y = Glucose, color = Classification_Predicted)) +
#   geom_point(size = 3) +
#   scale_color_manual(values = c("#27408B", "#B22222")) +
#   labs(title = paste("Cancer breast Classification with KNN (k =", k, ")"),
#        x = "Age", 
#        y = "Glucose") +
#   theme_gdocs()
# 
# # ROC Curve and AUC
# roc_curve <- roc(breast_test$Classification, probabilities_knn)  # Compute the ROC curve for the KNN model
# 
# # Data for the ROC curve
# roc_data <- data.frame(
#   tpr = roc_curve$sensitivities,  # True Positive Rate (Sensitivity)
#   fpr = 1 - roc_curve$specificities  # False Positive Rate (1 - Specificity)
# )
# 
# # Plot the ROC curve
# roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
#   geom_line(color = "#00688B", size = 1) +  # Draw the ROC curve line
#   geom_abline(linetype = "dashed", color = "grey") +  # Add a dashed diagonal line (no discrimination line)
#   labs(title = "ROC Curve - KNN Model",
#        x = "1 - Specificity",  # Label for the x-axis
#        y = "Sensitivity") +  # Label for the y-axis
#   theme_gdocs()
# 
# print(roc_plot)
# 
# # AUC
# auc_value_knn <- auc(roc_curve)
# cat("AUC:", auc_value_knn, "\n")

#########################################################
# . Tree Model
#########################################################

# #Train decision tree model
# decision_tree_model <- rpart(Classification ~ ., data = breast_train, method = "class")
# 
# # Predict probabilities for the positive class in the test set
# probabilities_tree <- predict(decision_tree_model,breast_test, type = "prob")[, 2]
# 
# #Convert predicted_labels in factor value 1- Healthy 2- Cancer
# probabilities_tree1 <- as.factor(ifelse(probabilities_tree > 0.5, 0, 1))
# levels(probabilities_tree1) <- c("Healthy","Cancer")
# levels(probabilities_tree1) <- c("Healthy","Cancer")
# 
# 
# # Create the confusion matrix
# confusionMatrix(probabilities_tree1, breast_test$Classification)
# confusionMatrix(probabilities_tree1, breast_test$Classification)$byClass["F1"]
# 
# breast_test$Classification_Predicted <- probabilities_tree
# 
# # ROC Curve and AUC
# roc_curve <- roc(breast_test$Classification, probabilities_tree)  # Compute the ROC curve for the tree model
# 
# # Data for the ROC curve
# roc_data <- data.frame(
#   tpr = roc_curve$sensitivities,  # True Positive Rate (Sensitivity)
#   fpr = 1 - roc_curve$specificities,  # False Positive Rate (1 - Specificity)
#   thresholds = roc_curve$thresholds
# )
# 
# # Plot the ROC curve
# roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
#   geom_line(color = "#00688B", size = 1) +  # Draw the ROC curve line
#   geom_abline(linetype = "dashed", color = "grey") +  # Add a dashed diagonal line (no discrimination line)
#   labs(title = "ROC Curve - Tree Model",
#        x = "1 - Specificity",  # Label for the x-axis
#        y = "Sensitivity") +  # Label for the y-axis
#   theme_gdocs()
# 
# print(roc_plot)
# 
# # AUC
# auc_value_tree_model <- auc(roc_curve)
# cat("AUC:", auc_value_tree_model, "\n")
# 
# 
# # Plot decision tree
# rpart.plot(decision_tree_model, main = "Decision Tree",
#             type = 5, extra = 101, fallen.leaves = TRUE,
#             box.palette = "RdBu", shadow.col = "gray", nn = TRUE)


#########################################################
# . SVM Model
#########################################################

# # Train SVM model
# svm_model <- svm(Classification ~ Glucose + Insulin, data = breast_train, kernel = "polynomial", probability = TRUE)
# 
# # Predict probabilities for the positive class in the test set
# probabilities_svm <- predict(svm_model,breast_test)
# 
# # Create the confusion matrix
# confusionMatrix(probabilities_svm, breast_test$Classification)
# confusionMatrix(probabilities_svm, breast_test$Classification)$byClass["F1"]
# 
# # ROC Curve
# svm_probs <- attr(predict(svm_model, breast_test, probability = TRUE), "probabilities")[, 2]
# svm_roc <- roc(breast_test$Classification, svm_probs)
# plot(svm_roc, main="SVM ROC Curve", col="#4169E1")
# 
# auc_value_svm <- auc(svm_roc)
# cat("AUC:", auc_value_svm, "\n")

# # Create grid of values for Glucose and BMI
# svm_grid <- expand.grid(
#   Glucose = seq(min(breast_train$Glucose), max(breast_train$Glucose), length.out = 10),
#   Insulin = seq(min(breast_train$Insulin), max(breast_train$Insulin), length.out = 10)
# )
# 
# # Predict outcomes for the grid
# svm_grid$Classification <- predict(svm_model, svm_grid)
# 
# # Plot decision boundary
# ggplot(svm_grid, aes(x = Glucose, y = Insulin, color = Classification)) +
#   geom_tile(aes(fill = Classification), alpha = 0.5) +
#   geom_point(data = breast_train, aes(x = Glucose, y = Insulin, color = Classification), size = 3) +
#   labs(title = "SVM Decision Boundary", x = "Glucose", y = "Insulin") +
#   scale_color_manual(values = c("#B22222", "#00688B"))


#########################################################
# . Neuronal Network
#########################################################

# Preprocess the data by handling missing values and scaling the features
breast_train[is.na(breast_train)] <- lapply(breast_train, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))
breast_test[is.na(breast_test)] <- lapply(breast_test, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))

# Scale features to ensure that the neural network performs well
train_data_scaled <- scale(breast_train[, c("Glucose", "Insulin", "HOMA", "Resistin")])
test_data_scaled <- scale(breast_test[, c("Glucose", "Insulin", "HOMA", "Resistin")])


# Convert scaled data back to data frame format
breast_train_nn <- data.frame(breast_train[, "Classification"], train_data_scaled)
colnames(breast_train_nn) <- c("Classification", "Glucose", "Insulin", "HOMA", "Resistin")
breast_test_nn <- data.frame(breast_test[, "Classification"], test_data_scaled)
colnames(breast_test_nn) <- c("Classification", "Glucose", "Insulin", "HOMA", "Resistin")

# Train the neural network model with adjusted parameters
nnet_model <- nnet(Classification ~ Glucose + Insulin + HOMA + Resistin,
                   data = breast_train_nn,
                   size = 15,
                   maxit = 1500,
                   linout = FALSE,
                   trace = FALSE,
                   decay = 1)

# Make predictions on the test set (probabilities)
predicted_probabilities <- predict(nnet_model, breast_test, type ="raw")

# Convert probabilities to binary labels (1 or 0) based on a threshold of 0.5
probabilities_nne <- as.factor(ifelse(predicted_probabilities > 0.5, 0, 1))

levels(probabilities_nne) <- c("Healthy","Cancer")
levels(probabilities_nne) <- c("Healthy","Cancer")

# Create the confusion matrix
confusionMatrix(probabilities_nne, breast_test$Classification)
confusionMatrix(probabilities_nne, breast_test$Classification)$byClass["F1"]

# Generate the ROC curve for the neural network
nnet_roc <- roc(as.numeric(breast_test$Classification), predicted_probabilities)

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

# Plot Neural Network Model
plotnet(nnet_model)

#########################################################
# V. Random forest
#########################################################

#Calculate accuracy to detect cancer with 9 predictors to use random forest

set.seed(4, sample.kind = "Rounding")

#Use train function to method rf, pass 9 predictors x argument
#and Classification train healthy status 1-healthy 2-cancer y argument
tuning <- data.frame(mtry = c(3, 5, 7, 9))    
train_rf <- train(breast_train_x, breast_train_y,
                  method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE)

#obtain the best parameter of sequence of tuning in this case 9
#train_rf$bestTune

#Use predict function to pass rf and 9 predictors to test data
rf_preds <- predict(train_rf, breast_test_x)
#To obtain accuracy compare mean to rf predictions results to Classification test 1-healthy 2-cancer
mean(rf_preds == breast_test_y) 

#the most important variable in the random forest model is glucose 100
varImp(train_rf)

#########################################################
# VI. Ensamble
#########################################################

#Calculate accuracy to detect cancer with 9 predictors to use ensamble

#Create a logic matrix to results glm, loess, knn and rf with 24 patients in test data 
#results FALSE = healthy and TRUE = cancer
ensemble <- cbind(glm = glm_preds == "Healthy", loess = loess_preds == "Healthy",
                  knn = knn_preds == "Healthy", rf = rf_preds == "Healthy")
                  #knn1 = probabilities_knn1 == "Healthy", tree = probabilities_tree1 == "Healthy",
                  #svm = probabilities_svm == "Healthy", nne = probabilities_nne == "healthy")

#calculate mean to row (glm,loess, knn,rf) to obtain result ensamble 
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "Healthy", "Cancer")

#To obtain accuracy compare mean to ensamble predictions to Classification test 1-healthy 2-cancer 
mean(ensemble_preds == breast_test_y)

######################################################### 
#4. RESULTS
#########################################################

#Create a data frame with all results (glm, loess, knn, rf, ensemble)
#models <- c("Logistic regression", "Loess", "K nearest neighbors", "Random forest", "knn1", "Tree", "SVM", "Neural Network", "Ensemble")
models <- c("Logistic regression", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(glm_preds == breast_test_y),
              mean(loess_preds == breast_test_y),
              mean(knn_preds == breast_test_y),
              mean(rf_preds == breast_test_y),
              # mean(probabilities_knn1 == breast_test_y),
              # mean(probabilities_tree1 == breast_test_y),
              # mean(probabilities_svm == breast_test_y),
              # mean(probabilities_nne == breast_test_y),
              mean(ensemble_preds == breast_test_y))
results <- data.frame(Model = models, Accuracy = accuracy)

#show the results
kable(results, caption = "Accuracy to detect breast cancer through 9 biomarkers") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Tahoma" ) %>%
  footnote(general = "Results of differents methods of machine learning use.")


