#title: "Data Science - Capstone - Project Free Breast Cancer"
#author: "Jose Maria Martin Arribas"
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
if(!require(ucimlrepo)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")

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
#and Classification train healthy status 1-healthy 2-cancer y argument
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

#calculate mean to row (glm,loess, knn,rf) to obtain result ensamble 
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "Healthy", "Cancer")

#To obtain accuracy compare mean to ensamble predictions to Classification test 1-healthy 2-cancer 
mean(ensemble_preds == breast_test_y)

######################################################### 
#4. RESULTS
#########################################################

#Create a data frame with all results (glm, loess, knn, rf, ensemble)
models <- c("Logistic regression", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(glm_preds == breast_test_y),
              mean(loess_preds == breast_test_y),
              mean(knn_preds == breast_test_y),
              mean(rf_preds == breast_test_y),
              mean(ensemble_preds == breast_test_y))
results <- data.frame(Model = models, Accuracy = accuracy)

#show the results
kable(results, caption = "Accuracy to detect breast cancer through 9 biomarkers") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Tahoma" ) %>%
  footnote(general = "Results of differents methods of machine learning use.")


