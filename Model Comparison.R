# Our main data is in the creditcard dataframe

#Standardizing the creditcard dataframe using scale()
cc_new <- scale(creditcard[1:30])




#Appending "Class" column from creditcard df to cc_new df
cc_new <- cbind(cc_new, creditcard["Class"])




#Converting "Class" column to factor
cc_new$Class <- as.factor(cc_new$Class)




#Splitting the cc_new df into training & test data
library(caTools)
set.seed(12345)
cc_bal_split <- sample.split(cc_new$Class, SplitRatio = 0.75)
cc_bal_train <- subset(cc_new, cc_bal_split == TRUE)
cc_bal_test <- subset(cc_new, cc_bal_split == FALSE)




#Oversampling underrepresented class using SMOTE for the training data i.e. cc_bal_train df
library(ROSE)
cc_bal <- ovun.sample(Class ~ ., data = cc_bal_train, method = "both", p=0.40, seed = 12345)$data
#now cc_bal is our balanced training data





#To check number of observation per category in Class column of cc_bal
library(plyr)
count(cc_bal, c("Class")) 




#MODEL - LOGISTIC REGRESSION




#Running Logistic Regression on training data
cc_log <- glm(formula = Class ~ .,
              family = binomial,
              data = cc_bal)




#Printing Logistic Regression results
summary(cc_log)




# Predicting probabilities
cc_pred1 <- predict(cc_log, type = 'response', newdata = cc_bal_test[-31])




#Classifying the results based on predictions, keeping threshold as x for a 1
cc_pred_def1 <- ifelse(cc_pred1 > 0.1, 1, 0) #Predicted classification




#Printing confusion matrix
cc_conf1 <- table(cc_pred_def1,cc_bal_test[ ,31])
FNR <- cc_conf1[3]/(cc_conf1[3] + cc_conf1[4]) #Calculating False -ve rate
FPR <- cc_conf1[2]/(cc_conf1[2] + cc_conf1[1]) #Calculating False +ve rate
SensitivityRate <- cc_conf1[4]/(cc_conf1[4] + cc_conf1[3])
FNR
FPR
SensitivityRate





#Calculating auc for predicted classification
#install.packages("pROC")
library(pROC)
cc_roc1 <- roc(cc_bal_test$Class,cc_pred1)
plot(cc_roc1, col = "green", main = "ROC Curve - Logistic Regression")
auc(cc_roc1)




# #SECTION - SVM
#Running SVM on training data
library(e1071)
set.seed(12345)
cc_svm = svm(formula = Class ~ .,
             data = cc_bal,
             type = 'C-classification',
             kernel = 'radial',probability=TRUE)

#Predicting the Test set results
cc_pred2 = predict(cc_svm, newdata = cc_bal_test[-31], probability=TRUE)
#
count(cc_pred2)
#Classifying the results based on predictions, keeping threshold as x for a 1
#cc_pred_def2 <- ifelse(cc_pred2 > 0.2, 1, 0) #Predicted classification
#
# #Printing confusion matrix
cc_conf2 <- table(cc_pred2,cc_bal_test[ ,31])
FNR <- cc_conf2[3]/(cc_conf2[3] + cc_conf2[4]) #Calculating False -ve rate
FPR <- cc_conf2[2]/(cc_conf1[2] + cc_conf2[1]) #Calculating False +ve rate
SensitivityRate <- cc_conf2[4]/(cc_conf2[4] + cc_conf2[3])
FNR
FPR
SensitivityRate





# 
# 
# #Calculating auc for predicted classification
# cc_roc2 <- roc(cc_pred2,cc_bal_test$Class)
# auc(cc_roc2)




#SECTION - RANDOM FOREST
library(randomForest)
set.seed(12345)
cc_rf = randomForest(Class ~ ., data = cc_bal,
                     ntree = 100)


cc_pred3 = predict(cc_rf, newdata = cc_bal_test[-31], type = "prob")
cc_pred3prob  = cc_pred3[,2]
#cc_pred3 = as.numeric(cc_pred3)


count(cc_pred3prob)


#Classifying the results based on predictions, keeping threshold as x for a 1
cc_pred_def3 <- ifelse(cc_pred3prob > 0.1, 1, 0) #Predicted classification




cc_roc3 <- roc(cc_bal_test$Class,cc_pred3[,1])
plot(cc_roc3, col = "blue", main = "ROC Curve - Random Forest")
auc(cc_roc3)




#Printing confusion matrix
cc_conf3 <- table(cc_pred_def3,cc_bal_test[ ,31])
FNR <- cc_conf3[3]/(cc_conf3[3] + cc_conf3[4]) #Calculating False -ve rate
FPR <- cc_conf3[2]/(cc_conf3[2] + cc_conf3[1]) #Calculating False +ve rate
SensitivityRate <- cc_conf3[4]/(cc_conf3[4] + cc_conf3[3])
FNR
FPR
SensitivityRate
































