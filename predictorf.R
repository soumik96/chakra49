predictorf <- function(usertest){
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
  
  # Predicting probabilities
  cc_pred1 <- predict(cc_log, type = 'response', newdata = usertest)
  
  #Classifying the results based on predictions, keeping threshold as x for a 1
  cc_pred_def1 <- ifelse(cc_pred1 > 0.1, 1, 0) #Predicted classification
  
  return(cc_pred1)
  
}






























































