#' @param data A data set with n rows (observations) and (p+1) columns 
#' (p variables and 1 binary response variable). An ordinal variable with 
#' \geq 10 levels will be treated as continues one. In this case, please 
#' make sure the levels are created in increasing alphabetical order.   
#' @param y_name The name of response variabel, i.e."missingAgain". 
#' Response variable must be binary. 
#' @param data_imputation If TRUE, data imputation will be performed. 
#' Otherwise the observations with missing values will be ignored.
#' @param char_col column index(es) for categorical variables for the given data. 
#' If is_numeric=FALSE, set char_col=0, the function will detect categorical variables 
#' automatically. If is_numeric=FALSE, select all categorical variables' indexes. 
#' ex.1 if column 1 is the only categorical variable, char_col=1; 
#' ex.2 if column 2, 3, 4 are categorical variables, char_col=c(2, 3, 4)    
#' @param ordinal_col column indexes for ordinal variables for the given data. 
#' ex.1 if column 1 is the only categorical variable, ordinal_col=1; 
#' ex.2 if column 2, 3, 4 are ordinal variables, char_col=c(2, 3, 4) 
#' @param group_lasso group lasso is used for variable selection or not. 
#' group_lasso=TRUE if group lasso is used for the model, otherwise group_lasso=FALSE. 
#' If the given data have at least one categorical variable with 3 or more levels, 
#' group lasso is required. Otherwise, lasso will be used in our model.
#' @param a Percentage of data for training, the rest would be used for testing.
#' 
#' @return List of:
#' \itemize{
#'   \item ROC - ROC curve is a graphical plot that illustrates the diagnostic ability of 
#'   a binary classifier system as its discrimination threshold is varied. 
#'   It is created by plotting the true positive rate (sensitivity) against 
#'   the false positive rate (1 − specificity) at various threshold settings. 
#'   We can get the "best" threshold which has the highest accuracy among all threshold settings, 
#'   in the meanwhile, the specificity and sensitivity will be returned at this threshold.
#'   \item Prediction of test data - Plot each person's probabilities (risks) of missing again &
#'    classification with the "best" threshold
#'   \item Selected variables - All selected variables and their coefficients will be returned
#'   \item emotion_type - Type designation from the \code{emotion} column of the \code{emotion_dt} table
#'   \item emotion_count - Count of the number of emotion words of that \code{emotion_type}
#'   \item emotion - A score of the percentage of emotion words of that \code{emotion_type}
#' }
#' 
#' @keywords prediction missing-person
#' 
#' @export
#' 
reportStats <- function(data, y_name, data_imputation=FALSE, is_numeric, 
                        char_col, ordinal_col, group_lasso, a) {
  ###################################Data Pre Processing###################################
  ############# imputation
  data[data=="NULL"] <- NA 
  if(data_imputation==TRUE){
    # DATA IMPUTATION
    data <- data
  } else{
    # delete obs including missing values
    data <- data[complete.cases(data), ]
  }
  ############# factor categorical variables
  if(is_numeric==TRUE){
    # if categorical data are recoreded as numeric
    # input: char_col=categorical variables col index
    data[, as.vector(char_col)] <- data.frame(lapply(mp_data[, as.vector(char_col)], factor))
  }else{
    # if categorical data are recoreded as characters 
    # input: char_col=0
    for (i in 1:ncol(data)){
      if(is.character(data[,i])){
        data[,i]=factor(data[,i])
      }
    }
  }
  ############# ordinal>10 levels, transfer to continuous
  for(i in ordinal_col){
    if(length(levels(data[,i]))>=10){
      data[, ordinal_col] <- sapply(mp_data[, ordinal_col], as.numeric)
    }
  }
  ############# recode y 
  names(data)[names(data)==y_name] <- "y"
  data$y <- as.factor(data$y)
  data$y <- ifelse(data$y != levels(data$y)[1], 1, -1)
  ############# training & test data 
  trainDataIndex <- sample(1:nrow(data), a*nrow(data))  
  trainData <- data[trainDataIndex, ]
  testData <- data[-trainDataIndex, ]
  Y_train <- data$y[trainDataIndex]
  Y_test <- data$y[-trainDataIndex]
  #########################################################################################
  
  
  
  #######################Logistic Model & Lasso Variables Selection########################
  ############# data & group
  X1 <- model.matrix(y ~., data = data.frame(trainData))
  X <- X1[,-1]
  group <- attributes(X1)$assign[-1]
  X1_test <- model.matrix(y ~., data = data.frame(testData))
  X_test <- X1_test[,-1]
  if(group_lasso==TRUE){
    #################################### group lasso
    ############# fit group lasso penalized logistic regression
    # finds the best lambda parameter by cross validation
    # and returns the corresponding model
    cv <- cv.gglasso(x=X ,y=Y_train,group=group,loss="logit",pred.loss = "loss")
    model <- gglasso(x=X ,y=Y_train,group=group,loss="logit", lambda = cv$lambda.min)
    ############# test 
    l <- predict(model, newx=X_test, type="link")
    prob <- exp(l) / (1 + exp(l))
  } else{
    #################################### lasso
    # finds the best lambda parameter by cross validation
    # and returns the corresponding model
    cv <- cv.glmnet(X, y=Y_train, alpha=1, family="binomial",type.measure = "mse")
    model <- glmnet(X, y=Y_train, alpha=1, family="binomial", lambda=cv$lambda.min)
    # test data
    prob <- predict(model,newx = X_test, s=cv$lambda.1se, type="response")
  } 
  #########################################################################################
  
  
  
  #######################################Prediction########################################
  # selected variables 
  coefs <- as.matrix(coef(cv)) 
  ix <- which(abs(coefs[,1]) > 0)
  selected_coefs_n <- length(ix)
  selected_coefs <- coefs[ix,1, drop=FALSE]   
  # choose the best cut off
  modelroc1 <- roc(Y_test, as.vector(prob))
  threshold <- coords(modelroc1, "best", "threshold")
  # probabilities(risks) to predictions
  predict1 <- ifelse(as.vector(prob)>threshold[1],1,-1)
  # accuracy, confusion matrix
  accuracy <- mean(predict1==Y_test)  
  confusion_matrix <- table(pred=predict1,true=Y_test)
  #########################################################################################
  
  
  
  #########################################Output##########################################
  print("******************************************************************************")
  print("****************************Statistical Report********************************")
  # ROC curve    
  print("ROC curve")
  plot(modelroc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1,0.2), 
       grid.col=c("green","red"), max.auc.polygon=TRUE, 
       auc.polygon.col="skyblue",print.thres=TRUE)
  cat("Prediction of test data", "\n")
  cat("Plot each person's probabilities of missing again", "\n")
  # predict test data 
  plot(prob,pch = 16,col="grey")
  points(which(predict1!=Y_test),
         as.vector(prob)[which(predict1!=Y_test)],
         pch = 13,col="red", cex = 1.2)
  abline(h=threshold[1], col="purple")  
  print(threshold)
  print(paste('The number of selected variables:', selected_coefs_n))
  cat("Selected variables")
  print(selected_coefs)
  cat("Confusion Matrix", "\n")
  print(confusion_matrix)
  print(paste('Accuracy:', format(accuracy,digits=2)))
  print("******************************************************************************")
  #########################################################################################
}