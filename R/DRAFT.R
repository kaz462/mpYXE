##############################Data Pre Processing##############################
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
  data[, char_col] <- sapply(mp_data[, char_col], factor)
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


##################Logistic Model & Lasso Variables Selection###################
############# data & group
X1 <- model.matrix(y ~., data = data.frame(trainData))
X <- X1[,-1]
group <- attributes(X1)$assign[-1]
X1_test <- model.matrix(y ~., data = data.frame(testData))
X_test <- X1_test[,-1]
if(group_lasso=TRUE){
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


##################################Prediction###################################
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


####################################Output#####################################
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




mp_data <- read.csv("mp_data.csv")
mp_data1 <- readxl::read_xlsx("test.xlsx")
mp_data2 <- readxl::read_xlsx("train.xlsx")
mp_data <- rbind(mp_data1, mp_data2) 
mp_data <- data.frame(mp_data)
mp_data <- mp_data[,-1] 

data=mp_data; 
y_name="missingAgain"; 
data_imputation=FALSE;
is_numeric=TRUE;
char_col=c(1,2,9);
ordinal_col=2; 
group_lasso=TRUE; 
a=0.7

