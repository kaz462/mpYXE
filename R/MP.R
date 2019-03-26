library(robustbase)
library(fpc)

library(fpc)
# NOT RUN {
set.seed(776655)
v1 <- rnorm(20)
v2 <- rnorm(20)
d1 <- sample(c(2,4,6,8),20,replace=TRUE)
d2 <- sample(1:4,20,replace=TRUE)
ldata <- cbind(v1,v1,d1,d2)
lc <-
  discrete.recode(ldata,xvarsorted=TRUE,continuous=2,discrete=2)
library(MASS)
data(Cars93)
Cars934 <- Cars93[,c(3,5,8,10)]
cc <- discrete.recode(Cars934,xvarsorted=FALSE,continuous=c(2,3),discrete=c(1,4))
# }
factor(ldata$v1,ldata$d1)

# To do it for all names
df[] <- lapply( df, factor) # the "[]" keeps the dataframe structure
col_names <- names(df)
# do do it for some names in a vector named 'col_names'
df[col_names] <- lapply(df[col_names] , factor)

for (i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i]=factor(data[,i])
  }
}








