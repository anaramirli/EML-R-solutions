# include libs

library(MASS)
library(boot)
library(glmnet)
library(RCurl) # for read.table()
 
install.packages("leaps") # fot subset selection
library(leaps)
install.packages("pls")
library(pls)


# read data
df<-read.table("prostate.txt")


# subset data and drop the train column
train <-subset(df, train == "TRUE", select=-(train))
test <-subset(df, train == "FALSE", select=-(train))

# NORMALIZE DATA
# exclude the target from the features
feature_names <- colnames(subset(train, select=-(lpsa)))
# normalize each input feature to a mean of 0 and a variance of 1
for (i_name in feature_names){

  # get the mean and var of train features
  mean = mean(as.numeric(unlist(train[i_name])))
  sd  = sd(as.numeric(unlist(train[i_name])))

  train[i_name] = (as.numeric(unlist(train[i_name]))-mean)/sd
  test[i_name] = (as.numeric(unlist(test[i_name]))-mean)/sd
}


# get train/test matrices
x.train = model.matrix(lpsa~.,train)
y.train = train$lpsa;

x.test = model.matrix(lpsa~.,test)
y.test = test$lpsa;



# TASK 1

# regression 
reg.fit = regsubsets(lpsa~., data = train, nvmax = 8, method="exhaustive")
reg.fit.summary = summary(reg.fit)
reg.fit.summary

# Generate plots for R2, adjusted R2, Cp, and BIC  
par(mfrow = c(2,2))
plot(reg.fit.summary$rss, xlab = "Subset Size k", ylab = "RSS", type = "l")
plot(reg.fit.summary$adjr2, xlab = "Subset Size k", ylab = "Adjusted RSq", type = "l")
plot(reg.fit.summary$cp, xlab = "Subset Size k", ylab = "Cp", type = "l")
plot(reg.fit.summary$bic, xlab = "Subset Size k", ylab = "BIC", type = "l")

# Calculate training and test error measured in MSE for the model you selected.
# we select model with 4 features, those are model with following featurs 
# lcavol, lweight, lbph, svi

# extract the vector of predictors in the best fit model (model with 4 predictor- 4th)
coefi = coef(reg.fit, id = 4) 

# get the names of feautres for these coefficients.
subset.names = names(coefi)


# do the prediction on both train and test 


# do prediction by matrix multiplication of the data matirx and the coefficients vector
# then calcualte the MSE

# for train
train.pred = x.train[,subset.names]%*%coefi
mean((y.train-train.pred)^2)
# [1] 0.489776

# for test
test.pred = x.test[,subset.names]%*%coefi
mean((y.test-test.pred)^2)
# [1] 0.4563321

# TASK 2

test.MSE <- c()
train.MSE <-c()

M <- 8 
for (m in 1:M){
  
  pcr.fit = pcr(lpsa~., data=train, ncomp=m, scale=F) # we've already normalized data
  
  # train result
  pcr.pred.train = predict(pcr.fit, train, ncomp=m, scale=F)
  mse = mean((pcr.pred.train-y.train)^2)
  train.MSE <- c(train.MSE, mse)
  
  # test resut
  pcr.pred.test = predict(pcr.fit, test, ncomp=m, scale=F)
  mse = mean((pcr.pred.test-y.test)^2)
  test.MSE <- c(test.MSE, mse)
  
}

plot(train.MSE, type="b", pch=19, col="red", xlab = "principal components M", ylab = "MSE")
# Add a line
lines(test.MSE, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend(1, 95, legend=c("Train", "Test"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


# TASK 3

test.MSE <- c()
train.MSE <-c()

M <- 8 
for (m in 1:M){
  
  plsr.fit = plsr(lpsa~., data=train, ncomp=m, scale=F) # we've already normalized data
  
  # train result
  plsr.pred.train = predict(plsr.fit, train, ncomp=m, scale=F)
  mse = mean((plsr.pred.train-y.train)^2)
  train.MSE <- c(train.MSE, mse)
  
  # test resut
  plsr.pred.test = predict(plsr.fit, test, ncomp=m, scale=F)
  mse = mean((plsr.pred.test-y.test)^2)
  test.MSE <- c(test.MSE, mse)
  
}

plot(train.MSE, type="b", pch=19, col="red", xlab = "number of directions M", ylab = "MSE")
# Add a line
lines(test.MSE, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend(1, 95, legend=c("Train", "Test"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

