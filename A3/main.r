library(MASS)
library(boot)
library(glmnet)
library(RCurl) # for read.table()

# TASK 1

# read data
df<-read.table("prostate.txt")


# subset data and drop the train column
train <-subset(df, train == "TRUE", select=-(train))
test <-subset(df, train == "FALSE", select=-(train))

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


# TASK 2

# creates lpsa ~ lcavol  +  lweight + age + lbph + svi + lcp + gleason + pgg45 formula
f <- paste(feature_names[1], " + ", paste(feature_names[-1], collapse=" + "))
formula <- as.formula(paste("lpsa ~ ", f))
# build regression function
glm.fit = glm(formula ,data=train)

# LOOCV
loocv.err = cv.glm(train , glm.fit)
loocv.err$delta[1]
# [1] 0.5839552

# 5-fold CV
cv.5.err = cv.glm(train , glm.fit, K=5) # the default cost fun is the average squared error function
cv.5.err$delta[1]
# [1] 0.6833892


# 10-fold CV
cv.10.err = cv.glm(train , glm.fit, K=10)
cv.10.err$delta[1]
# [1] 0.602335


# predict test
mean((test$lpsa - predict(glm.fit, test))^2)
# [1] 0.521274


# TASK 3

# get train/test matrices
x.train = model.matrix(lpsa???.,train)[,-1]
y.train = train$lpsa;

x.test = model.matrix(lpsa???.,test)[,-1]
y.test = test$lpsa;


# create sequence of values for lmabda ranging from 10^-2 to 10^10 
lmabda =10^seq(-2, 10, length = 100)
# we've already standardize data, thus we set standardize=FALSE
ridge.mod=glmnet (x.train, y.train, alpha=0, lambda=lmabda, standardize=FALSE) 


plot(ridge.mod, xvar = "lambda", label = TRUE)


# TASK 4

m.ridge.cv <- cv.glmnet(x.train, y.train, alpha = 0, lambda = lmabda, nfolds=10, standardize=FALSE)
plot(m.ridge.cv)

# best lmabda value
bestlam.ridge <- m.ridge.cv$lambda.min
bestlam.ridge


# # prediction for the training/test sets
m.ridge.cv.pred.train = predict(m.ridge.cv, newx = x.train, s = bestlam.ridge)
m.ridge.cv.pred.test  = predict(m.ridge.cv, newx = x.test, s = bestlam.ridge)

# calculate rmse on training/test sets
print(paste('RMSE on training set:', mean((m.ridge.cv.pred.train - y.train)^2)))
print(paste('RMSE on test set:', mean((m.ridge.cv.pred.test - y.test)^2)))

ridge.coef <- predict(m.ridge.cv, s = bestlam.ridge, type = "coefficients")
ridge.coef
# gleason is the non-used feature, where its coefficent equals to zero.



# TASK 5

lasso.mod <- glmnet (x.train, y.train, alpha=1, lambda=lmabda, standardize=FALSE) 
plot(lasso.mod, xvar = "lambda", label = TRUE)


# TASK 6

m.lasso.cv <- cv.glmnet(x.train, y.train, alpha = 1, lambda = lmabda, nfolds=10, standardize=FALSE)
plot(m.lasso.cv)

# best lmabda value
bestlam.lasso <- m.lasso.cv$lambda.min
bestlam.lasso




# # prediction for the training/test sets
m.lasso.cv.pred.train <- predict(m.lasso.cv, newx = x.train, s = bestlam.lasso)
m.lasso.cv.pred.test  <- predict(m.lasso.cv, newx = x.test, s = bestlam.lasso)

# calculate rmse on training/test sets
print(paste('RMSE on training set:', mean((m.lasso.cv.pred.train - y.train)^2)))
print(paste('RMSE on test set:', mean((m.lasso.cv.pred.test - y.test)^2)))

feature_names

lasso.coef <- predict(m.lasso.cv, s = bestlam.lasso, type = "coefficients")
lasso.coef
# gleason is the non-used feature, where its coefficent equals to zero.

