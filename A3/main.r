library(MASS)
library(boot)
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
# 0.6833892


# 10-fold CV
cv.10.err = cv.glm(train , glm.fit, K=10)
cv.10.err$delta[1]
# 0.602335


# predict test
mean((test$lpsa - predict(glm.fit, test))^2)
# [1] 0.521274