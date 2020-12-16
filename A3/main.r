library(MASS)
library(RCurl) # for read.table()

# TASK 1

# read data
df<-read.table("prostate.txt")


# subset data and drop the train column
train <-subset(df, train == "TRUE", select=-(train))
test <-subset(df, train == "FALSE", select=-(train))

# normalize each input feature to a mean of 0 and a variance of 1
# exclude the target from features
for (col_name in colnames(subset(train, select=-(lpsa)))){
  
  # get the mean and var of train features
  mean = mean(as.numeric(unlist(train[col_name])))
  sd  = sd(as.numeric(unlist(train[col_name])))
  
  train[col_name] = (as.numeric(unlist(train[col_name]))-mean)/sd
  test[col_name] = (as.numeric(unlist(test[col_name]))-mean)/sd
}


# TASK 2


> cv.error=rep(0,5)
> for (i in 1:5){
  + glm.fit=glm(mpg∼poly(horsepower ,i),data=Auto)
  + cv.error[i]=cv.glm(Auto ,glm.fit)$delta [1]
  + }
  



