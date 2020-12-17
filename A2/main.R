
library(dplyr)


library(MASS)

# PART 1)

# load data
data <- read.csv("/Users/joshgunguliyev/Desktop/phoneme.csv")


# SPLIT DATA

train_idx = list()
test_idx = list()

for (i in 1:length(data$speaker)){ 
  
  # split string and get the label
  label <- strsplit(as.character(data$speaker[[i]]), "\\.")[[1]][1]
  
  if (label=="train") {
    train_idx <-c (train_idx, i)
  } else if (label=="test") {
    test_idx <-c (test_idx, i)
  }
}

# create subgroups based on train/test inices
data.train = data[unlist(train_idx),]
data.test = data[unlist(test_idx),]

# remove row number, speaker, response columns from the features
train_features = subset(data.train, select = -c(row.names, speaker, g))
test_features = subset(data.test, select = -c(row.names, speaker, g))

# PART 2)

# create formula
f <- paste(names(train_features)[1], " + ", paste(names(train_features)[-1], collapse=" + "))

# fit
lda.fit <- lda(as.formula(paste("g ~ ", f)), data = data.train)

lda.fit

# TRAIN ERROR

# predict train
lda.train.pred <- predict(lda.fit, newdata = data.train)
# get prediction labels
lda.train.class=lda.train.pred$class
# error and accuracy
mean(lda.train.class!=data.train$g)
mean(lda.train.class==data.train$g)

# TEST ERROR

# predict test
lda.test.pred <- predict(lda.fit, newdata = data.test)
# get prediction labels
lda.test.class=lda.test.pred$class
# error and accuracy
mean(lda.test.class!=data.test$g)
mean(lda.test.class==data.test$g)



# PART 3) 
#plotting the projection of observations of train data on first 2 dimensions of LDA

plot(lda.fit,dimen=2)
plot(lda.fit,dimen=3)

# PART 4)

# remove other response but ao, a
train_new <- subset(data.train, g == "ao"|g=="aa")
train_new <- transform(train_new,g=factor(g))
test_new <- subset(data.test, g == "ao"|g=="aa")
test_new <- transform(test_new,g=factor(g))

#new model for train data
lda.fit.new <- lda(as.formula(paste("g ~ ", f)), data = ungroup(train_new))
lda.fit.new

lda.train_new.pred <- predict(lda.fit.new, newdata = train_new)
# get prediction labels
lda.train_new.class=lda.train_new.pred$class
# error and accuracy
mean(lda.train_new.class!=train_new$g)
mean(lda.train_new.class==train_new$g)
#new model for test data
lda.fit.new <- lda(as.formula(paste("g ~ ", f)), data = ungroup(test_new))
lda.fit.new

lda.test_new.pred <- predict(lda.fit.new, newdata = test_new)
# get prediction labels
lda.test_new.class=lda.test_new.pred$class
# error and accuracy
mean(lda.test_new.class!=test_new$g)
mean(lda.test_new.class==test_new$g)
#Part 5)
#QDA model for train and test data 

# fit
qda.fit <- qda(as.formula(paste("g ~ ", f)), data = data.train)

qda.fit

# TRAIN ERROR

# predict train
qda.train.pred <- predict(qda.fit, newdata = data.train)
# get prediction labels
qda.train.class=qda.train.pred$class
# error and accuracy
mean(qda.train.class!=data.train$g)
mean(qda.train.class==data.train$g)

# TEST ERROR

# predict test
qda.test.pred <- predict(qda.fit, newdata = data.test)
# get prediction labels
qda.test.class=qda.test.pred$class
# error and accuracy
mean(qda.test.class!=data.test$g)
mean(qda.test.class==data.test$g)
#new model for train data
qda.fit.new <- qda(as.formula(paste("factor(g) ~ ", f)), data = ungroup(train_new))
qda.fit.new

qda.train_new.pred <- predict(qda.fit.new, newdata = train_new)
# get prediction labels
qda.train_new.class=qda.train_new.pred$class
# errorand accuracy
mean(qda.train_new.class!=train_new$g)
mean(qda.train_new.class==train_new$g)
#new model for test data
#test_new <- transform(test_new,g=factor(g))
qda.fit.new <- qda(as.formula(paste("factor(g) ~ ", f)), data = ungroup(test_new))
qda.fit.new

qda.test_new.pred <- predict(qda.fit.new, newdata = test_new)
# get prediction labels
qda.test_new.class=qda.test_new.pred$class
# error and accuracy
mean(qda.test_new.class!=test_new$g)
mean(qda.test_new.class==test_new$g)
#Part 6)
#Confusion matrices for lda train and test models that contains only aa and ao
conf_train<-table(lda.train_new.class, train_new$g)
conf_test<-table(lda.test_new.class, test_new$g)
conf_train
conf_test

#Confusion matrices for qda train and test models that contains only aa and ao
conf_train_qda<-table(qda.train_new.class, train_new$g)
conf_test_qda<-table(qda.test_new.class, test_new$g)
conf_train_qda
conf_test_qda

