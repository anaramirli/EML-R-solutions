
install.packages("dplyr")
library(dplyr)

install.packages("MASS")
library(MASS)

# PART 1)

# load data
data <- read.csv("phoneme.csv")


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

# predict test
lda.train.pred <- predict(lda.fit, newdata = data.train)
# get prediction labels
lda.train.class=lda.train.pred$class
# error
mean(lda.train.class!=data.train$g)


# TEST ERROR

# predict test
lda.test.pred <- predict(lda.fit, newdata = data.test)
# get prediction labels
lda.test.class=lda.test.pred$class
# error
mean(lda.test.class!=data.test$g)


# print talble (confussion matrix)
# table(lda.train.class, data.train$g)


# PART 3) 

plot(lda.fit)

# PART 4)
# eroor var kodda
# remove other response but ao, a
data.train.new <-subset(data.train, g=="ao" | g=="aa")
data.test.new <-subset(data.train, g=="ao" | g=="aa")


lda.fit.new <- lda(as.formula(paste("g ~ ", f)), data = ungroup(data.train.new))

