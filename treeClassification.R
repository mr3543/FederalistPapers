#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 05
# Question 5
# 
#############################

#part(a)

source('NBLib.R')
library(rpart)
library(rpart.plot)

hamilton.train = read.directory("fp_hamilton_train_clean")
hamilton.test = read.directory("fp_hamilton_test_clean")
madison.train = read.directory("fp_madison_train_clean")
madison.test = read.directory("fp_madison_test_clean")

l = c(madison.train,hamilton.train,madison.test,hamilton.test)
dict = make.sorted.dictionary.df(l)

dtm.hamilton.train = make.document.term.matrix(hamilton.train,dict)
dtm.hamilton.test = make.document.term.matrix(hamilton.test,dict)
dtm.madison.train = make.document.term.matrix(madison.train,dict)
dtm.madison.test = make.document.term.matrix(madison.test,dict)

hamilton.train.response = rep(1,dim(dtm.hamilton.train)[1])
hamilton.test.response = rep(1,dim(dtm.hamilton.test)[1])
madison.train.response = rep(0,dim(dtm.madison.train)[1])
madison.test.response = rep(0,dim(dtm.madison.test)[1])

dtm.training = as.data.frame(rbind(cbind(dtm.hamilton.train,as.matrix(hamilton.train.response)), 
                     cbind(dtm.madison.train,as.matrix(madison.train.response))))

dtm.testing = as.data.frame(rbind(cbind(dtm.hamilton.test,as.matrix(hamilton.test.response)),
                    cbind(dtm.madison.test,as.matrix(madison.test.response))))

colnames(dtm.training) = c(as.vector(dict$word),"y")
colnames(dtm.testing) = c(as.vector(dict$word),"y")

set.seed(100)
i = runif(nrow(dtm.training))
dtm.training.shuffled = dtm.training[order(i),]

d = dim(dtm.training)[2]
training.model= rpart(y ~ ., data = dtm.training,method = "class")
test.predictions = predict(training.model,newdata = dtm.testing[,1:(d-1)],type = "class")
test.predicitons.m = as.matrix(test.predictions)

correct1 = 0
falsePos1 = 0
falseNeg1 = 0

for (i in 1:dim(test.predicitons.m)[1]){
  if (test.predicitons.m[i] == as.factor(dtm.testing$y[i])){
    correct1 = correct1 + 1
  }
  else if (as.factor(dtm.testing$y[i] == "0")){
    falsePos1 = falsePos1 +1
  }
    else{
    falseNeg1 = falseNeg1 +1
  }
}

corPct1 = correct1/dim(test.predicitons.m)[1]
fpPct1 = falsePos1/dim(test.predicitons.m)[1]
fnPct1 = falseNeg1/dim(test.predicitons.m)[1]
rpart.plot(training.model)
text(training.model,use.n = TRUE)

#part(b)

training.model2 = rpart(y ~ ., data = dtm.training,method = "class",parms = list(split='information'))
test.predictions2 = predict(training.model2,newdata = dtm.testing[,1:(d-1)],type = "class")
test.predicitons.m2 = as.matrix(test.predictions2)

correct2 = 0
falsePos2 = 0
falseNeg2 = 0

for (i in 1:dim(test.predicitons.m2)[1]){
  if (test.predicitons.m2[i] == as.factor(dtm.testing$y[i])){
    correct2 = correct2 + 1
  }
  else if (as.factor(dtm.testing$y[i] == "0")){
    falsePos2 = falsePos2 +1
  }
  else{
    falseNeg2 = falseNeg2 +1
  }
}

corPct2 = correct2/dim(test.predicitons.m)[1]
fpPct2 = falsePos2/dim(test.predicitons.m)[1]
fnPct2 = falseNeg2/dim(test.predicitons.m)[1]
rpart.plot(training.model2)
text(training.model2,use.n = TRUE)





