#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 04
# Question 3
# 
#############################

logp.hamilton.train = make.log.pvec(dtm.hamilton.train,1/dim(dict)[1])
logp.madison.train = make.log.pvec(dtm.madison.train,1/dim(dict)[1])

hprior = dim(dtm.hamilton.train)[1]/(dim(dtm.hamilton.train)[1] + dim(dtm.madison.train)[1])
mprior = dim(dtm.madison.train)[1]/(dim(dtm.hamilton.train)[1] + dim(dtm.madison.train)[1])

hclass = naive.bayes(logp.hamilton.train,logp.madison.train,log(hprior),log(mprior),dtm.hamilton.test)
mclass = naive.bayes(logp.hamilton.train,logp.madison.train,log(hprior),log(mprior),dtm.madison.test)

correct = 0
falsePos = 0
falseNeg = 0
for (i in 1:length(hclass)){
  if (hclass[i] == 1){
    correct = correct + 1
  }
  else{
    falseNeg = falseNeg + 1
  }
  
}

for (i in 1:length(mclass)){
  if (mclass[i] == 0){
    correct = correct +1
  }
  else {
    falsePos = falsePos +1
  }
}

pCorrect = correct/(length(hclass)+length(mclass))
pFpos = falsePos/length(mclass)
pFneg = falseNeg/length(hclass)




