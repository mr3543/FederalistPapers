#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 04
# Question 3
# 
#############################

#(a)

library(cvTools)

l = c(madison.train,hamilton.train)
dict = make.sorted.dictionary.df(l)


muvec = c(1/10*(1/dim(dict)[1]), 1/dim(dict)[1],10*1/dim(dict)[1],100*1/dim(dict)[1],1000*1/dim(dict)[1])
corCV = matrix(data=0,nrow = 5, ncol = 5)
falseposCV = matrix(data=0,nrow=5,ncol=5)
falsenegCV = matrix(data=0,nrow=5,ncol=5)
k=5
hprior = dim(dtm.hamilton.train)[1]/(dim(dtm.hamilton.train)[1] + dim(dtm.madison.train)[1])
mprior = dim(dtm.madison.train)[1]/(dim(dtm.hamilton.train)[1] + dim(dtm.madison.train)[1])

for (i in 1:length(muvec)){
  HamFolds = cvFolds(nrow(dtm.hamilton.train),k)
  MadFolds = cvFolds(nrow(dtm.madison.train),k)
  
  for (j in 1:k){
  
    hamTrain = dtm.hamilton.train[HamFolds$subsets[HamFolds$which != j],]
    madTrain = dtm.madison.train[MadFolds$subsets[MadFolds$which != j],]
    hamTest = dtm.hamilton.train[HamFolds$subsets[HamFolds$which == j],]
    madTest = dtm.madison.train[MadFolds$subsets[MadFolds$which == j],]
    
    logp.hamTrain = make.log.pvec(hamTrain,muvec[i])
    logp.madTrain = make.log.pvec(madTrain,muvec[i])
    
    hamClass = naive.bayes(logp.hamTrain,logp.madTrain,log(hprior),log(mprior),hamTest)
    madClass = naive.bayes(logp.hamTrain,logp.madTrain,log(hprior),log(mprior),madTest)
    
    tcor = 0
    fneg = 0
    fpos = 0
    for(l in 1:length(hamClass)){
      
      if (hamClass[l] == 1){
        tcor = tcor +1
      }
      else{
        fneg = fneg + 1
      }
    }
    
    for (l in 1:length(madClass)){
      if (madClass[l] == 0){
        tcor = tcor + 1
      }
      else{
        fpos = fpos + 1
      }
    }
    
    pcor = tcor/(length(hamClass) + length(madClass))
    pfpos = fpos/length(madClass)
    pfneg = fneg/length(hamClass)
    corCV[i,j] = pcor
    falseposCV[i,j] = pfpos
    falsenegCV[i,j] = pfneg
    
  }
}

pcorEstsCV = rowMeans(corCV)
plot(pcorEstsCV)
title(main="Percentage Correct CV")
fposEstsCV = rowMeans(falseposCV)
plot(fposEstsCV)
title(main="False Positives Pct CV")
fnegEstsCV = rowMeans(falsenegCV)
plot(fnegEstsCV)
title(main="False Negatives Pct CV")

#(c)

cor = rep(0,5)
falsepos = rep(0,5)
falseneg = rep(0,5)

logp.hamTrain = make.log.pvec(dtm.hamilton.train)
logp.madTrain = make.log.pvec(dtm.madison.train)



for (i in 1:length(muvec)){
  logp.hamTrain = make.log.pvec(dtm.hamilton.train,muvec[i])
  logp.madTrain = make.log.pvec(dtm.madison.train,muvec[i])
  
  hamClass = naive.bayes(logp.hamTrain,logp.madTrain,log(hprior),log(mprior),dtm.hamilton.test)
  madClass = naive.bayes(logp.hamTrain,logp.madTrain,log(hprior),log(mprior),dtm.madison.test)
  
  
  tcor = 0
  fneg = 0
  fpos = 0
  for(l in 1:length(hamClass)){
    
    if (hamClass[l] == 1){
      tcor = tcor +1
    }
    else{
      fneg = fneg + 1
    }
  }
  
  for (l in 1:length(madClass)){
    if (madClass[l] == 0){
      tcor = tcor + 1
    }
    else{
      fpos = fpos + 1
    }
  }
  
  pcor = tcor/(length(hamClass) + length(madClass))
  pfpos = fpos/length(madClass)
  pfneg = fneg/length(hamClass)
  cor[i] = pcor
  falsepos[i] = pfpos
  falseneg[i] = pfneg
  
  }


plot(cor)
title(main="Percentage Correct")
plot(falsepos)
title(main="False Positives Pct")
plot(falseneg)
title(main="False Negatives Pct")

# (d)

corDiffs = pcorEstsCV - cor
fposDiffs = fposEstsCV - falsepos
fnegDiffs = fnegEstsCV - falseneg

