#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 05
# Question 7
# 
#############################

source('hw4.R')
mi = rep(NA,ncol(dtm.training)-1)
dtm.hamilton = rbind(dtm.hamilton.test,dtm.hamilton.train)
dtm.madison = rbind(dtm.madison.test,dtm.madison.train)

ex = nrow(dtm.training)+nrow(dtm.testing)
log.prior.hamilton = log((nrow(dtm.hamilton.test) + nrow(dtm.hamilton.train))/ex,2)
log.prior.madison = log((nrow(dtm.madison.test) + nrow(dtm.madison.train))/ex,2)

nvec = c(200,500,1000,2500)
pcorGini = rep(NA,length(nvec))
fposGini = rep(NA,length(nvec))
fnegGini = rep(NA,length(nvec))
pcorInfo = rep(NA,length(nvec))
fposInfo = rep(NA,length(nvec))
fnegInfo = rep(NA,length(nvec))
pcorRidge = rep(NA,length(nvec))
fposRidge = rep(NA,length(nvec))
fnegRidge = rep(NA,length(nvec))
pcorLasso = rep(NA,length(nvec))
fposLasso = rep(NA,length(nvec))
fnegLasso = rep(NA,length(nvec))

d = ncol(dtm.training)

hamilton.prior = nrow(dtm.hamilton)/(nrow(dtm.hamilton) + nrow(dtm.madison))
madison.prior = nrow(dtm.madison)/(nrow(dtm.hamilton) + nrow(dtm.madison))


for (i in 1:length(nvec)){
  n = nvec[i]
  hamilton.conds = make.log.pvec(dtm.hamilton,1/n)
  madison.conds = make.log.pvec(dtm.madison,1/n)
  
  inv.hamilton.conds = log((1-exp(hamilton.conds)))
  inv.madison.conds = log((1-exp(madison.conds)))
  total.prob = make.log.pvec(rbind(dtm.madison,dtm.hamilton),1/n)
  inv.total.prob = log((1-exp(total.prob)))
  p.joint.ham = hamilton.conds + log.prior.hamilton
  p.joint.mad = madison.conds + log.prior.madison
  
  for(k in 1:length(mi)){
    mi[k] = p.joint.ham[k]*log(hamilton.conds[k]/total.prob[k]) + ((inv.hamilton.conds[k])+log.prior.hamilton)*log((inv.hamilton.conds[k])/(inv.total.prob[k]),2)
    + p.joint.mad[k]*log(madison.conds[k]/total.prob[k]) + ((inv.madison.conds[k])+log.prior.madison)*log((inv.madison.conds[k])/(inv.total.prob[k]),2)
  }
  inds = seq(1,d)
  inds = inds[order(mi,decreasing=TRUE)]
  
  gini.model = rpart(dtm.training$y~.,data=dtm.training[,inds[1:n],d],method = "class")
  info.model = rpart(dtm.training$y~.,data=dtm.training[,inds[1:n],d],method = "class",parms = list(split='information'))
  ridge.model = cv.glmnet(x= dtm.training.scaled[,inds[1:n]],y=class.vector,family = "binomial",alpha=0,standardize = FALSE)
  lasso.model = cv.glmnet(x=dtm.training.scaled[,inds[1:n]],y=class.vector,family = "binomial",alpha=1,standardize = FALSE)
  
  gini.predict = predict(gini.model,newdata = dtm.testing[,inds[1:n]],type = "class")
  info.predict = predict(gini.model,newdata = dtm.testing[,inds[1:n]],type = "class")
  ridge.predict = predict(ridge.model,dtm.testing.scaled[,inds[1:n]],type = "response",s = "lambda.min")
  lasso.predict = predict(lasso.model,dtm.testing.scaled[,inds[1:n]],type = "response",s = "lambda.min")
  
  gp = as.vector(gini.predict, mode = "integer")
  ip = as.vector(info.predict, mode = "integer")
  
  for(j in 1:length(gp)){
    gp[j] = gp[j] -1
    ip[j] = ip[j] -1
  }

  pcorGini[i] = (sum(gp[1:16] == 1) + sum(gp[16:27]==0))/nrow(dtm.testing)
  pcorInfo[i] = (sum(ip[1:16] == 1) + sum(ip[16:27]==0))/nrow(dtm.testing)
  pcorRidge[i] = (sum(ridge.predict[1:16] > .5) + sum(ridge.predict[16:27]<=.5))/nrow(dtm.testing)
  pcorLasso[i] = (sum(lasso.predict[1:16] > .5) + sum(lasso.predict[16:27]<=.5))/nrow(dtm.testing)
  
  fposGini[i] = sum(gp[16:27] == 1)/11
  fposInfo[i] = sum(ip[16:27] == 1)/11
  fposRidge[i] = sum(ridge.predict[16:27] > .5)/12
  fposLasso[i] = sum(lasso.predict[16:27] > .5)/12
  
  fnegGini[i] = sum(gp[1:16] == 0)/16
  fnegInfo[i] = sum(ip[1:16] == 0)/16
  fnegRidge[i] = sum(ridge.predict[1:16] <= .5)/15
  fnegLasso[i] = sum(lasso.predict[1:16] <= .5)/15
  
}

plot(y=pcorGini,x=nvec,type="l",main = "percent correct",xlab="n",ylab="pcorrect",col ="yellow")
lines(y=pcorInfo,x=nvec,col="red")
lines(y=pcorRidge,x=nvec,col="blue")
lines(y=pcorLasso,x=nvec,col = "green")

plot(y=fposGini,x=nvec,type="l",main = "false pos",xlab="n",ylab="pFalsePos",col = "yellow")
lines(y=fposInfo,x=nvec,col="red")
lines(y=fposRidge,x=nvec,col="blue")
lines(y=fposLasso,x=nvec,col="green")

plot(y=fnegGini,x=nvec,type="l",main = "false neg",xlab="n",ylab="pFalseNeg",col="yellow")
lines(y=fnegInfo,x=nvec,col="red")
lines(y=fnegRidge,x=nvec,col="blue")
lines(y=fnegLasso,x=nvec,col="green")





