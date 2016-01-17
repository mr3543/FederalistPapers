#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 05
# Question 5
# 
#############################

# part(b)
library(glmnet)
dm = dim(dtm.training)[2]
dtm.training.scaled = scale(as.matrix(dtm.training[,1:(dm-1)]),center = TRUE,scale = FALSE)
dtm.testing.scaled =scale(as.matrix(dtm.testing[,1:(dm-1)]),center = TRUE,scale = FALSE)

for (i in 1:(dm-1)){
  if (! (sd(dtm.training.scaled[,i]) == 0)){
    dtm.training.scaled[,i] = dtm.training.scaled[,i]*(1/sd(dtm.training.scaled[,i]))
  }
  if (!(sd(dtm.testing.scaled[,i]) == 0)){
    dtm.testing.scaled[,i] = dtm.testing.scaled[,i]*(1/sd(dtm.testing.scaled[,i]))
  }
}

class.vector = as.factor(dtm.training$y)
ridge.model = cv.glmnet(x=dtm.training.scaled[,1:(dm-1)],y=class.vector,family = 'binomial',alpha = 0,standardize = FALSE)

ypred.ridge.resp = predict(ridge.model,dtm.testing.scaled[,1:(dm-1)],type = "response",s = ridge.model$lambda.min)
ypred.ridge.coef = predict(ridge.model,dtm.testing.scaled[,1:(dm-1)],type = "coefficients",s = ridge.model$lambda.min)

coefAbs.ridge = rownames(ypred.ridge.coef)[order(abs(ypred.ridge.coef[,1]),decreasing = TRUE)]

correct.ridge = 0
fpos.ridge = 0
fneg.ridge = 0


for (i in 1:dim(dtm.testing)[1]){
  if (abs(dtm.testing[i,dm] - ypred.ridge.resp[i]) < .5){
    correct.ridge = correct.ridge + 1
  }
  else if (dtm.testing[i,dm] == 1){
    fpos.ridge = fpos.ridge + 1
  }
  else{
    fneg.ridge = fneg.ridge + 1
  }
}

pctCor.ridge = correct.ridge/dim(dtm.testing)[1]
pctfpos.ridge = fpos.ridge/dim(dtm.testing)[1]
pctfneg.ridge = fneg.ridge/dim(dtm.testing)[1]

#part(c)

lasso.model = cv.glmnet(x=dtm.training.scaled[,1:(dm-1)],y=class.vector,family = 'binomial',alpha = 1,standardize = FALSE)

ypred.lasso.resp = predict(lasso.model,dtm.testing.scaled[,1:(dm-1)],type = "response", s = "lambda.min")
ypred.lasso.coef = predict(lasso.model,dtm.testing.scaled[,1:(dm-1)],type = "coefficients",s = "lambda.min")

coefAbs.lasso = rownames(ypred.lasso.coef)[order(abs(ypred.lasso.coef[,1]),decreasing = TRUE)]

correct.lasso = 0
fpos.lasso = 0
fneg.lasso = 0


for (i in 1:dim(dtm.testing)[1]){
  if (abs(dtm.testing[i,dm] - ypred.lasso.resp[i]) < .5){
    correct.lasso = correct.lasso + 1
  }
  else if (dtm.testing[i,dm] == 1){
    fpos.lasso = fpos.lasso + 1
  }
  else{
    fneg.lasso = fneg.lasso + 1
  }
}

pctCor.lasso = correct.lasso/dim(dtm.testing)[1]
pctfpos.lasso = fpos.lasso/dim(dtm.testing)[1]
pctfneg.lasso = fneg.lasso/dim(dtm.testing)[1]
