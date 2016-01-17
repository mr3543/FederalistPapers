#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 04
# Question 2
# 
#############################


naive.bayes = function(logp.hamilton.train,logp.madison.train,log.prior.hamilton,
                        log.prior.madison,dtm.test){
  
  preds = vector()
  for (i in 1:dim(dtm.test)[1]){
    p.madison = 0
    p.hamilton = 0
    for (j in 1:dim(dtm.test)[2]){
      if (dtm.test[i,j] > 0){
        p.madison = p.madison + logp.madison.train[j]
        p.hamilton = p.hamilton + logp.hamilton.train[j]
      }
    }
    
    p.madison = p.madison+log.prior.madison
    p.hamilton = p.hamilton+log.prior.hamilton
    
    if (p.hamilton > p.madison){
      preds = c(preds,1)
    }
    else{
      preds = c(preds,0)
    }
  }
  
  return (preds)
}
  