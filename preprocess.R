#############################
# 
# Michael Regan
# mr3543
# STAT W4240 
# Homework 04
# Question 1
# 
#############################
#Part 1:

source('NBLib.R')

#preprocess.directory("fp_hamilton_train")
#preprocess.directory("fp_hamilton_test")
#preprocess.directory("fp_madison_test")
#preprocess.directory("fp_madison_train")


#Part 2:

hamilton.train = read.directory("fp_hamilton_train_clean")
hamilton.test = read.directory("fp_hamilton_test_clean")
madison.train = read.directory("fp_madison_train_clean")
madison.test = read.directory("fp_madison_test_clean")

#Part 3:

l = c(madison.train,hamilton.test,hamilton.train,madison.test)
dict = make.sorted.dictionary.df(l)

#Part 4:

dtm.hamilton.train = make.document.term.matrix(hamilton.train,dict)
dtm.hamilton.test = make.document.term.matrix(hamilton.test,dict)
dtm.madison.train = make.document.term.matrix(madison.train,dict)
dtm.madison.test = make.document.term.matrix(madison.test,dict)

#Part 5:

logp.hamilton.train = make.log.pvec(dtm.hamilton.train,1/dim(dict)[1])
logp.hamilton.test = make.log.pvec(dtm.hamilton.test,1/dim(dict)[1])
logp.madison.train = make.log.pvec(dtm.madison.train,1/dim(dict)[1])
logp.madison.test = make.log.pvec(dtm.madison.test,1/dim(dict)[1])


