#### Loading Required Libraries
library(stats)
library(rpart)
library(ISLR) 
library(tidyverse)
library(partykit)

#### Loading the data file and looking at data
Data<-stagec
#str(Data)

#### Creating train and test set 
n<-nrow(stagec)
train <- sample(1:n, 120)
test <- setdiff(1:n, train)

#### Implementing Adaboost algorithm

## creating the status based on pgstat 
status <- factor(stagec$pgstat, levels=c(0,1), labels=c(-1,1))

## initializations 
m<-length(train)
k<-length(test)
epsilon=0 # initialise epsilon
alpha=0 # initialise alpha
T<-500 #number of rounds 
Stump=matrix(data = NA, nrow = m, ncol = T) # where Stumps are to be store
pred=matrix(data = NA, nrow = 26, ncol = T) # where Stumps are to be store

## Algorithm 
for (t in seq(1:T))
{
    
    # construct distribution: D_{t}(i)=D_{t-1}(i)*F_i/Z
   if (t==1){
      D=rep(1/m, m) # particular case, D_1
           } 
    else
      {
        
      for (i in seq(1:m)){
          
          if (h[i]==status[i]) { 
          F[i]=exp(-alpha[t-1]) }
          
            else {    
            # F_i(for t-1)
            F[i]=exp(alpha[t-1]) 
                  }
          
        # D_{t}(i)=D_{t-1}(i)*F_i/Z    
        D[i]=D[i]*F[i] 
      }
        
       }
    
    # normalisation
    D=D/sum(D)
      

    # fit a single classification tree
    ctrl <- rpart.control(cp = 0, maxdepth = 1, xval = 0)

    fit.rp <- rpart(factor(status[train]) ~ age + eet + g2 + grade + gleason + ploidy,
              data = stagec[train,], method="class",weights = D, control = ctrl)

    # make predictions on the train data (needed for boosting)
    h <- predict(fit.rp, newdata =stagec[train,] , type="class")
    h <- as.numeric(as.character(h))   
    Stump[,t]<-h
    
    # make prediction on test set (needed for test error)
    p <- predict(fit.rp, newdata =stagec[test,] , type="class")
    p <- as.numeric(as.character(p))   
    pred[,t]<-p
    
   # compute error to get epsilon and alpha (based on train data)
   epsilon[t]=sum(D[!(status[train] == h)])
    
   alpha[t]=0.5*log((1-epsilon[t])/epsilon[t]) # alpha_{t}
    
}


## Last step: error on test set using the boosted model 

final<-matrix(data = 0, nrow =k , ncol = 1)

for (t in seq(1:T)){
final=final+ alpha[t]*pred[,t]
   }

final<-sign(final)
accuracytable<-table(final, status[test])
accuracytable

error=(accuracytable[1, 2]+accuracytable[2, 1])/k
paste0("Testing error after using Adaboost is:  ",error)

#### Test Error based on a single classification tree

## fit a single classification tree
fitted <- rpart(status[train] ~ age + eet + g2 + grade + gleason + ploidy,
              data = stagec[train,], method="class")

## make predictions on the test data
pred <- predict(fitted, newdata = stagec[test,], type="class")
err<- mean(status[test] != pred)  # the test error

paste0("Testing error after using a single classification tree is:  ",err)
