###Author: Shirui Wang Jue Wang Dong Dai


setwd("C:\\Users\\jue wang\\Desktop\\kaggle")
tr<-read.csv("cleanedTrain.csv",header=T)
te<-read.csv("cleanedTest.csv",header=T)
library(Metrics)
library(caret)
library(xgboost)
install.packages("plyr")
library(plyr)
install.packages("Boruta")
library(Boruta)
library(ranger)
#######xgboost##########
fac2dum<-function(dfr, betweenColAndLevel="") {
  nc<-dim(dfr)[2]
  firstRow<-dfr[1,]
  coln<-colnames(dfr)
  retval<-do.call(cbind, lapply(seq(nc), function(ci){
    if(is.factor(firstRow[,ci]))
    {
      lvls<-levels(firstRow[,ci])[-1]
      stretchedcols<-sapply(lvls, function(lvl){
        rv<-dfr[,ci]==lvl
        mode(rv)<-"integer"
        return(rv)
      })
      if(!is.matrix(stretchedcols))
        stretchedcols<-matrix(stretchedcols, nrow=1)
      colnames(stretchedcols)<-paste(coln[ci],
                                     lvls, sep=betweenColAndLevel)
      return(stretchedcols)
    }
    else
    {
      curcol<-matrix(dfr[,ci], ncol=1)
      colnames(curcol)<-coln[ci]
      return(curcol)
    }
  }))
  rownames(retval)<-rownames(dfr)
  return(retval)
}
y = tr$SalePrice
X = fac2dum(tr[, c(2:52, 54)])
par.xg = data.frame(mtry=1:10)
ctrl.xg = trainControl(method="repeatedcv", repeats=10, number=10)
xgbstTune = train(y=y, x=X, method="xgbTree", 
                  trControl=ctrl.xg, 
                  preProc=c("center", "scale"), 
                  allowParallel=T)
xgbstTune$bestTune
save(xgbstTune, file="../result/xgTune.rda")
load(file="../result/xgTune.rda")
x.test = test[, names(train[, c(2:52, 54)])]
x.test = fac2dum(x.test)
y.test = test$SalePrice

stopCluster(cluster)
registerDoSEQ()
y = te$SalePrice
X = fac2dum(te[, c(2:10, 54)])
predict(xgbstTune,X)


##########VARIABLE SELECTION###
library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)
set.seed(13)
bor.results <- Boruta(tr[,-82],tr$SalePrice,
                      maxRuns=101,
                      doTrace=0)
write.csv(bor.results$finalDecision,"bor.csv")
varsel=read.csv("bor.csv",header = T)
vs<-varsel$X[varsel$x == 'Confirmed']
tr2<-tr[,vs]
te2<-te[,vs]

##OLS##
tr2$GarageYrBlt<-as.numeric(tr2$GarageYrBlt)
te2$GarageYrBlt<-as.numeric(te2$GarageYrBlt)

tr2$saleprice <- log(tr$SalePrice)
olsfit<-lm(saleprice~.,tr2)

olspred<-predict(olsfit,tr2)
sqrt(mean(olsfit$residuals)^2)
rmse(olsfit$fitted.values,tr2$saleprice)

##0.1154711
##########glmnet########
library(glmnet)
glmnetfit<-train(y=tr2$saleprice,
           x=tr2[,-53],
           method="glmnet",
           tuneGrid=data.frame(alpha=seq(0.1,1,0.1),lambda=seq(0.1,1,0.1)),
           trControl=trainControl(method="repeatedcv",
                                  repeats=10,number=8))
########random forest###
library(randomForest)



####pls###
library(pls)
plsfit<-train(y=tr2$saleprice,
                 x=tr2[,-53],
                 method="pls",
                 trControl=trainControl(method="repeatedcv",
                                        repeats=10,number=8))
plsfit$bestTune
##  ncomp
#3     3
min(plsfit$results$RMSE)
##[1] 0.2482014

#### pcr


####nnet######

nnetfitW <- train(y=tr2$saleprice,
                  x=tr2[,-53],
                  method = "nnet",
                  tunegrid =  expand.grid(.size=seq(1,8,1),.decay=seq(0.01,0.91,0.03)),
                  trControl = trainControl(method="repeatedcv",
                                           repeats=10,number=10,
                                           summaryFunction = defaultSummary)
)
##########r part######

library(rpart)
rpartFit <- train(y=tr2$saleprice,
                  x=tr2[,-53],
                  "rpart",
                  preProcess=c("center","scale"), 
                  tuneLength=100,
                  trControl = trainControl(method="repeatedcv",
                                           repeats=10,number=8,
                                           summaryFunction = defaultSummary
                  ))
rpartFit$bestTune
#0.0002459326
min(rpartFit$results$RMSE)

###0.1923468

############
