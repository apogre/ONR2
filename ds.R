data$Moderate_ds = data$Easy1_Distraction-data$Moderate_Distraction
dsdf = data[,c("id","Easy2_ds","Moderate_ds","Difficult_ds","WMC_ds","Posttest_ds","output")]

describeBy(dsdf,dsdf$output)

t.test(dsdf$WMC_ds~dsdf$output)
t.test(dsdf$Easy2_ds~dsdf$output)
t.test(dsdf$Moderate_ds~dsdf$output)
t.test(dsdf$Difficult_ds~dsdf$output)
t.test(dsdf$Posttest_ds~dsdf$output)#p-value = 0.06693

mpdf=dsdf[-1]
mpdf<-mpdf[complete.cases(mpdf),]
#Creating Train and Test set
index <- 1:nrow(pdf)
set.seed(127)
testindex <- sample(index, trunc(length(index)/3))
testset <- mpdf[testindex,]
trainset <- mpdf[-testindex,]

bestmtry <- tuneRF(trainset[-ncol(mpdf)],as.factor(trainset$output), ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
fit <-randomForest(as.factor(output)~.,data=trainset, mtry=2, ntree=1000, 
                   keep.forest=TRUE, importance=TRUE)
varImpPlot(fit)#Easy2/Posttest/Moderate
round(importance(fit), 2)
Prediction <- predict(fit, testset)
prediction <- predict(fit, testset[,-ncol(mpdf)])
tab <- table(pred = prediction, true = testset[,ncol(mpdf)])
tab
