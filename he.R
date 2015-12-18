data$Difficult_he = data$Easy1_HighEngagement-data$Difficult_HighEngagement


hdf = data[,c("id","Easy2_he","Moderate_he","Difficult_he","WMC_he","Posttest_he","output")]
describeBy(hdf,hdf$output)


t.test(hdf$Posttest_he~hdf$output)

mpdf=hdf[-1]
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
varImpPlot(fit)
round(importance(fit), 2)
Prediction <- predict(fit, testset)
prediction <- predict(fit, testset[,-ncol(mpdf)])
tab <- table(pred = prediction, true = testset[,ncol(mpdf)])
tab
