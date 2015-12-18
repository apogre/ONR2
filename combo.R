cdf = data[,c("id","WMC_fd","WMC_pupilsize","Difficult_wl","WMC_wl","output")]
cdf = data[,c("id","WMC_fd","WMC_pupilsize","Difficult_wl","WMC_wl","Easy2_he","Moderate_he","output")]

cdf = data[,c("id","WMC_fd","WMC_pupilsize","Posttest_pupilsize","Difficult_wl","WMC_wl","Easy2_he","Moderate_he","Difficult_he","Easy2_dr","WMC_dr","output")]
cdf = data[,c("id","WMC_fd","WMC_pupilsize","Posttest_pupilsize","Difficult_wl","WMC_wl","Easy2_he","Moderate_he","Difficult_he","Easy2_dr","WMC_dr","Easy2_ds","Moderate_ds","Posttest_ds","Easy2_le","Moderate_le","WMC_le","Posttest_le","output")]

cdf$output = as.factor(cdf$output)
describeBy(cdf,cdf$output)
t.test(cdf$Easy2_he~cdf$output)

mpdf=cdf[-1]
mpdf<-mpdf[complete.cases(mpdf),]
#Creating Train and Test set
index <- 1:nrow(pdf)
set.seed(127)
testindex <- sample(index, trunc(length(index)/3))
testset <- mpdf[testindex,]
trainset <- mpdf[-testindex,]

glm_model <- glm(output ~.,family = binomial,data=trainset)
summary(glm_model)
anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=testset,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != as.factor(testset$output))
print(paste('Accuracy',1-misClasificError))


bestmtry <- tuneRF(trainset[-ncol(mpdf)],as.factor(trainset$output), ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
fit <-randomForest(output~.,data=trainset, mtry=2, ntree=1000, 
                   keep.forest=TRUE, importance=TRUE)
varImpPlot(fit)
round(importance(fit), 2)
Prediction <- predict(fit, testset)
prediction <- predict(fit, testset[,-ncol(mpdf)])
tab <- table(pred = prediction, true = testset[,ncol(mpdf)])
tab
