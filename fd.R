data$Difficult_fd = data$Easy1_FixationDuration-data$Difficult_FixationDuration

fdf = data[,c("id","Easy2_fd","Moderate_fd","Difficult_fd","WMC_fd","Posttest_fd","output")]
describeBy(fdf,fdf$output)

fdf<-fdf[complete.cases(fdf),]

t.test(fdf$Difficult_fd~fdf$output)

mpdf=fdf[-1]
mpdf<-mpdf[complete.cases(mpdf),]
#Creating Train and Test set
index <- 1:nrow(pdf)
set.seed(52)
testindex <- sample(index, trunc(length(index)/3))
testset <- mpdf[testindex,]
trainset <- mpdf[-testindex,]

glm_model <- glm(output ~.,family = binomial,data=trainset)
summary(glm_model)
anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=testset,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testset$output)
print(paste('Accuracy',1-misClasificError))

library(randomForest)

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
