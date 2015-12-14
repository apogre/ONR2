library(psych)
require(ggplot2)

getwd()
setwd("C:/Users/apradha7/Downloads/Lab/ONR2/onr_data")
final_data<-read.table("revised_data.csv",header=TRUE,sep=",",fill =TRUE,quote="",comment.char = "",allowEscapes=TRUE,stringsAsFactors=TRUE)

#Describe the data [looks similar in both cases]
describeBy(final_data,final_data$Output)

##Normality test
shapiro.test(final_data$Difficult_HighEngagement)
qqnorm(final_data$Difficult_HighEngagement)

#ran quick t-test to verify
t.test(final_data$Difficult_HighEngagement~final_data$Output)

final_data$Output = as.factor(final_data$Output)
final_data$Gender_1_Male = as.factor(final_data$Gender_1_Male)
ggplot(final_data, aes(x=Output,y=Difficult_HighEngagement))+
  geom_boxplot()

##Linear model
model1 = lm(Difficult_HighEngagement ~ Output, data = final_data)
summary(model1)

#Creating Train and Test set
index <- 1:nrow(mydata)
set.seed(52)
testindex <- sample(index, trunc(length(index)/3))
testset <- mydata[testindex,]
trainset <- mydata[-testindex,]

#Logistic Regression["Accuracy 0.476190476190476"]

glm_model <- glm(Output ~.,family = binomial,data=trainset)
summary(glm_model)
anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=testset,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testset$Output)
print(paste('Accuracy',1-misClasificError))

#K-means Clustering
df = scale(final_data[,3:7])

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(df)

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 4, nstart=25)                          
fit.km$size
fit.km$centers
aggregate(final_data[,3:7], by=list(cluster=fit.km$cluster), mean)
ct.km <- table(final_data$Output, fit.km$cluster)
ct.km

##Random Forest
library(rpart)
fit = rpart(Output~ ., method="class",data=final_data[-1])
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE, main="Classification Tree for Constructs")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

library(randomForest)
final_data$Output = as.factor(final_data$Output)
final_data$Gender_1_Male = as.factor(final_data$Gender_1_Male)
mydata = final_data[-1]
index <- 1:nrow(mydata)
set.seed(52)
testindex <- sample(index, trunc(length(index)/3))
testset <- mydata[testindex,]
trainset <- mydata[-testindex,]

bestmtry <- tuneRF(trainset[-ncol(mydata)],trainset$Output, ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
fit <-randomForest(Output~.,data=trainset, mtry=2, ntree=1000, 
                   keep.forest=TRUE, importance=TRUE)
varImpPlot(fit)
round(importance(fit), 2)
Prediction <- predict(fit, testset)
prediction <- predict(fit, testset[,-ncol(mydata)])
tab <- table(pred = prediction, true = testset[,ncol(mydata)])
tab
