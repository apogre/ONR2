install.packages('e1071', dependencies = FALSE)
install.packages('caret', dependencies = TRUE)

library(reshape2)
library(plyr)
library(e1071)
library(ggplot2)
library(caret)
library(doSMP)
library(AppliedPredictiveModeling)
library(randomForest)

library(xlsx)
write.xlsx(avg, "C:/Users/apradha7/Desktop/avg.xls") 

transparentTheme(trans = .4)

getwd()
setwd("C:/Users/apradha7/Downloads/Lab/ONR2/onr_data")



file_list = list.files("./ONR_S2") #Get all the files from the folder
count=0

for (f in file_list){
  print(f)
  # f="Dump008_N100.txt"
  
  myfile = paste("ONR_S2/",f,sep="")
  data = read.csv(file=myfile, header=TRUE, fill=TRUE,skip = 5, sep = "\t")
  print("data read")
  if(ncol(data)<104){
    print("columns less than required")
    next
  }
  
  ex_data <- data[,c("Age","Gender","StimulusName","EventSource","UTCTimestamp","PupilLeft","PupilRight","FixationDuration","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadAverage")]
  
  ex_data$PupilLeft[ex_data$PupilLeft == -1.000]<-NA
  ex_data$PupilRight[ex_data$PupilRight == -1.000]<-NA
  ex_data$WorkloadAverage[ex_data$WorkloadAverage == -99999.00]<-NA
  
  
  ex_data$StimulusName<-factor(ex_data$StimulusName,c("Easy1","Easy2","Moderate","Difficult","Posttest","WMC"))
 
  # ex_data$StimulusName<-factor(ex_data$StimulusName,c("Easy1","Easy2","Moderate","Difficult"))
  
  avg<-aggregate(ex_data[,6:13],list(StimulusName=ex_data$StimulusName),mean,na.rm=TRUE)
  avg$pupil=rowMeans(avg[,c("PupilLeft", "PupilRight")], na.rm=TRUE)
  avg[2:3]=list(NULL)
  avg <- avg[c("StimulusName","pupil","FixationDuration","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadAverage")]
  avg
  x<-melt(avg)
  id<-sapply(strsplit(as.character(f), split = "[_.]"), "[", 2)
  # get_row<-pas_fail[pas_fail$id==as.character(id),]
  
  if(count==0){
    existingDF<-dcast(x,id~StimulusName+variable)
    existingDF$Age<-ex_data$Age[1]
    existingDF$Gender<-ex_data$Gender[1]
    # existingDF$Difficult_Status<-get_row$L2.Difficult
  }
  else{
    newDF <- dcast(x,id~StimulusName+variable)
    newDF$Age<-ex_data$Age[1]
    newDF$Gender<-ex_data$Gender[1]
    DF <- rbind.fill(existingDF,newDF)
    # DF$Difficult_Status<-get_row$L2.Difficult
    existingDF=DF
  }
  count = count+1
  
}

newfile = paste("ONR_S2_Pre/",f,sep="")
write.table(existingDF, file=newfile,row.names = FALSE,sep = "," ,col.names=TRUE, quote= FALSE)
print("data written in csv")

#Merging the sensor data with output data.
pas_fail<-read.csv(file="ONR_Pass_FAIL.tsv", header=TRUE, fill=TRUE, sep = "\t")
final<-read.csv(file="final.txt", header=TRUE, fill=TRUE, sep = ",")


  finalID <-final[1]
  for(id in 1:length(finalID[[1]])){
    id_1= as.character(finalID[[1]][id])
    print(id_1[[1]])
    p_row <-pas_fail[pas_fail$id==id_1,]
    print(p_row$L2.Difficult)
    final$output[id]<-p_row$L2.Difficult
  }
  
  write.table(data, file='data.txt',row.names = FALSE,sep = "," ,col.names=TRUE, quote= FALSE)
  print("data written in csv")
  

  #SVM implementation
  final_data<-read.table("revised_data.csv",header=TRUE,sep=",",fill =TRUE,quote="",comment.char = "",allowEscapes=TRUE,stringsAsFactors=TRUE)
  final_data$Gender = as.character(final_data$Gender)
  final_data$Gender[as.character(final_data$Gender) == "MALE"] <- "1"
  final_data$Gender[as.character(final_data$Gender) == "FEMALE"] <- "0"
  final_data$Age[final_data$Age < 20] <- 0
  final_data$Age[final_data$Age >= 20] <- 1
  
  #Removing id
  mydata <- final_data[c(-1)]
  
  
  #Removing NA's
  mydata<-mydata[complete.cases(mydata),]
  
  mydata$output <-factor(mydata$output)
  mydata$Age <-factor(mydata$Age)
  mydata$Gender <-factor(mydata$Gender) 
  
  mydata[29:44] <- list(NULL) 
  
#   model <- svm(output~.,data=mydata)
#   print(model)
#   summary(model)
#   
  
  #Creating Train and Test set
  index <- 1:nrow(mydata)
  set.seed(52)
  testindex <- sample(index, trunc(length(index)/3))
  testset <- mydata[testindex,]
  trainset <- mydata[-testindex,]
  
  set.seed(345)
  testindex <- createDataPartition(mydata$output, p = .2,
                                    list = FALSE,
                                    times = 1)
  testset <- mydata[testindex,]
  trainset <- mydata[-testindex,]
  
  tuned <- tune.svm(Output~., data = mydata, gamma = 10^(-6:-1), cost = 10^(1:2))
  summary(tuned)
  
  bestGamma <- tuned$best.parameters[[1]]
  bestC <- tuned$best.parameters[[2]]
  best_model <- svm(Output ~ .,kernal="radial",cross=10,data = mydata,cost = bestC, gamma = bestGamma)
  summary(best_model)
  
  #Using the model to predict on tesset
  prediction <- predict(best_model, testset[,-ncol(mydata)])
  tab <- table(pred = prediction, true = testset[,ncol(mydata)])
  tab
  
  classAgreement(tab)
  
  #Creating SVM model from trainset
  model <- svm(output~.,data=trainset)
  print(model)
  summary(model)
  
  #Logistic Regression
  trainset <- mydata[1:43,]
  testset <- mydata[44:53,]
  
  glm_model <- glm(output ~.,family = binomial,data=trainset)
  summary(glm_model)
  anova(glm_model, test="Chisq")
  
  fitted.results <- predict(glm_model,newdata=testset,type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  
  misClasificError <- mean(fitted.results != testset$output)
  print(paste('Accuracy',1-misClasificError))
  
  #Random Forest
  
  bestmtry <- tuneRF(trainset[-ncol(mydata)],trainset$output, ntreeTry=100, 
                     stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
  
fit <-randomForest(output~.,data=trainset, mtry=4, ntree=1000, 
                          keep.forest=TRUE, importance=TRUE)
  fit <- randomForest(Output~.,data=mydata,importance=TRUE,proximity=TRUE,mtry=2,do.trace=100)
  varImpPlot(fit)
  round(importance(fit), 2)
  Prediction <- predict(fit, testset)
  #Using the model to predict on tesset
  prediction <- predict(fit, testset[,-ncol(mydata)])
  tab <- table(pred = prediction, true = testset[,ncol(mydata)])
  tab
  
  
  X <- trainset[,-ncol(mydata)]
  Y <- trainset[,ncol(mydata)]
  
  model<-train(as.data.frame(X),Y,method='lda')
  
  featurePlot(x = mydata[, 3:6],
              y = mydata$output,
              plot = "pairs",
              ## Add a key at the top
              auto.key = list(columns = 3))
  
  featurePlot(x = mydata[, 3:6],
              y = mydata$output,
              plot = "ellipse",
              ## Add a key at the top
              auto.key = list(columns = 3))
  
  transparentTheme(trans = .9)
  featurePlot(x = mydata[, 1:6],
              y = mydata$output,
              plot = "density",
              ## Pass in options to xyplot() to 
              ## make it prettier
              scales = list(x = list(relation="free"),
                            y = list(relation="free")),
              adjust = 1.5,
              pch = "|",
              layout = c(6, 1),
              auto.key = list(columns = 3))
  
  
  featurePlot(x = mydata[, 3:6],
              y = mydata$output,
              plot = "box",
              ## Pass in options to bwplot() 
              scales = list(y = list(relation="free"),
                            x = list(rot = 90)),
              layout = c(4,1 ),
              auto.key = list(columns = 2))
  
  gbmImp <- varImp(mydata, scale = FALSE)
  gbmImp
  
  final_data<-read.table("revised_data.csv",header=TRUE,sep=",",fill =TRUE,quote="",comment.char = "",allowEscapes=TRUE,stringsAsFactors=TRUE)
  
  library(psych)
  require(ggplot2)
  describeBy(final_data,final_data$Output)
  t.test(final_data$Difficult_HighEngagement~final_data$Output)
  final_data$Output = as.factor(final_data$Output)
  final_data$Gender_1_Male = as.factor(final_data$Gender_1_Male)
  ggplot(final_data, aes(x=Output,y=Difficult_HighEngagement))+
    geom_boxplot()
  model1 = lm(Difficult_HighEngagement ~ Output, data = final_data)
  summary(model1)
  
  summary(final_data)
  sapply(final_data, sd)
  
  ##Normality test
  shapiro.test(final_data$Difficult_HighEngagement)
  qqnorm(final_data$Difficult_HighEngagement)
  
  xtabs(~Output+Gender_1_Male,data=final_data)
  mylogit <- glm(Output ~ ., data = final_data[-1], family = "binomial")
  summary(mylogit)
 pairs(final_data[,3:4])
 
 mydata=final_data[-1]
 mydata$Gender_1_Male = as.factor(mydata$Gender_1_Male)
 #Creating Train and Test set
 index <- 1:nrow(mydata)
 set.seed(52)
 testindex <- sample(index, trunc(length(index)/3))
 testset <- mydata[testindex,]
 trainset <- mydata[-testindex,]
 xtabs(~Output+Gender_1_Male,data=trainset)
 
 #Random Forest
 
 
 bestmtry <- tuneRF(trainset[-ncol(mydata)],trainset$Output, ntreeTry=100, 
                    stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
 
 
 fit <-randomForest(Output~.,data=trainset, mtry=2, ntree=1000, 
                    keep.forest=TRUE, importance=TRUE)
 fit <- randomForest(output~.,data=mydata,importance=TRUE,proximity=TRUE,mtry=2,do.trace=100)
 varImpPlot(fit)
 round(importance(fit), 2)
 Prediction <- predict(fit, testset)
 #Using the model to predict on tesset
 prediction <- predict(fit, testset[,-ncol(mydata)])
 tab <- table(pred = prediction, true = testset[,ncol(mydata)])
 tab
 
  