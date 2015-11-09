install.packages('e1071', dependencies = FALSE)
install.packages('caret', dependencies = TRUE)

library(reshape2)
library(plyr)
library(e1071)
library(ggplot2)
library(caret)

getwd()
setwd("C:/Users/apradha7/Downloads/Lab/ONR2/raw-data")



file_list = list.files("./ONR_S2") #Get all the files from the folder
count=0

for (f in file_list){
  print(f)
  
  myfile = paste("ONR_S2/",f,sep="")
  data = read.csv(file=myfile, header=TRUE, fill=TRUE,skip = 6, sep = "\t")
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
  
  avg<-aggregate(ex_data[,6:13],list(StimulusName=ex_data$StimulusName),mean,na.rm=TRUE)
  avg$pupil=rowMeans(avg[,c("PupilLeft", "PupilRight")], na.rm=TRUE)
  avg[2:3]=list(NULL)
  avg <- avg[c("StimulusName","pupil","FixationDuration","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadAverage")]
  
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
  final_data<-read.table("newfile.txt",header=TRUE,sep=",",fill =TRUE,quote="",comment.char = "",allowEscapes=TRUE,stringsAsFactors=TRUE)
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
  
  # mydata[29:42] <- list(NULL) 
  
#   model <- svm(output~.,data=mydata)
#   print(model)
#   summary(model)
#   
  
  #Creating Train and Test set
  index <- 1:nrow(mydata)
  set.seed(42)
  testindex <- sample(index, trunc(length(index)/3))
  testset <- mydata[testindex,]
  trainset <- mydata[-testindex,]
  
  tuned <- tune.svm(output~., data = trainset, gamma = 10^(-6:-1), cost = 10^(1:2))
  summary(tuned)
  
  bestGamma <- tuned$best.parameters[[1]]
  bestC <- tuned$best.parameters[[2]]
  best_model <- svm(output ~ .,kernal="radial", cross=10,data = trainset,cost = bestC, gamma = bestGamma)
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
  
  
  
  
  