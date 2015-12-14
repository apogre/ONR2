require(ggplot2)
getwd()
setwd("C:/Users/apradha7/Downloads/Lab/ONR2/onr_data")

file_list = list.files("./ONR_S2") #Get all the files from the folder
count=0

for (f in file_list){
  print(f)
  f="Dump012_N022.txt"
  
  myfile = paste("ONR_S2/",f,sep="")
  data = read.csv(file=myfile, header=TRUE, fill=TRUE,skip = 5, sep = "\t")
  print("data read")
  if(ncol(data)<104){
    print("columns less than required")
    next
  }
  
  ex_data <- data[,c("Age","Gender","StimulusName","EventSource","UTCTimestamp","PupilLeft","PupilRight","FixationDuration","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadAverage")]
  wa <- data[,c("StimulusName","UTCTimestamp","EventSource","WorkloadAverage")]
  wa$WorkloadAverage[wa$WorkloadAverage == -99999.00]<-NA
  
  wa<-wa[complete.cases(wa),]
  
  wa$EventSource = as.character(wa$EventSource)
  wa <- wa[wa$EventSource=="ABMRawEEG|ABMDeconEEG|ABMBrainState",]
  
  
  wa$UTCTimestamp= strptime(x = sub(pattern = '([0-9]{8})_([0-9]{2})([0-9]{2})([0-9]{2})([0-9]*)',
                                          replacement = '\\1 \\2:\\3:\\4.\\5',x =wa$UTCTimestamp),format = '%Y%m%d %H:%M:%OS')
  wa$WorkloadAverage=scale(wa$WorkloadAverage)
  p<-ggplot(wa,aes(UTCTimestamp,WorkloadAverage,color=StimulusName))+geom_line()
  p + geom_hline(yintercept=wa$WorkloadAverage[1])
  
  pup_data <- data[,c("StimulusName","UTCTimestamp","EventSource","PupilLeft","PupilRight")]
  pup_data <- pup_data[grep("ET", data$EventSource),]
  pup_data$PupilLeft[pup_data$PupilLeft == -1.000]<-NA
  pup_data$PupilRight[pup_data$PupilRight == -1.000]<-NA
  pup_data<-pup_data[complete.cases(pup_data),]
  pup_data$avg_pupil=rowMeans(pup_data[,c("PupilLeft", "PupilRight")])
  pup_data$UTCTimestamp = as.character(pup_data$UTCTimestamp)
  
  
  
  strptime(x = sub(pattern = '([0-9]{8})_([0-9]{2})([0-9]{2})([0-9]{2})([0-9]*)',
    replacement = '\\1 \\2:\\3:\\4.\\5',x ='20150819_221559460'),format = '%Y%m%d %H:%M:%OS')
  
  pup_data$UTCTimestamp= strptime(x = sub(pattern = '([0-9]{8})_([0-9]{2})([0-9]{2})([0-9]{2})([0-9]*)',
                   replacement = '\\1 \\2:\\3:\\4.\\5',x =pup_data$UTCTimestamp),format = '%Y%m%d %H:%M:%OS')
  
  p=ggplot(pup_data,aes(x=UTCTimestamp,y=avg_pupil, colour=StimulusName))+geom_line()
  p + geom_hline(yintercept=pup_data$avg_pupil[1])
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