library(reshape2)
library(plyr)

getwd()
setwd("C:/Users/apradha7/Desktop/ONR2/raw-data/ONR@")

file_list = list.files("./ONR_S2")
count=0


for (f in file_list){
  print(f)
  myfile = paste("ONR_S2/",f,sep="")
  data = read.csv(file=myfile, header=TRUE, fill=TRUE, sep = "\t")
  print("data read")
  # filter_by_eventSource(myfile)
  # f_data <- data[grep("ET", data$EventSource),]
  print("data filtered by eventSource")
  
  ex_data <- data[,c("Age","Gender","StimulusName","EventSource","UTCTimestamp","PupilLeft","PupilRight","FixationDuration","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadAverage")]
  
  print("Extracting Constructs")
  
  ex_data$PupilLeft[ex_data$PupilLeft == -1.000]<-NA
  ex_data$PupilRight[ex_data$PupilRight == -1.000]<-NA
  ex_data$WorkloadAverage[ex_data$WorkloadAverage == -99999.00]<-NA
  
  # el_data <- ex_data[(ex_data$PupilLeft != -1.000) & (ex_data$PupilRight != -1.000),]
  # (ex_data$WorkloadAverage!=-99999.00)
  
  ex_data$StimulusName<-factor(ex_data$StimulusName,c("Easy1","Easy2","Moderate","Difficult","Posttest","WMC"))
  print("REmoving negative values")
  
  avg<-aggregate(ex_data[,6:13],list(StimulusName=ex_data$StimulusName),mean,na.rm=TRUE)
  avg$pupil=rowMeans(avg[,c("PupilLeft", "PupilRight")], na.rm=TRUE)
  avg[2:3]=list(NULL)
  avg <- avg[c("StimulusName","pupil","FixationDuration","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadAverage")]
#   
#   avg$Age<-ex_data$Age[1]
#   avg$Gender<-ex_data$Gender[1]
  f<-"Dump001_N0.txt"
  x<-melt(avg)
  id<-sapply(strsplit(as.character(f), split = "[_.]"), "[", 2)

  if(count==0){
    existingDF<-dcast(x,id~StimulusName+variable)
    existingDF$Age<-ex_data$Age[1]
    existingDF$Gender<-ex_data$Gender[1]
  }
  else{
    newDF <- dcast(x,id~StimulusName+variable)
    DF <- rbind.fill(existingDF,newDF)
    existingDF=DF
  }
  count = count+1
  
  
#   newfile = paste("ONR_S2_EX/",f,sep="")
#   write.table(x, file=newfile,row.names = FALSE,sep = "," ,col.names=TRUE, quote= FALSE)
#   print("data written in csv")
}

newfile = paste("ONR_S2_EX/",f,sep="")
newfile<-"data"
write.table(existingDF, file=newfile,row.names = FALSE,sep = "," ,col.names=TRUE, quote= FALSE)
print("data written in csv")
