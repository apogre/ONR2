library(reshape2)
library(plyr)

getwd()
setwd("C:/Users/apradha7/Desktop/ONR2/raw-data/ONR@")

file_list = list.files("./ONR_S2")
count=0


for (f in file_list){
  print(f)
  myfile = paste("ONR_S2/",f,sep="")
  data = read.csv(file=myfile, header=TRUE, fill=TRUE, sep = "\t", skip=5)
  print("data read")
  # filter_by_eventSource(myfile)
  
#   f_data <- data[grep("ET", data$EventSource),]
#   print("data filtered by eventSource")
  
  data<-subset(data,data$StimulusName=="Difficult")
  
  data <- data[,c("EventSource","UTCTimestamp","HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadFBDS","WorkloadBDS","WorkloadAverage")]
  
  data<-data[!duplicated(data[,2]),]
  
  ex_data <- f_data[,c("StimulusName","EventSource","UTCTimestamp","PupilLeft","PupilRight")]
  
  print("Extracting Constructs")
  el_data <- ex_data[(ex_data$PupilLeft != -1.000) & (ex_data$PupilRight != -1.000),]
  el_data$StimulusName<-factor(el_data$StimulusName,c("Easy1","Easy2","Moderate","Difficult","Posttest","WMC"))
  print("REmoving negative values")
  
  avg<-aggregate(el_data[,4:5],list(StimulusName=el_data$StimulusName),mean)
  avg$mean=rowMeans(avg[,c("PupilLeft", "PupilRight")], na.rm=TRUE)
  avg[2:3]=list(NULL)
  x<-melt(avg)
  id<-sapply(strsplit(as.character(f), split = "[_.]"), "[", 2)
  
  if(count==0){
    existingDF<-dcast(x,id~StimulusName+variable)
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
write.table(existingDF, file=newfile,row.names = FALSE,sep = "," ,col.names=TRUE, quote= FALSE)
print("data written in csv")
