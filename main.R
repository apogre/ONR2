library(ggplot2)
library(grid)
library(gridExtra)



getwd()
setwd("C:/Users/apradha7/Desktop/ONR2")

file_list = list.files("./raw-data/ONR@/ONR_S2_V4")
for (f in file_list){
  print(f)
  myfile = paste("./raw-data/ONR@/ONR_S2_V4/",f,sep="")
  data = read.csv(file=myfile, header=TRUE, fill=TRUE, sep = "\t", skip=5)
  print("data read")
  # filter_by_eventSource(myfile)
  f_data <- data[grep("ABMBrainState", data$EventSource),]
  print("data filtered by eventSource")
  ex_data <- f_data[,c("HighEngagement","LowEngagement","Distraction","Drowsy","WorkloadFBDS","WorkloadBDS","WorkloadAverage")]
  print("Extracting Constructs")
  el_data <- ex_data[(ex_data$WorkloadBDS != -99999) | (ex_data$WorkloadFBDS != -99999) | (ex_data$WorkloadAverage != -99999),]
  print("REmoving negative values")
  newfile = paste("ONR_S2_V5_EX/",f,sep="")
  write.table(el_data, file=newfile,row.names = FALSE,sep = "," ,col.names=TRUE, quote= FALSE)
  print("data written in csv")
}

ONR_raw_74_1 = read.table("./raw-data/ONR@/ONR_S2_V4/Dump014_N074.txt", sep="\t", header=TRUE, skip=6, fill=TRUE)
ONR_raw_61_0 = read.table("./raw-data/ONR@/ONR_S2_V4/Dump005_N061.txt", sep="\t", header=TRUE, skip=6, fill=TRUE)


ONR_raw$StudyName <- NULL
ONR_raw$ExportDate<-NULL
ONR_raw$Name<-NULL
ONR_raw$Timestamp<-NULL

ONR_raw$SlideType<-NULL #The type of slide(BlackInterslide=Interslide,Random=Interslide,TestImage=Stimulus)
ONR_raw$MediaTime<-NULL #If video, there is a separate timesignal from the media
ONR_raw[7:10]<-list(NULL) #Gaze Co-ordinates

#Pupil size Analysis
pupil_data <- ONR_raw_74_1[c("StimulusName","EventSource","UTCTimestamp","PupilLeft","PupilRight")]
pupil_data_0 <- ONR_raw_61_0[c("StimulusName","EventSource","UTCTimestamp","PupilLeft","PupilRight")]


pupil_data1<-pupil_data[grep("ET", pupil_data$EventSource),]
pupil_data1_0<-pupil_data_0[grep("ET", pupil_data$EventSource),]

#removing the duplicate
pupil_data2<-pupil_data1[!duplicated(pupil_data1[,3]),]
pupil_data2_0<-pupil_data1_0[!duplicated(pupil_data1_0[,3]),]

pupil_data3<-subset(pupil_data2,pupil_data2$PupilLeft!=-1.000 & pupil_data2$PupilRight!=-1.000)
pupil_data3_0<-subset(pupil_data2_0,pupil_data2_0$PupilLeft!=-1.000 & pupil_data2_0$PupilRight!=-1.000)

pupil_data3$UTCTimestamp<-sapply(strsplit(as.character(pupil_data3$UTCTimestamp), split = "_"), "[", 2)
pupil_data3_0$UTCTimestamp<-sapply(strsplit(as.character(pupil_data3_0$UTCTimestamp), split = "_"), "[", 2)

pupil_data3$UTCTimestamp <- as.POSIXct(as.character(pupil_data3$UTCTimestamp), origin="1970-01-01", tz="GMT")
pupil_data3_0$UTCTimestamp <- as.POSIXct(as.character(pupil_data3_0$UTCTimestamp), origin="1970-01-01", tz="GMT")

pupil_data4<-subset(pupil_data3,pupil_data3$StimulusName=="Difficult")
pupil_data4_0<-subset(pupil_data3_0,pupil_data3_0$StimulusName=="Difficult")

p<-ggplot(pupil_data3_0, aes(UTCTimestamp,PupilLeft,color=StimulusName))
p<-p+geom_point()
p + stat_summary(fun.y = mean, geom="line")
p

avg<-aggregate(pupil_data3[,4:5],list(StimulusName=pupil_data3$StimulusName),mean)
avg$mean=rowMeans(avg[,c("PupilLeft", "PupilRight")], na.rm=TRUE)
avg
avg1<-aggregate(pupil_data3_0[,4:5],list(StimulusName=pupil_data3_0$StimulusName),mean)
avg1$mean=rowMeans(avg1[,c("PupilLeft", "PupilRight")], na.rm=TRUE)
avg1

pupil_data3$StimulusName<-factor(pupil_data3$StimulusName,c("Easy1","Easy2","Moderate","Difficult","Posttest","WMC"))
pupil_data3_0$StimulusName<-factor(pupil_data3_0$StimulusName,c("Easy1","Easy2","Moderate","Difficult","Posttest","WMC"))

boxplot(data=pupil_data3,PupilLeft~StimulusName)
boxplot(data=pupil_data3_0,PupilLeft~StimulusName)


b<-ggplot(data=pupil_data3, aes(x=StimulusName,y=PupilLeft))+geom_boxplot(aes(fill=pupil_data3$StimulusName))+ theme(legend.position="none")
c<-ggplot(data=pupil_data3_0, aes(x=StimulusName,y=PupilLeft))+geom_boxplot(aes(fill=pupil_data3_0$StimulusName))+ theme(legend.position="none")

grid.arrange(b,c,ncol=2)
