library(ggplot2)

getwd()
setwd("C:/Users/apradha7/Desktop/ONR2")

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
pupil_data <- ONR_raw[c("StimulusName","EventSource","PupilLeft","PupilRight")]

pupil_data1<-pupil_data[which(pupil_data$EventSource=="ET"),]

ggplot(pupil_data1, aes(y=pupil_data1$PupilLeft,col=pupil_data1$StimulusName)) +geom_line()
