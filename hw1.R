library(ggplot2)
library(reshape2)
setwd("C:\\Users\\apradha7\\Downloads\\Lab")
getwd()

#Data Pre-Processing
file_list = list.files("ONR_S2_V5")

for (f in file_list){
  print(f)
  myfile = paste("ONR_S2_V4/",f,sep="")
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

#Mean Value Calculation
i = 1
p_file_list <- list.files("ONR_S2_V5_EX")
for (pf in p_file_list){
  print(pf)
  mypfile = paste("ONR_S2_V5_EX/",pf,sep="")
  data = read.csv(file=mypfile, header=TRUE, fill=TRUE, sep = ",")
  pf = data
  if (i ==1){
    mylist = as.list(colMeans(data,na.rm = TRUE))
    i = 0
  }
  else{
    newlist = as.list(colMeans(data,na.rm=TRUE))
    templist = mylist
    mylist =mapply(c,templist,newlist,SIMPLIFY=FALSE)  
  }
}
an_data <- as.data.frame(mylist)

#Plotting 7 Constructs as Time Series
par(xpd=TRUE)
plot(head(pf$HighEngagement,n=100),type='l',col="RED",ylab="",main="Timeseries Plot of Constructs")
lines(head(pf$LowEngagement,n=100),type='l',col="BLUE")
lines(head(pf$Distraction ,n=100),type='l',col="GREEN")
lines(head(pf$Drowsy ,n=100),type='l',col="BLACK")
legend(-10,-0.2,angle=45,lty=c(1,1,1,1),lwd=c(2,2,2,2),
       c("HighEngagement","LowEngagement","Distraction","Drowsy"), col =c("red","blue","green","black"), horiz=TRUE) 

par(xpd=TRUE)
plot(head(pf$WorkloadBDS,n=100),type='l',main="Timeseries plot for Constructs",col="red",ylab="")
lines(head(pf$WorkloadFBDS,n=100),type='l',col="blue")
lines(head(pf$WorkloadAverage,n=100),type='l',col="green")
legend(0,0,lty=c(1,1,1),lwd=c(2,2,2),
       c("WorkloadBDS","workloadFBDS","workloadAverage"), col =c("red","blue","green"), horiz=TRUE) 

labels = colnames(sd[[1]])
#ANOVA 
set.seed(10)
sd<-split(an_data,sample(rep(1:2,4)))
group1 = melt(sd[[1]])
group2= melt(sd[[2]])

labels = colnames(sd[[1]])
par(xpd=FALSE)
boxplot(sd[[1]],col = "lightgray", xaxt = "n",  xlab = "", main="Box Plot for Group 2")
axis(1, labels = FALSE)
text(x =  seq_along(labels), y = par("usr")[3], srt = 45, adj = 1.1,
     labels = labels, xpd = TRUE)

oneway.test(value~variable,data=group1)
oneway.test(value~variable,data=group2)
