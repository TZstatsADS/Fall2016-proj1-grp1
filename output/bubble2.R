#bubble plot#
#Occupation bubble#
library(plotly)
pop2<-readRDS("C:/Users/Owner/Desktop/ADS/Project1/dat_pus.rds")
data_c<-as.data.frame(pop2[,c(3,10,11,14)])
data_c<-na.omit(data_c)
OCP<-c("MGR","BUS","FIN","CMM","ENG","SCI","CMS","LGL","EDU","ENT","MED","HLS","PRT"
       ,"EAT","CLN","PRS","SAL","OFF","FFF","CON","EXT","RPR","PRD","TRN","MIL")
OCP<-as.vector(OCP)
MedianArrivalTime<-vector()
for(i in 1:25)
{ MedianArrivalTime<-c(MedianArrivalTime,median(data_c$JWAP[data_c$occupation==OCP[i]]))}
MedianArrivalTime<-MedianArrivalTime[-c(1:3)]
OCP<-OCP[-c(1:3)]
MedianIncome<-vector()
for(i in 1:length(OCP))
{ MedianIncome<-c(MedianIncome,median(data_c$PINCP[data_c$occupation==OCP[i]]))}
data_c2<-data.frame(OCP,MedianIncome,MedianArrivalTime)
plot_ly(data_c2,x=MedianArrivalTime,y=MedianIncome,text=paste("Occupation: ", OCP),
        mode="markers",color=MedianIncome,size=MedianIncome)
layout(title="Median Income v.s. Median Arrival Time")
