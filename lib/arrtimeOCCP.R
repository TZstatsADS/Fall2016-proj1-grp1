#Bar plot 1#
#JWAP v.s. OCCP#
library(ggplot2)
library(reshape2)
library(plotly)
pop1<-readRDS("C:/Users/Owner/Desktop/ADS/Project1/dat_pus.rds")
data_b<-as.data.frame(pop1[,c(10,14)])
data_b<-na.omit(data_b)
JWAPnames<-c("0-4","4-8","8-12","12-16","16-20","20-24")
for(i in 1:45)
{data_b$JWAP[data_b$JWAP==i]<-JWAPnames[1]}
for(i in 46:93)
{data_b$JWAP[data_b$JWAP==i]<-JWAPnames[2]}
for(i in 94:141)
{data_b$JWAP[data_b$JWAP==i]<-JWAPnames[3]}
for(i in 142:189)
{data_b$JWAP[data_b$JWAP==i]<-JWAPnames[4]}
for(i in 190:237)
{data_b$JWAP[data_b$JWAP==i]<-JWAPnames[5]}
for(i in 238:285)
{data_b$JWAP[data_b$JWAP==i]<-JWAPnames[6]}
OCP<-c("MGR","BUS","FIN","CMM","ENG","SCI","CMS","LGL","EDU","ENT","MED","HLS","PRT"
       ,"EAT","CLN","PRS","SAL","OFF","FFF","CON","EXT","RPR","PRD","TRN","MIL")
OCP<-as.vector(OCP)
data_OCCP<-matrix(0,ncol=7,nrow=25)
for(i in 1:25)
{for(j in 2:7)
{data_OCCP[i,j]<-sum((data_b$occupation==OCP[i])&(data_b$JWAP==JWAPnames[j-1]))}}
data_OCCP<-data_OCCP[-c(1:3),]
colnames(data_OCCP)<-c("Occupation","0-4","4-8","8-12","12-16","16-20","20-24")
data_OCCP<-as.data.frame(data_OCCP)
data_OCCP1<-melt(data_OCCP,id.vars="Occupation")
data_OCCP[,1]<-OCP[-c(1:3)]
data_OCCP1<-melt(data_OCCP,id.vars="Occupation")
colnames(data_OCCP1)[2]<-"ArrivalTime"
colnames(data_OCCP1)[3]<-"CumulativeProportion"
g<-ggplot(data=data_OCCP1,aes(x=Occupation,y=CumulativeProportion,fill=ArrivalTime))
g+geom_bar(position="fill",stat="identity")+ggtitle("Arrival Time v.s. Occupation")
ggplotly()
