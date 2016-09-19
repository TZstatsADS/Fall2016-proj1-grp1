#Arrival Time Part#

#Import package#
library(ggplot2)
library(reshape2)
library(plotly)
library(RColorBrewer)

#Import data#
pop1<-readRDS("C:/Users/Owner/Desktop/ADS/Project1/dat_pus.rds")
data_d<-as.data.frame(pop1[,c(3,10,11,14)])
data_d<-na.omit(data_d)

#Divide Arrival Time as 6-12 & Others#
JWAPnames<-c("6-7","7-8","8-9","9-10","10-11","11-12","Others")
for(i in 70:81)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[1]}
for(i in 82:93)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[2]}
for(i in 94:105)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[3]}
for(i in 106:117)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[4]}
for(i in 118:129)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[5]}
for(i in 130:141)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[6]}
for(i in 1:69)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[7]}
for(i in 142:285)
{data_d$JWAP[data_d$JWAP==i]<-JWAPnames[7]}

#Define Occupations#
OCP<-c("MGR","BUS","FIN","CMM","ENG","SCI","CMS","LGL","EDU","ENT","MED","HLS","PRT"
       ,"EAT","CLN","PRS","SAL","OFF","FFF","CON","EXT","RPR","PRD","TRN","MIL")
OCP<-as.vector(OCP)

#Define State names#
state<-read.csv("C:/Users/Owner/Desktop/ADS/Project1/statenames.csv",header=T)
stnums<-as.vector(state[,1])
stnames<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN"
           ,"IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH"
           ,"NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT"
           ,"VT","VA","WA","WV","WI","WY","PR")
stnames<-as.vector(stnames)
for(i in 1:52)
{data_d$ST[data_d$ST==stnums[i]]<-stnames[i]}

#Plot Arrival Time v.s. Occupation#
data_OCCP<-matrix(0,ncol=8,nrow=25)
for(i in 1:25)
{for(j in 2:8)
{data_OCCP[i,j]<-sum((data_d$occupation==OCP[i])&(data_d$JWAP==JWAPnames[j-1]))}}
data_OCCP<-data_OCCP[-c(1:3),]
colnames(data_OCCP)<-c("Occupation",JWAPnames)
data_OCCP<-as.data.frame(data_OCCP)
data_OCCP1<-melt(data_OCCP,id.vars="Occupation")
data_OCCP[,1]<-OCP[-c(1:3)]
data_OCCP1<-melt(data_OCCP,id.vars="Occupation")
colnames(data_OCCP1)[2]<-"ArrivalTime"
colnames(data_OCCP1)[3]<-"CumulativeProportion"
g<-ggplot(data=data_OCCP1,aes(x=Occupation,y=CumulativeProportion,fill=ArrivalTime))
g+geom_bar(position="fill",stat="identity")+ggtitle("Arrival Time v.s. Occupation")+scale_fill_manual(values=c(
"red","green","yellow","brown","orange","grey",'blue')) 
ggplotly()

#Plot Arrival Time v.s. State#
data_JWAP<-matrix(0,ncol=8,nrow=51)
for(i in 1:51)
{for(j in 2:8)
{data_JWAP[i,j]<-sum((data_d$ST==stnames[i])&(data_d$JWAP==JWAPnames[j-1]))}}
colnames(data_JWAP)<-c("State",JWAPnames)
data_JWAP<-as.data.frame(data_JWAP)
data_JWAP1<-melt(data_JWAP,id.vars="State")
data_JWAP[,1]<-stnames[1:51]
data_JWAP1<-melt(data_JWAP,id.vars="State")
colnames(data_JWAP1)[2]<-"ArrivalTime"
colnames(data_JWAP1)[3]<-"CumulativeProportion"
g<-ggplot(data=data_JWAP1,aes(x=State,y=CumulativeProportion,fill=ArrivalTime))
g+geom_bar(position="fill",stat="identity")+ggtitle("Arrival Time v.s. State")+scale_fill_manual(values=c(
"red","green","yellow","brown","orange","grey",'blue')) 
ggplotly()

#Reload data to make bubble plots#
pop2<-readRDS("C:/Users/Owner/Desktop/ADS/Project1/dat_pus.rds")
data_c<-as.data.frame(pop2[,c(3,10,11,14)])
data_c<-na.omit(data_c)

#Plot State's bubble plot#
median_income<-vector()
for(i in stnums)
{ median_income<-c(median_income,median(data_c$PINCP[data_c$ST==i]))}
median_income<-median_income[-52]
stnums1<-stnums[-52]
median_arrival<-vector()
for(i in 1:51)
{ median_arrival<-c(median_arrival,median(data_c$JWAP[data_c$ST==stnums1[i]]))}
ST<-stnames[1:length(stnums1)]
MedianArrivalTime<-median_arrival
MedianIncome<-median_income
data_c1<-data.frame(ST,MedianIncome,MedianArrivalTime)
plot_ly(data_c1,x=MedianArrivalTime,y=MedianIncome,text=paste("State: ", ST),
        mode="markers",color=MedianIncome,size=MedianIncome)
layout(title="Median Income v.s. Median Arrival Time")

#Plot Occupation's bubble plot#
MedianArrivalTime<-vector()
for(i in 1:25)
{MedianArrivalTime<-c(MedianArrivalTime,median(data_c$JWAP[data_c$occupation==OCP[i]]))}
MedianArrivalTime<-MedianArrivalTime[-c(1:3)]
MedianIncome<-vector()
for(i in 1:length(OCP))
{ MedianIncome<-c(MedianIncome,median(data_c$PINCP[data_c$occupation==OCP[i]]))}
MedianIncome<-MedianIncome[-c(1:3)]
OCP1<-OCP[-c(1:3)]
data_c2<-data.frame(OCP1,MedianIncome,MedianArrivalTime)
plot_ly(data_c2,x=MedianArrivalTime,y=MedianIncome,text=paste("Occupation: ", OCP1),
        mode="markers",color=MedianIncome,size=MedianIncome)
layout(title="Median Income v.s. Median Arrival Time")

#End of code#