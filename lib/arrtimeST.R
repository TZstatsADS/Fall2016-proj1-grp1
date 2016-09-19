#Bar plot 2#
#JWAP v.s ST#
library(ggplot2)
library(reshape2)
library(plotly)
load("C:/Users/Owner/Desktop/ADS/Project1/pop update.RData")
data_a<-as.data.frame(pop[,c(3,8,9,10)])
state<-read.csv("C:/Users/Owner/Desktop/ADS/Project1/statenames.csv",header=T)
stnums<-as.vector(state[,1])
stnames<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN"
           ,"IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH"
           ,"NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT"
           ,"VT","VA","WA","WV","WI","WY","PR")
stnames<-as.vector(stnames)
for(i in 1:52)
{data_a$ST[data_a$ST==stnums[i]]<-stnames[i]}
JWAPnames<-c("0-4","4-8","8-12","12-16","16-20","20-24")
for(i in 1:45)
{data_a$JWAP[data_a$JWAP==i]<-JWAPnames[1]}
for(i in 46:93)
{data_a$JWAP[data_a$JWAP==i]<-JWAPnames[2]}
for(i in 94:141)
{data_a$JWAP[data_a$JWAP==i]<-JWAPnames[3]}
for(i in 142:189)
{data_a$JWAP[data_a$JWAP==i]<-JWAPnames[4]}
for(i in 190:237)
{data_a$JWAP[data_a$JWAP==i]<-JWAPnames[5]}
for(i in 238:285)
{data_a$JWAP[data_a$JWAP==i]<-JWAPnames[6]}
data_JWAP<-matrix(0,ncol=7,nrow=51)
for(i in 1:51)
 {for(j in 2:7)
  {data_JWAP[i,j]<-sum((data_a$ST==stnames[i])&(data_a$JWAP==JWAPnames[j-1]))}}
colnames(data_JWAP)<-c("State","0-4","4-8","8-12","12-16","16-20","20-24")
data_JWAP<-as.data.frame(data_JWAP)
data_JWAP1<-melt(data_JWAP,id.vars="State")
data_JWAP[,1]<-stnames[1:51]
data_JWAP1<-melt(data_JWAP,id.vars="State")
colnames(data_JWAP1)[2]<-"ArrivalTime"
colnames(data_JWAP1)[3]<-"CumulativeProportion"
g<-ggplot(data=data_JWAP1,aes(x=State,y=CumulativeProportion,fill=ArrivalTime))
g+geom_bar(position="fill",stat="identity")+ggtitle("Arrival Time v.s. State")
ggplotly()
