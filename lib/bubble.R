#bubble plot#
library(plotly)
pop2<-readRDS(file.choose())
data_c<-as.data.frame(pop2[,c(3,10,11,14)])
data_c<-na.omit(data_c)
median_income<-vector()
state<-read.csv("C:/Users/Owner/Desktop/ADS/Project1/statenames.csv",header=T)
stnums<-as.vector(state[,1])
for(i in stnums)
{ median_income<-c(median_income,median(data_c$PINCP[data_c$ST==i]))}
median_income<-median_income[-52]
stnums<-stnums[-52]
median_arrival<-vector()
for(i in 1:51)
{ median_arrival<-c(median_arrival,median(data_c$JWAP[data_c$ST==stnums[i]]))}
stnames<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN"
           ,"IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH"
           ,"NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT"
           ,"VT","VA","WA","WV","WI","WY","PR")
ST<-stnames[1:length(stnums)]
MedianArrivalTime<-median_arrival
MedianIncome<-median_income
data_c1<-data.frame(ST,MedianIncome,MedianArrivalTime)
plot_ly(data_c1,x=MedianArrivalTime,y=MedianIncome,text=paste("State: ", ST),
        mode="markers",color=MedianIncome,size=MedianIncome)
layout(title="Median Income v.s. Median Arrival Time")
