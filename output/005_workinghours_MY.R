
######occupation choise considering working hour

####install packages and load libraries
install.packages("plotly")
install.packages("directlabels")
library(plotly)
library(dplyr)
library(ggplot2)
library(xlsx)
library(directlabels)


######read and manage data
workinghourdata<-readRDS("/Users/monicatao/Documents/ads/project1/dat_pus.rds")
workinghour<-workinghourdata[,c(1,3,6,8,11,14,15,16)]
workinghour<-workinghour[!is.na(workinghour$WKHP),]
workinghour<-workinghour[!is.na(workinghour$occupation),]
workinghour<-workinghour[workinghour$PINCP>=0,]
workinghour$PINCP<-workinghour$PINCP/1000


#######boxplot of working hour of different occupation, arrange by descending of average working hour
avgworkinghour<-workinghour%>%group_by(occupation)%>%summarise(avgwkhp=mean(WKHP))%>%arrange(desc(avgwkhp))
workinghour$occupation<-factor(workinghour$occupation,levels=avgworkinghour$occupation)
workinghour<-workinghour%>%arrange(occupation)
box_plot_occupation <- plot_ly(workinghour, x = WKHP, color = occupation, type = "box")
box_plot_occupation


#####An overview of working hour, occupation and income
avgincome<-workinghour%>%group_by(WKHP,occupation)%>%summarise(avgincome=mean(PINCP))
line_plot_nogroup<-ggplot(data=avgincome, aes(x=WKHP, y=avgincome, group = occupation, colour = occupation)) +
  geom_line(size=1)+
  ylab("Income (in thousand)")+
  xlab("")+
  geom_point( size=1.2)+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text=element_text(size=8)
    ,axis.ticks=element_blank()
    ,legend.title = element_blank()
  )+
  theme(legend.key.height=unit(0.7,"line")) +
  theme(legend.position = "bottom") +
  guides(colour=guide_legend(nrow=2))+
  ggtitle("Working hours vs Income")

line_plot_nogroup

lingplotweb_nogroup<-plot_ly(avgincome, x = WKHP, y = avgincome, color = occupation)
lingplotweb_nogroup


########Careful analysis by deviding working hour into 10 groups
workinghourgroup<-workinghour
workinghourgroup$workhour<-with(workinghourgroup,ifelse(WKHP>=1&WKHP<=9,"0-9",
                                                        ifelse(WKHP>=10&WKHP<=19,"10-19",
                                                               ifelse(WKHP>=20&WKHP<=29,"20-29",
                                                                      ifelse(WKHP>=30&WKHP<=39,"30-39",
                                                                             ifelse(WKHP>=40&WKHP<=49,"40-49",
                                                                                    ifelse(WKHP>=50&WKHP<=59,"50-59",
                                                                                           ifelse(WKHP>=60&WKHP<=69,"60-69",
                                                                                                  ifelse(WKHP>=70&WKHP<=79,"70-79",
                                                                                                         ifelse(WKHP>=80&WKHP<=89,"80-89","90-99"))))))))))

workinghourgroup<-workinghourgroup%>%arrange(workhour)
workinghourgroup$workhour<-factor(workinghourgroup$workhour,levels=unique(workinghourgroup$workhour))

box_plot_income <- plot_ly(workinghourgroup, x = Income, color = workhour, type = "box")
box_plot_income



averageincome<-workinghourgroup%>%group_by(workhour,occupation)%>%summarise(avgincome=mean(PINCP))
line_plot_group<-ggplot(data=averageincome, aes(x=workhour, y=avgincome, group = occupation, colour = occupation)) +
  geom_line(size=1)+
  ylab("Income(in thousand)")+
  xlab("")+
  geom_point( size=1.2)+
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.text=element_text(size=8)
    ,axis.ticks=element_blank()
    ,legend.position="none"
    ,legend.title = element_blank()
  )+
  guides(colour=guide_legend(ncol=1))+
  ggtitle("Working hours vs Income")+
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = occupation), method = list(dl.combine("first.points","last.points"), cex = 0.8))

line_plot_group

lineplotweb_group<-plot_ly(averageincome, x = workhour, y = avgincome, color = occupation)



######Analysis of working hours by States
avgworkinghourbystate<-workinghour%>%group_by(ST.code)%>%summarise(avgwkhp=mean(WKHP))%>%arrange(desc(avgwkhp))
l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
WKHP_map<-plot_ly(avgworkinghourbystate, z = avgwkhp, locations = ST.code, type = 'choropleth',
        locationmode = 'USA-states', color = avgwkhp, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "hours"))
layout(title = 'Average working hours in states', geo = g)
WKHP_map






