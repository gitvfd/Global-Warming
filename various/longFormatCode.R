library(reshape2)
library(ggplot2)
library(dplyr)

library(ggthemes)
#import data
data <- read.csv("/Users/vfduclos/Dropbox/viz/data7.csv",sep=";")
summary(data)

write.table(data, file = "/Users/vfduclos/Dropbox/viz/dataWideD3.tsv", sep = "\t", row.names = FALSE,quote=TRUE)

data4Mean <- data
data4Mean[is.na(data4Mean)] <- 0
meanDeviation <- data4Mean %>% summarise_each(funs(mean))

#format data long format with date as mmm-YYYY
dataLong<-melt(data,id.vars = c("Year"))
dataLong$date<-paste(dataLong$variable,dataLong$Year,sep="-")
write.csv(dataLong,"/Users/vfduclos/Dropbox/viz/dataLong3.csv")

summary(dataLong)
#plot all years no style
TempChart <- ggplot(data=dataLong, aes(x=variable, y=value, group=Year)) +  geom_line(color="gray60") + theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
TempChart

TempChart + geom_smooth(method = lm, formula= y ~ poly(x,3),se = FALSE)

#plot all years colored by year
yearColored <- ggplot(data=dataLong, aes(x=variable, y=value, group=Year)) +  geom_line(aes(color=Year))+ theme_tufte(base_size=17, ticks=F)  + theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12)) 
yearColored

#Obtain for each month the year with the min and max temperature
dataMin <-   dataLong %>% group_by(variable) %>% slice(which.min(value))
dataMax <-   dataLong %>% group_by(variable) %>% slice(which.max(value))


#Obtain for each month the year with the avg temperature
dataMean<-dataLong
dataMean$value[is.na(dataMean$value)] <- "0"
str(dataMean)
dataMean2 <-   dataMean %>% group_by(variable) %>% summarise(mean=mean(as.double(value)))
dataMean2$Year <- ""

#Obtain the avg temp for each year
dataYear<-dataLong
  #calculate the average for the 2 first month of 2016
v<-dataYear[which(dataYear$date=="Jan-2016"),] 
w<-dataYear[which(dataYear$date=="Feb-2016"),] 
x<-dataYear[which(dataYear$date=="Mar-2016"),] 
y<-dataYear[which(dataYear$date=="Apr-2016"),] 
z<-dataYear[which(dataYear$date=="May-2016"),] 
z0<-dataYear[which(dataYear$date=="Jun-2016"),] 
z1<-dataYear[which(dataYear$date=="Jul-2016"),] 
z2<-dataYear[which(dataYear$date=="Aug-2016"),] 
dataYear[is.na(dataYear)] <- (v$value+w$value+x$value+y$value+z$value+z0$value+z1$value+z2$value)/8
dataYear2<-dataYear %>% group_by(Year) %>% summarise(mean=mean(value))

#Plot the Min point on the chart
Min <- ggplot() +  geom_line(aes(x=dataLong$variable, y=dataLong$value, group=dataLong$Year),color="gray60")+geom_point( aes(x=dataMin$variable, y=dataMin$value)) +geom_point( aes(x=dataMin$variable, y=dataMin$value),color="navyblue", size = 3) +geom_text(aes(x=dataMin$variable,y=dataMin$value-5,label=dataMin$Year),color="navyblue") +  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
Min

#Plot the Max point on the chart
Max <- ggplot() +  geom_line(aes(x=dataLong$variable, y=dataLong$value, group=dataLong$Year),color="gray60")+geom_point( aes(x=dataMax$variable, y=dataMax$value)) +geom_point( aes(x=dataMax$variable, y=dataMax$value),color="red", size = 3) +geom_text(aes(x=dataMax$variable,y=dataMax$value+5,label=dataMax$Year),color="red")+  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
Max


#Plot a given year
dataColor <- dataLong
dataColor$color[dataColor$Year=="1881"]<-"red"
dataColor$color[dataColor$Year!="1881"]<-"gray60"
yearSelect <- ggplot(data=dataColor, aes(x=variable, y=value, group=Year)) +  geom_line(aes(color=dataColor$color))+  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
yearSelect


#Plot the avg line on the chart
Avg <- ggplot() +  geom_line(aes(x=dataLong$variable, y=dataLong$value, group=dataLong$Year),color="gray60")+geom_line(aes(x=dataMean2$variable, y=dataMean2$mean, group=dataMean2$Year),color="black",size=2,linetype=3)+ theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
Avg


#Plot the avg year temp chart
avgYearTemp <- ggplot() +  geom_line(aes(x=dataYear2$Year, y=dataYear2$mean),color="gray60",size=1)+  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
avgYearTemp


#Plot the avg year temp chart with polynomial regression
avgYearTemp <- ggplot() +  geom_smooth(aes(x=dataYear2$Year, y=dataYear2$mean),color="gray20",size=0.5,linetype=2, method=lm, formula = y ~ poly(x,2), level=0.95) +  geom_line(aes(x=dataYear2$Year, y=dataYear2$mean),color="gray60",size=1) +  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
avgYearTemp

dataMonthly<-dataYear
dataMonthly <- mutate_each(dataMonthly, funs(tolower))
dataMonthly$date <-paste("01-",dataMonthly$date,sep="")
dataMonthly$datef<- as.Date(dataMonthly$date, format="%d-%B-%Y")
dataMonthly<-arrange(dataMonthly,datef)

#Plot monthly avg  temp chart with polynomial regression
monthlyTemp <- ggplot() +  geom_line(aes(x=dataMonthly$datef, y=as.double(dataMonthly$value)),color="gray60",size=0.5)+  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
monthlyTemp

monthlyTempPolar <- ggplot() +  geom_line(aes(x=dataMonthly$datef, y=as.double(dataMonthly$value),color=dataMonthly$Year),size=0.5)+  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.8,0.95),legend.direction="horizontal",axis.text=element_text(size=12)) + coord_polar(theta = "x")
monthlyTempPolar


dataMonthly$test <- as.Date(months(dataMonthly$datef),format="%B")
dataMonthly$test <-format(as.yearmon(dataMonthly$datef),format="%B")
monthlyTempPolarbis <- ggplot()+ geom_line(aes(x=format(as.yearmon(dataMonthly$datef),format="%m"), y=as.double(dataMonthly$value)),size=0.5)+  theme(panel.background=element_blank(),legend.background=element_blank(),axis.text=element_text(size=12)) + coord_polar(theta = "x")
monthlyTempPolarbis


#Plot the monthly temp chart with polynomial regression
monthlyTempModel <- ggplot() +  geom_smooth(aes(x=dataMonthly$datef, y=as.double(dataMonthly$value)),color="gray20",size=0.5,linetype=2, method=lm, formula = y ~ poly(x,2), level=0.95) +  geom_line(aes(x=dataMonthly$datef, y=as.double(dataMonthly$value)),color="gray60",size=0.25) +  theme(panel.background=element_blank(),legend.background=element_blank(),legend.position=c(0.5,0.95),legend.direction="horizontal",axis.text=element_text(size=12))
monthlyTempModel


# heatmap
base_size <- 9
p <- ggplot(dataYear, aes(variable, Year)) + geom_tile(aes(fill = value/100),  colour = "white") + scale_fill_gradient(low = "#8CB7E5",high = "#59374E")+ theme_grey(base_size = base_size)+ labs(x = "", y = "") + theme(legend.position = "none",axis.ticks = element_blank(), axis.text.x = element_text(size = base_size *0.8, angle = 0, hjust = 0, colour = "grey50"), axis.text.y = element_text(size = base_size *0.8, angle = 0, hjust = 0, colour = "grey50"),panel.background=element_blank()) #+ scale_x_discrete(expand = c(0, 0))+scale_y_discrete(expand = c(0, 0))


dataYear <- dataYear %>% filter( date != "Sep-2016"& date != "Oct-2016"& date != "Nov-2016"& date != "Dec-2016")
dataYear<-arrange(dataYear,-value)
write.table(dataYear, file = "/Users/vfduclos/Dropbox/viz/dataD3.csv", sep = ",", row.names = TRUE,quote=TRUE)


dataYearWide <-dataYear[c("Year","variable","value")] 
dataYearWide <-dcast(dataYearWide,variable~Year,mean)
write.table(dataYearWide, file = "/Users/vfduclos/Dropbox/viz/dataWideD3.csv", sep = ",", row.names = FALSE,quote=TRUE)
dataYearWide <-dataYear[c("Year","variable","value")] 
dataYearWide <-dcast(dataYearWide,Year~variable,mean)
write.table(dataYearWide, file = "/Users/vfduclos/Dropbox/viz/dataWideD3.tsv", sep = "\t", row.names = FALSE,quote=TRUE)
