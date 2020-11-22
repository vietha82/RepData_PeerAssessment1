library(data.table)
library(ggplot2)
library(dplyr)

unzip('activity.zip')
activity<-data.table(read.csv(file='activity.csv', header = T))
activity[, date:=as.Date(date, format='%Y-%m-%d')]
TotalNumberOfSteps<-activity[, list(total_steps=sum(steps, na.rm = T)), by='date']

hist(TotalNumberOfSteps$total_steps, 
     main="Step Breakdown by Day" ,
     xlab='Total Number of Steps',
     col="green",
     breaks = 20)

meanSteps<-mean(TotalNumberOfSteps$total_steps, na.rm = T)
medianSteps<-median(TotalNumberOfSteps$total_steps, na.rm = T)
#time series plot\
average_steps<-activity[!is.na(steps),list(steps=mean(steps)), by='interval']
average_steps[ ,steps:=as.numeric(steps)]
p1<-ggplot(data=average_steps, aes(x=interval, y=steps)) + 
        geom_line(color='green')+
        theme_bw()+
        ggtitle('Mean Steps by Interval')
max_step<-max(average_steps$steps)
max_interval<-average_steps[steps==max_step,]
max_interval<-max_interval$interval

#missing data
empty_row<-nrow(activity[is.na(steps),])
meanInterval<-mean(activity$steps, na.rm=T)
data_fill<-activity[, steps:=ifelse(is.na(steps), meanInterval, steps)]
updateNumberSteps<-data_fill[, list(total_steps=sum(steps, na.rm = T)), by='date']
updateMeanStep<-mean(updateNumberSteps$total_steps)
updatemediaStep<-median(updateNumberSteps$total_steps)

hist(updateNumberSteps$total_steps, 
     main="Step Breakdown by Day" ,
     xlab='Total Number of Steps',
     col="green",
     breaks = 20)




