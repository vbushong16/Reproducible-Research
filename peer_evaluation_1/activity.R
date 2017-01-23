#########
#Peer review
#########

library(ggplot2)

### SETTING THE WORK DIRECTORY
wkdir = "~\\"
subdir = "Github\\Reproducible-Research"

# create directory if it doesn't exist yet
dir.create(file.path(wkdir, subdir))
setwd(file.path(wkdir, subdir))
getwd()
#zip file download to director
# url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(url, destfile = "peer_review_data.zip")
# 
# #unzip file
# unzip("peer_review_data.zip")

activity = read.csv("activity.csv")
activity$date = as.Date(activity$date)

#q1
head(activity)
#1
steps_tot = with(activity,aggregate(steps, by =list(date),sum, na.rm = TRUE))
colnames(steps_tot) = c("Date","Total.Steps")
#2
hist(steps_tot$Total.Steps)
#3 
summary(steps_tot)

#q2
summary(activity)
#1
steps_t = with(activity,aggregate(steps, by =list(interval),mean,na.rm= TRUE))
plot(steps_t,type= "l",xlab = "5-minute interval", ylab = "Average Number of Steps", main = "Average Number of steps by 5-minute intervals")
 #2
steps_t[max(steps_t$x),]


#q3
steps_t = with(activity,aggregate(steps, by =list(interval),mean,na.rm= TRUE))
colnames(steps_t) = c("interval","steps")
activity_NA = activity
summary(activity)


for(i in 1:length(activity_NA[,1])){
  if(is.na(activity_NA[i,"steps"])){
    activity_NA[i,"steps"] = steps_t[steps_t$interval == activity_NA[i,"interval"],"steps"]
  }
}

steps_tot_NA_imputed = with(activity_NA,aggregate(steps, by =list(date),sum))
colnames(steps_tot_NA_imputed) = c("Date","Total.Steps")
hist(steps_tot_NA_imputed$Total.Steps)

stepsperday = merge(steps_tot_NA_imputed,steps_tot, by = "Date")
check<-ggplot(stepsperday,aes(x=Total.Steps.y))+
  geom_density()+
  geom_density(aes(x=Total.Steps.x,color="Imputed"))+
  ggtitle("Density plot comparing raw and NA-imputed activity datasets")+
  xlab("total steps")
print(check)




activity_NA[,"week_day"] = weekdays(activity_NA$date)
head(activity_NA)
for(i in 1:length(activity_NA[,1])){
  if(activity_NA[i,"week_day"] == "Saturday" || activity_NA[i,"week_day"] == "Sunday"){
    activity_NA[i,"week_period"] = "weekend"
  }else{
    activity_NA[i,"week_period"] = "weekday"
  }
}

activity_NA$week_period = as.factor(activity_NA$week_period)

head(activity_NA)

library(dplyr)

steps_interval_weekday = with(activity_NA,aggregate(steps,by = list(interval,weekday),mean))
head(steps_interval_weekday)
colnames(steps_interval_weekday) = c("interval","weekday","steps")


ggplot(steps_interval_weekday,aes(x=interval,y=steps))+
  facet_wrap(~weekday,nrow=1,ncol=2)+
  geom_line()+
  ggtitle("Mean steps by 5min interval and weekday/weekend")+
  ylab("steps")+
  xlab("Interval")
