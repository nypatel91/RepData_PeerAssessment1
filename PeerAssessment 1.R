library(ggplot2)

activity <- read.csv("activity.csv")
stepsbyDay <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(stepsbyDay,xlab = "Number of Steps per Day",
     ylab= " Frequency",
     main ="Total Number of Steps Taken per day")
meansteps <- mean(stepsbyDay)
mediansteps <- median(stepsbyDay)

tsSteps <- aggregate(steps ~ interval,data=activity, mean)

si <- ggplot(tsSteps,aes(x=interval,y=steps)) + 
  geom_line() + 
  xlab("Interval") +
  ylab("Average Steps") +
  ggtitle("Average Number of Steps Taken per Interval")
print(si)
mostSteps <- which.max(tsSteps$meanSteps)

activity2 <- activity

#activity2$steps[is.na(activity2$steps)] <- 
#ave(activity2$steps,activity2$interval,
#FUN = function(x)mean(x, na.rm = TRUE))[c(which(is.na(activity2$steps)))]

for (i in 1:dim(activity2)[1]){
  if (is.na(activity2[i,1])){
    int <- activity2[i,3]
    row <- which(tsSteps$interval == int)
    activity2[i,1] = tsSteps[row,2]
  }    
}

stepsbyDay2 <- tapply(activity2$steps,activity2$date,sum,na.rm=TRUE)
hist(stepsbyDay2,xlab = "Number of Steps per Day",
     ylab= " Frequency",
     main ="Total Number of Steps Taken per day (imputed)")
meansteps2 <- mean(stepsbyDay2)
mediansteps2 <- median(stepsbyDay2)

activity2$daytype = weekdays (as.Date(activity2$date))

for (i in 1:dim(activity2)[1]){
  if(activity2[i,4] %in% c("Saturday","Sunday")) {
    activity2[i,4] = "weekend"
  }
   else{
     activity2[i,4] = "weekday" 
   }
}
activity2$daytype <- as.factor(activity2$daytype)
tsSteps2 <-  aggregate(steps ~ interval + daytype, 
                       data=activity2, mean)
sp <- ggplot(tsSteps2,aes(x=interval,y=steps)) + 
  geom_line() +
  facet_grid(daytype ~ .) + 
  xlab("Interval") + 
  ylab("Average Steps") +
  ggtitle("Average Number of Steps Taken per Interval")
print(sp)
