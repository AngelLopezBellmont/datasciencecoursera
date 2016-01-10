#Module-5-Project-1
I can not commit the new PA1_template.Rmd I just made I dont know why

Here is the code:

---
title: "Module-5 Project-1"
author: "Angel_Lopez_Bellmont" 
date: "January 10, 2016"
output: html_document
---

## 1.Code for reading in the dataset and/or processing the data

I point to my workingdirectory, and I control if the dataSet activity.csv is already read or not

```{r echo=TRUE}

setwd("C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch/Mod5_Proj_1")
#here I control that the file is read only when it is not been already read
if(!exists("myData")) 
{
  mypath_toFile_csv <-  paste("C:/2015.07.31_Angel/00Angel/SoftwareProgramsCursosIT/Coursera/2016.01.06-Mod5_ReproducibleResearch",
                              "/Mod5_Proj_1/activity.csv", sep="")
  
  myData  <-  read.csv(file=mypath_toFile_csv,  header=TRUE, sep=",") 
  head (myData)
}

stepsPerDay <- aggregate (steps ~ date, myData, sum, na.rm = TRUE)
head(stepsPerDay)


```

 
## 2. Histogram of the total number of steps taken each day 


```{r, echo=TRUE }

#png ("stepsPerDay_Total.png", height = 500, width = 500)
hist (stepsPerDay$steps, main = "Total Steps make forEach Day", xlab = "Number of Steps", ylab="number")
#graphics.off()

```

## 3.mean and median
Here we calculate the mean and median

```{r echo=TRUE}
stepsPerDayMean <- mean (stepsPerDay$steps)
stepsPerDayMedian <- median (stepsPerDay$steps)

stepsPerDayMean
stepsPerDayMedian

```



## 4.Time series plot of the average number of steps taken


```{r echo=TRUE}
stepsMeanEachInterval <- aggregate (steps ~ interval, myData, mean)
plot (stepsMeanEachInterval$interval, stepsMeanEachInterval$steps, type = "l", main = "Average Number of Steps for Each Interval", xlab = "Interval", ylab = "Number of Steps")

```



## 5. The 5-minute interval that, on average, contains the maximum number of steps

```{r echo=TRUE}
  intervalMaxStep <- stepsMeanEachInterval [which.max(stepsMeanEachInterval$steps), 1]
  intervalMaxStep

```


## 6. Code to describe and show a strategy for imputing missing data

  We change the NA of each interval in the dataSet, for the mean value for that interval.
  I run all myData each row and if it's NA and  I put the mean value for that interval in case there is a NA.
  The new data set I call it myData_2.
  
```{r echo=TRUE}
  
  newColumnSteps <- numeric ()
  
  for (i in 1:nrow (myData)) 
{
     row_i <- myData [i, ]
    
    if (is.na (row_i$steps)) 
    {
       stepsNew <- subset (stepsMeanEachInterval, interval == row_i$interval)$steps
    }
    else 
    {
      stepsNew <- row_i$steps
    }
    
  newColumnSteps <- c (newColumnSteps, stepsNew)
}

# we create the new DataSet myData_2
myData_2 <- myData
myData_2$steps <- newColumnSteps

head(myData_2)

```



## 7. Histogram of the total number of steps taken each day after missing values are imputed
```{r echo=TRUE}

stepsPerDay_2 <- aggregate (steps ~ date, myData_2, sum, na.rm = TRUE)
hist (stepsPerDay_2$steps, main = "Total Steps make forEach Day", xlab = "Number of Steps", ylab="number")

```


## 8.Panel plot comparing steps in weekdays vs. weekends

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
weekdays gives back the day of the week example: x <- weekdays(as.Date("2016-01-09")) gives x = "Saturday"

I have to transfor the original dataset  myDate, the date colum into date.

```{r echo=TRUE}

myData$date <- as.Date(myData$date, "%Y-%m-%d")

day <- weekdays (myData$date)
daytype <- vector ()

  for (i in 1:nrow (myData)) 
  {
    if (day [i] == "Saturday")
    {
      daytype [i] <- "weekend"
    }
    else if (day [i] == "Sunday")
    {
      daytype [i] <- "weekend"
    }
    else 
    {
      daytype [i] <- "weekday"
    }
  }

myData$daytype <- daytype
myData$daytype <- factor (myData$daytype)



myDataWeek <- aggregate (steps ~ interval + daytype, myData, mean)
names(myDataWeek) <- c ("interval", "daytype", "steps")

noWeekEnd <- subset (myDataWeek, daytype=="weekday")
weekEnd <- subset (myDataWeek, daytype=="weekend")

plot (noWeekEnd$interval, noWeekEnd$steps, type ="l" , col="blue",  xlab="interval", ylab="steps")
lines (  weekEnd$interval,   weekEnd$steps, type ="l", col="red")
legend("topright", c("no weekEnds", "Weekends"), lty=1, lwd=2.5, col=c("blue", "red"))

```














