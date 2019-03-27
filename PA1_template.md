Reproducible Research: Peer Assessment 1

Loading and preprocessing the data
----------------------------------

    ###1.Load the data 

    source_data = read.csv("C:/Users/abinashi.prakash/Desktop/Coursera/repdata_data_activity/activity.csv")

    ###2.Process/transform the data (if necessary) into a format suitable for your analysis--Not required as data is already in required format.

What is mean total number of steps taken per day?
-------------------------------------------------

    ###1.Calculate the total number of steps taken per day

    Tot_steps <- sum(source_data$steps, na.rm = TRUE)

    Tot_steps_day <- aggregate(steps~date, data=source_data, FUN=sum, na.rm=TRUE)

    ###2. Make a histogram of the total number of steps taken each day

    hist(Tot_steps_day$steps,main = "Total Steps per Day",xlab = "Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    ###3.Calculate and report the mean and median of the total number of steps taken per day

    Tot_steps_day_mean <- mean(Tot_steps_day$steps,na.rm = TRUE)
    Tot_steps_day_mean

    ## [1] 10766.19

    Tot_steps_day_median <- median(Tot_steps_day$steps,na.rm = TRUE)
    Tot_steps_day_median

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

    ###1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    avg_interval <- aggregate(steps~interval, data=source_data, FUN=mean, na.rm=TRUE)

    plot(x = avg_interval$interval, y = avg_interval$steps, type = "l") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

    max_steps <- max(avg_interval$steps) ##Storing Max steps in a variable
    for (i in 1:288) 
    {
      if (avg_interval$steps[i] == max_steps)
        max_steps_five_min_interval <- avg_interval$interval[i]
    }
    max_steps_five_min_interval 

    ## [1] 835

Imputing missing values
-----------------------

    ###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    total_na <- 0
    for (i in 1:17568)
    {
      if(is.na(source_data$steps[i])) 
        total_na <- total_na+1 
    }
    total_na

    ## [1] 2304

    ###2.Devise a strategy for filling in all of the missing values in the dataset.
    ###3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
    source_data_filled_in <- source_data
    for (i in 1:17568) 
    {
      if(is.na(source_data_filled_in$steps[i])) # if steps is na store the pointer 
      { 
        five_min_point <- source_data_filled_in$interval[i] 
        for (j in 1:288)  
        {
          if (avg_interval$interval[j] == five_min_point) 
            source_data_filled_in$steps[i] <- avg_interval$steps[j] 
          
        }
      }
    }

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    Tot_steps_day_filled_in <- aggregate(steps~date, data=source_data_filled_in, FUN=sum, na.rm=TRUE)
    ###Histogram of the total number of steps taken each day

    hist(Tot_steps_day_filled_in$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    ###Calculate and report the mean and median total number of steps taken per day. 
    Tot_steps_day_mean_filled_in <- mean(Tot_steps_day_filled_in$steps)
    Tot_steps_day_mean_filled_in

    ## [1] 10766.19

    Tot_steps_day_median_filled_in <- median(Tot_steps_day_filled_in$steps)
    Tot_steps_day_median_filled_in

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    ###1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    week <- weekdays(as.Date(source_data_filled_in$date))
    week_day <- week

    for (i in 1:17568) 
    {
      if(week[i] == "Sunday")
        week_day[i] <- 'weekend'
      if(week[i] == "Monday")
        week_day[i] <- 'weekday'
      if(week[i] == "Tuesday")
        week_day[i] <- 'weekday'
      if(week[i] == "Wednesday")
        week_day[i] <- 'weekday'
      if(week[i] == "Thursday")
        week_day[i] <- 'weekday'
      if(week[i] == "Friday")
        week_day[i] <- 'weekday'
      if(week[i] == "Saturday")
        week_day[i] <- 'weekend'
    }

    source_data_filled_in$weekday <- week_day

    ###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    weekday <- grep("weekday",source_data_filled_in$weekday)
    wday_frame <- source_data_filled_in[weekday,]
    wend_frame <- source_data_filled_in[-weekday,]

    ## Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
    ##    averaged across all days (yaxis)
    avg_interval_wday <- aggregate(steps~interval, data=wday_frame, FUN=mean, na.rm=TRUE)
    ##avg_interval_wday
    avg_interval_wend <- aggregate(steps~interval, data=wend_frame, FUN=mean, na.rm=TRUE)
    ##avg_interval_wend

    ### Plots for Weekend and Weekdays
    plot(x = avg_interval_wday$interval, y = avg_interval_wday$steps, type = "l") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    plot(x = avg_interval_wend$interval, y = avg_interval_wend$steps, type = "l") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-2.png)
