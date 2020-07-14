---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The dataset “Activity monitoring data” contains steps, date and interval variables. After observing and cleaning the data,we are able to answer all the questions.



## What is mean total number of steps taken per day?
The mean of total number of steps taken per day is 10766.19. The median of total number of steps taken per day is 10765.


## What is the average daily activity pattern?
The 835 5-min interval contains the maximum number of steps on average across all the days in the dataset, which is 10927 steps.


## Imputing missing values
Some values are different from the estimates from the first part of the assignment (original data) since NA values are removed, the mean is still the same but median will be affected, which differs from original data by 1.19.



## Are there differences in activity patterns between weekdays and weekends?
Yes, the number of steps taken in weekday is higher than in weekend. Perhaps most of people are working in weekday so the steps taken is higher.

*Please refer code.R for the coding and the pictures of graphs
