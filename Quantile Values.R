library(dplyr)
library(ggplot2)
a <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/Ridgetop 1995-2019 Refined.csv", sep = ",", header = TRUE)
a <- select(a, ď..AvgTEMP_C, MaxTEMP_C, MinTEMP_C, Month, Day, Year)
a1 <- select(a, ď..AvgTEMP_C, Year)%>%
  group_by(Year) %>%
  summarize(average = mean(ď..AvgTEMP_C))
ab1 <- na.omit(a1)
a2 <- select(a, MaxTEMP_C, Year)%>%
  group_by(Year)%>%
  summarize(average = mean(MaxTEMP_C))
ab2 <- na.omit(a2)
a3 <- select(a, MinTEMP_C, Year)%>%
  group_by(Year) %>%
  summarize(average = mean(MinTEMP_C))
ab3 <- na.omit(a3)
b1 <- pnorm(ab1$average, mean = mean(a$ď..AvgTEMP_C, na.rm = TRUE), sd = sd(a$ď..AvgTEMP_C, na.rm = TRUE))
b2 <- pnorm(ab2$average, mean = mean(a$MaxTEMP_C, na.rm = TRUE), sd = sd(a$MaxTEMP_C, na.rm = TRUE))
b3 <- pnorm(ab3$average, mean = mean(a$MinTEMP_C, na.rm = TRUE), sd = sd(a$MinTEMP_C, na.rm = TRUE))

library(dplyr)
library(ggplot2)
a <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/Ridgetop 1995-2019 Refined.csv", sep = ",", header = TRUE)
a <- select(a, ď..AvgTEMP_C, MaxTEMP_C, MinTEMP_C, Month, Day, Year)
a1 <- select(a, ď..AvgTEMP_C, Year)%>%
  group_by(Year) %>%
  summarize(average = mean(ď..AvgTEMP_C))
ab1 <- na.omit(a1)
a2 <- select(a, MaxTEMP_C, Year)%>%
  group_by(Year)%>%
  summarize(average = mean(MaxTEMP_C))
ab2 <- na.omit(a2)
a3 <- select(a, MinTEMP_C, Year)%>%
  group_by(Year) %>%
  summarize(average = mean(MinTEMP_C))
ab3 <- na.omit(a3)
p1 <- quantile(ab1$average, c(0.25,0.50,0.75))
p2 <- quantile(ab2$average, c(0.25,0.50,0.75))
p3 <- quantile(ab3$average, c(0.25,0.50,0.75))

library(dplyr)
library(ggplot2)
a <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/Ridgetop 1995-2019 Refined.csv", sep = ",", header = TRUE)
a <- select(a, ï..AvgTEMP_C, MaxTEMP_C, MinTEMP_C, TotalRain, Month, Day, Year)
a1 <- select(a, TotalRain, Year)%>%
  group_by(Year) %>%
  summarize(average = sum(TotalRain))
ab1 <- na.omit(a1)
#Quantile Values of Each Variable
c <- quantile(ab1$average, c(0.25,0.50,0.75))
