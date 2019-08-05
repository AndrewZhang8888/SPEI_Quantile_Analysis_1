#read in data
tempraw<- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/Ridgetop 1995-2019 Refined.csv", header=TRUE, sep = ",")

#sort into dataframes by year
tempsplit<-split(tempraw, tempraw$Year)

#return sum for each year
Average=NULL
for (i in tempsplit){
  tempavg<-data.frame(i$Year[1], mean(i$ï..AvgTEMP_C))
  Average<-rbind(Average, tempavg)
}

Average$mean.i.ï..AvgTEMP_C.[1]<- 9.76204
Average$mean.i.ï..AvgTEMP_C.[2]<- 14.67024
# Simple Bar Plot 
counts <- (Average$mean.i.ï..AvgTEMP_C.)
barplot(counts,names.arg = Average$i.Year.1., main="Avg Annual Temperature", 
        xlab="Year", ylab="Temperature (c)")


