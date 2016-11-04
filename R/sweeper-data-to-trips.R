# roll up GPS point data to distance and duration data by trip
library(data.table)
setwd(dirname(parent.frame(2)$ofile)) # set current working dir to source directory (only works when run as source)
df <- read.csv("sweeper-data.csv", header=TRUE, sep=",", as.is = TRUE, strip.white=TRUE)

df$ODometer <- round(as.numeric(df$ODometer), 5)
df$Speed <- as.numeric(df$Speed)
df$Event <- as.factor(df$Event)

datetime <- strptime(df$Time, format="%m/%d/%Y %H:%M")
df$date <- as.IDate(datetime)
df$time <- as.ITime(datetime)

DT <- data.table(df)


trips <- DT[, list(
  start.date=min(date),
  end.date=max(date),
  start.time=min(time), 
  end.time=max(time), 
  start.odometer=min(ODometer), 
  end.odometer=max(ODometer)
                  ), by=.(asset_label, date, Trip)]

trips <- trips[trips$start.date == trips$end.date,] # get rid of any trips spanning more than one day

trips$distance <- trips$end.odometer - trips$start.odometer
trips$duration <- (trips$end.time - trips$start.time) / 60 # duration in mins
trips$avg.speed <- trips$distance / (trips$duration / 60) # mph

# get rid of trips less than a half mile, or less than 10 minutes
trips <- trips[trips$distance > 0.5,]
trips <- trips[trips$distance < 150,]
trips <- trips[trips$duration > 10, ]

hist(trips$distance)
hist(trips$duration)

trips$division <- substr(trips$asset_label,4,4)

boxplot(avg.speed ~ division, data=trips, ylab="avg speed (mph)", xlab="division", main="average sweeper speeds by division")

write.csv(trips, 'sweeper-trips.csv', row.names = FALSE)
