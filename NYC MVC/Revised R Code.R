library(dplyr)
library(sqldf)
library(data.table)
library(ggplot2)

data1 <- 
  read.csv("//msfs-03.grove.ad.uconn.edu/home/cib17001/Downloads/Motor_Vehicle_Collisions_-_Crashes.csv",
           header=TRUE)

# Rename columns

setnames(data1, c("CRASH.DATE",
                  "NUMBER.OF.PERSONS.INJURED",
                  "NUMBER.OF.PERSONS.KILLED", 
                  "NUMBER.OF.PEDESTRIANS.INJURED",
                  "NUMBER.OF.PEDESTRIANS.KILLED",
                  "NUMBER.OF.CYCLIST.INJURED",
                  "NUMBER.OF.CYCLIST.KILLED",
                  "NUMBER.OF.MOTORIST.INJURED",
                  "NUMBER.OF.MOTORIST.KILLED"),
         c("Date", "Num_Injured", "Num_Killed",
           "Pedestrians_Injured", "Pedestrians_Killed",
           "Cyclists_Injured", "Cyclists_Killed",
           "Motorists_Injured", "Motorists_Killed"))



# Get subsets of data using SQL queries

data2 <- sqldf("SELECT 
      Num_Injured, 
      Num_Killed,
      Pedestrians_Injured,
      Pedestrians_Killed,
      Cyclists_Injured,
      Cyclists_Killed,
      Motorists_Injured,
      Motorists_Killed
      FROM data1")


# df with months attached as column

month <- months.POSIXt(as.Date(data1$Date,format='%m/%d/%Y'))
num <- match(month, month.name)
data3 <- cbind(data2, month, num)


# combining columns into pedestrians, cyclists, and motoorists

data4 <- sqldf("SELECT *, 
               (Num_Injured+Num_Killed) as Total,
               (Pedestrians_Injured+Pedestrians_Killed) as Pedestrians,
               (Cyclists_Injured+Cyclists_Killed) as Cyclists,
               (Motorists_Injured+Motorists_Killed) as Motorists
               FROM data3")


# Group by month

data5 <- sqldf("SELECT month as Month, SUM(Total) as Total, 
                SUM(Pedestrians) as Pedestrians,
                SUM(Cyclists) as Cyclists,
                SUM(Motorists) as Motorists
                FROM data4
                GROUP BY month 
                ORDER BY num")



# can use subset funct: data2 <- subset(data1, select=c('CRASH.TIME'))

### Create bar plots for each group by month

name <- c("Jan", "Feb", "Mar", "Apr", "May", "June",
          "July", "Aug", "Sep", "Oct", "Nov", "Dec")

barplot(data5$Total, names.arg=name, col="red",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 70000), main="All Groups")


barplot(data5$Pedestrians, names.arg=name, col="blue",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 20000), main="Pedestrians")  

barplot(data5$Cyclists, names.arg=name, col="purple",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 20000), main="Cyclists")  


barplot(data5$Motorists, names.arg=name, col="black",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 40000), main="Motorists")





