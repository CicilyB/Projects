library(dplyr)
library(sqldf)
library(data.table)
library(ggplot2)
library(reshape2)

# Dataset From:
# https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95


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


# df with months and month num attached as column

month <- months.POSIXt(as.Date(data1$Date,format='%m/%d/%Y'))
month_num <- match(month, month.name)
data3 <- cbind(data2, month, month_num)


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
                ORDER BY month_num")



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



## bar plot of proportions/percentages

# remove total column and use month_num for month column

data6 <- sqldf("SELECT month_num as Month, 
                SUM(Pedestrians) as Pedestrians,
                SUM(Cyclists) as Cyclists,
                SUM(Motorists) as Motorists
                FROM data4
                GROUP BY month_num")

# Convert data from wide to long format

long_data6 <- melt(data6, id.vars = "Month")


# Percentage bar plot by month for groups(pedestrian, cyclist, motorist)

plot1 <- ggplot(long_data6, aes(as.factor(Month), value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent)

plot1 + labs(x = "Month", y = "Percentage Injured/Killed in a Accident", fill = "Group",
             title = "Percentage Injured/Killed in a Accident by Group")


# Proportions bar plot by month for groups(pedestrian, cyclist, motorist)

plot2 <- ggplot(long_data6, aes(as.factor(Month), value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity")

plot2 + labs(x = "Month", y = "Proportion Injured/Killed in a Accident", fill = "Group")




