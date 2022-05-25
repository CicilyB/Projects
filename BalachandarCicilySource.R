# Cicily Balachandar

install.packages("sqldf")
library(sqldf)


data1 <- 
  read.csv("//msfs-03.grove.ad.uconn.edu/home/cib17001/Downloads/Motor_Vehicle_Collisions_-_Crashes.csv")

# Subset 1

subset1 <- data.frame(c1=data1$CRASH.TIME,
                c2=data1$NUMBER.OF.PERSONS.INJURED,
                c3=data1$NUMBER.OF.PERSONS.KILLED,
                c4=data1$NUMBER.OF.PEDESTRIANS.INJURED,
                c5=data1$NUMBER.OF.PEDESTRIANS.KILLED,
                c6=data1$NUMBER.OF.CYCLIST.INJURED,
                c7=data1$NUMBER.OF.CYCLIST.KILLED,
                c8=data1$NUMBER.OF.MOTORIST.INJURED,
                c9=data1$NUMBER.OF.MOTORIST.KILLED,
                c10=data1$CONTRIBUTING.FACTOR.VEHICLE.1,
                c11=data1$CRASH.DATE)
head(subset1)

# Subset 2 
c23 <- data1$NUMBER.OF.PERSONS.INJURED+data1$NUMBER.OF.PERSONS.KILLED
c45 <- data1$NUMBER.OF.PEDESTRIANS.INJURED+data1$NUMBER.OF.PEDESTRIANS.KILLED
c67 <- data1$NUMBER.OF.CYCLIST.INJURED+data1$NUMBER.OF.CYCLIST.KILLED
c89 <- data1$NUMBER.OF.MOTORIST.INJURED+data1$NUMBER.OF.MOTORIST.KILLED

subset2 <- data.frame(c1=data1$CRASH.TIME, c2=data1$CRASH.DATE,
                      c3=c23, c4=c45, c5=c67, c6=c89, c7=data1$CONTRIBUTING.FACTOR.VEHICLE.1)
colnames(subset2) <- c("Time", "Date", "Total People", "Pedestrians",
                       "Cyclists", "Motorists", "Factor")
head(subset2)

set2 <- subset2[c("Total People", "Pedestrians",
                  "Cyclists", "Motorists")]
summary(set2)




v1 <- data1$CRASH.TIME[which(c23 > 0)]
v2 <- data1$CRASH.DATE[which(c23 > 0)]
v3 <- c23[which(c23 > 0)]
v4 <- data1$CONTRIBUTING.FACTOR.VEHICLE.1[which(c23 > 0)]

subset3 <- data.frame(c1=v1, c2=v2, c3=v3, c4=v4)
head(subset3)

subset4 <- data.frame(c1=v3, c2=v4)
colnames(subset4) <- c("Total", "Factor")

table(subset4$c2)
table(subset4[,"Factor"])


# Create barplots with frequencies
# of injuries and deaths for diff groups
b1 <- c(sum(c45), sum(c67), sum(c89))
b2 <- c(sum(data1$NUMBER.OF.PEDESTRIANS.INJURED),
        sum(data1$NUMBER.OF.CYCLIST.INJURED),
        sum(data1$NUMBER.OF.MOTORIST.INJURED))
b3 <- c(sum(data1$NUMBER.OF.PEDESTRIANS.KILLED),
        sum(data1$NUMBER.OF.CYCLIST.KILLED),
        sum(data1$NUMBER.OF.MOTORIST.KILLED))

barplot(b1, names.arg=c("Pedestrians", "Cyclists", "Motorists"), col="blue",
        ylab="Freq of Deaths/Injuries", ylim=c(0,500000), main="People Killed/Injured")
barplot(b2, names.arg=c("Pedestrians", "Cyclists", "Motorists"), col="red",
        ylab="Freq of Injuries", ylim=c(0,500000), main="People Injured")
barplot(b3, names.arg=c("Pedestrians", "Cyclists", "Motorists"), col="purple",
        ylab="Freq of Deaths", ylim=c(0,2000), main="People Killed")


#Subset 5

subset5 <- subset(subset2, Factor == "Driver Inattention/Distraction")
head(subset5)

table(subset5$Time)

date5 <- as.Date(data1$CRASH.DATE,format='%m/%d/%Y')

g1 <- as.POSIXct.Date(date5)

month1 <- months.POSIXt(g1)
head(month1)


#Subset 6

subset6 <- data.frame(c1=month1, c2=data1$NUMBER.OF.PERSONS.INJURED,
                      c3=data1$NUMBER.OF.PERSONS.KILLED,
                      c4=data1$NUMBER.OF.PEDESTRIANS.INJURED,
                      c5=data1$NUMBER.OF.PEDESTRIANS.KILLED,
                      c6=data1$NUMBER.OF.CYCLIST.INJURED,
                      c7=data1$NUMBER.OF.CYCLIST.KILLED,
                      c8=data1$NUMBER.OF.MOTORIST.INJURED,
                      c9=data1$NUMBER.OF.MOTORIST.KILLED,
                      c10=data1$CONTRIBUTING.FACTOR.VEHICLE.1,
                      c11=c23, c12=c45, c13=c67, c14=c89)


# Separate frequencies of injuries by month

jan1 <- subset6$c11[which(subset6$c1 =="January")]
feb1 <- subset6$c11[which(subset6$c1=="February")]
mar1 <- subset6$c11[which(subset6$c1=="March")]
apr1 <- subset6$c11[which(subset6$c1=="April")]
may1 <- subset6$c11[which(subset6$c1=="May")]
june1 <- subset6$c11[which(subset6$c1=="June")]
july1 <- subset6$c11[which(subset6$c1=="July")]
aug1 <- subset6$c11[which(subset6$c1=="August")]
sep1 <- subset6$c11[which(subset6$c1=="September")]
oct1 <- subset6$c11[which(subset6$c1=="October")]
nov1 <- subset6$c11[which(subset6$c1=="November")]
dec1 <- subset6$c11[which(subset6$c1=="December")]


jan2 <- subset6$c12[which(subset6$c1=="January")]
feb2 <- subset6$c12[which(subset6$c1=="February")]
mar2 <- subset6$c12[which(subset6$c1=="March")]
apr2 <- subset6$c12[which(subset6$c1=="April")]
may2 <- subset6$c12[which(subset6$c1=="May")]
june2 <- subset6$c12[which(subset6$c1=="June")]
july2 <- subset6$c12[which(subset6$c1=="July")]
aug2 <- subset6$c12[which(subset6$c1=="August")]
sep2 <- subset6$c12[which(subset6$c1=="September")]
oct2 <- subset6$c12[which(subset6$c1=="October")]
nov2 <- subset6$c12[which(subset6$c1=="November")]
dec2 <- subset6$c12[which(subset6$c1=="December")]


jan3 <- subset6$c13[which(subset6$c1=="January")]
feb3 <- subset6$c13[which(subset6$c1=="February")]
mar3 <- subset6$c13[which(subset6$c1=="March")]
apr3 <- subset6$c13[which(subset6$c1=="April")]
may3 <- subset6$c13[which(subset6$c1=="May")]
june3 <- subset6$c13[which(subset6$c1=="June")]
july3 <- subset6$c13[which(subset6$c1=="July")]
aug3 <- subset6$c13[which(subset6$c1=="August")]
sep3 <- subset6$c13[which(subset6$c1=="September")]
oct3 <- subset6$c13[which(subset6$c1=="October")]
nov3 <- subset6$c13[which(subset6$c1=="November")]
dec3 <- subset6$c13[which(subset6$c1=="December")]


jan4 <- subset6$c14[which(subset6$c1=="January")]
feb4 <- subset6$c14[which(subset6$c1=="February")]
mar4 <- subset6$c14[which(subset6$c1=="March")]
apr4 <- subset6$c14[which(subset6$c1=="April")]
may4 <- subset6$c14[which(subset6$c1=="May")]
june4 <- subset6$c14[which(subset6$c1=="June")]
july4 <- subset6$c14[which(subset6$c1=="July")]
aug4 <- subset6$c14[which(subset6$c1=="August")]
sep4 <- subset6$c14[which(subset6$c1=="September")]
oct4 <- subset6$c14[which(subset6$c1=="October")]
nov4 <- subset6$c14[which(subset6$c1=="November")]
dec4 <- subset6$c14[which(subset6$c1=="December")]



#Create barplot for frequency by month
b4 <- c(sum(jan1, na.rm=TRUE), sum(feb1, na.rm=TRUE),
   sum(mar1, na.rm=TRUE), sum(apr1, na.rm=TRUE),
   sum(may1, na.rm=TRUE), sum(june1, na.rm=TRUE),
   sum(july1, na.rm=TRUE), sum(aug1, na.rm=TRUE),
   sum(sep1, na.rm=TRUE), sum(oct1, na.rm=TRUE),
   sum(nov1, na.rm=TRUE), sum(dec1, na.rm=TRUE))
  

name <- c("Jan", "Feb", "Mar", "Apr", "May", "June",
          "July", "Aug", "Sep", "Oct", "Nov", "Dec")

barplot(b4, names.arg=name, col="red",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 70000), main="All People")


b5 <- c(sum(jan2, na.rm=TRUE), sum(feb2, na.rm=TRUE),
              sum(mar2, na.rm=TRUE), sum(apr2, na.rm=TRUE),
              sum(may2, na.rm=TRUE), sum(june2, na.rm=TRUE),
              sum(july2, na.rm=TRUE), sum(aug2, na.rm=TRUE),
              sum(sep2, na.rm=TRUE), sum(oct2, na.rm=TRUE),
              sum(nov2, na.rm=TRUE), sum(dec2, na.rm=TRUE))
  
barplot(b5, names.arg=name, col="blue",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 20000), main="Pedestrians")  
  

b6 <- c(sum(jan3, na.rm=TRUE), sum(feb3, na.rm=TRUE),
        sum(mar3, na.rm=TRUE), sum(apr3, na.rm=TRUE),
        sum(may3, na.rm=TRUE), sum(june3, na.rm=TRUE),
        sum(july3, na.rm=TRUE), sum(aug3, na.rm=TRUE),
        sum(sep3, na.rm=TRUE), sum(oct3, na.rm=TRUE),
        sum(nov3, na.rm=TRUE), sum(dec3, na.rm=TRUE))


barplot(b6, names.arg=name, col="purple",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 20000), main="Cyclists")  


b7 <- c(sum(jan4, na.rm=TRUE), sum(feb4, na.rm=TRUE),
        sum(mar4, na.rm=TRUE), sum(apr4, na.rm=TRUE),
        sum(may4, na.rm=TRUE), sum(june4, na.rm=TRUE),
        sum(july4, na.rm=TRUE), sum(aug4, na.rm=TRUE),
        sum(sep4, na.rm=TRUE), sum(oct4, na.rm=TRUE),
        sum(nov4, na.rm=TRUE), sum(dec4, na.rm=TRUE))

barplot(b7, names.arg=name, col="black",
        ylab="Freq of Deaths/Injuries", ylim=c(0, 40000), main="Motorists")



# Create barplots for frequency by factor

h1 <- data1$NUMBER.OF.PERSONS.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction")]
h3 <- data1$NUMBER.OF.PERSONS.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Alcohol Involvement")]
h4 <- data1$NUMBER.OF.PERSONS.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Fatigued/Drowsy")]

b8 <- c(sum(h1, na.rm=TRUE),
        sum(h3, na.rm=TRUE), sum(h4, na.rm=TRUE))

barplot(b8, names.arg=c("Distracted", "Alcohol", "Fatigue"),
        col="brown", ylim=c(0, 150000),
        ylab="Freq of Injured", main="Number of People Injured")


h5 <- data1$NUMBER.OF.PEDESTRIANS.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction")]
h6 <- data1$NUMBER.OF.PEDESTRIANS.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Alcohol Involvement")]
h7 <- data1$NUMBER.OF.PEDESTRIANS.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Fatigued/Drowsy")]

b9 <- c(sum(h5, na.rm=TRUE),
        sum(h6, na.rm=TRUE), sum(h7, na.rm=TRUE))
barplot(b9, names.arg=c("Distracted", "Alcohol", "Fatigue"),
        col="grey", ylim=c(0, 30000),
        ylab="Freq of Injured", main="Number of Pedestrians Injured")

h8 <- data1$NUMBER.OF.CYCLIST.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction")]
h9 <- data1$NUMBER.OF.CYCLIST.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Alcohol Involvement")]
h10 <- data1$NUMBER.OF.CYCLIST.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Fatigued/Drowsy")]

b10 <- c(sum(h8, na.rm=TRUE),
        sum(h9, na.rm=TRUE), sum(h10, na.rm=TRUE))
barplot(b10, names.arg=c("Distracted", "Alcohol", "Fatigue"),
        col="green", ylim=c(0, 30000),
        ylab="Freq of Injured", main="Number of Cyclists Injured")


h11 <- data1$NUMBER.OF.MOTORIST.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction")]
h12 <- data1$NUMBER.OF.MOTORIST.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Alcohol Involvement")]
h13 <- data1$NUMBER.OF.MOTORIST.INJURED[which(data1$CONTRIBUTING.FACTOR.VEHICLE.1=="Fatigued/Drowsy")]

b11 <- c(sum(h11, na.rm=TRUE),
         sum(h12, na.rm=TRUE), sum(h13, na.rm=TRUE))
barplot(b11, names.arg=c("Distracted", "Alcohol", "Fatigue"),
        col="black", ylim=c(0, 100000),
        ylab="Freq of Injured", main="Number of Motorists Injured")




