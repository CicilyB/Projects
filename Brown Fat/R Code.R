brownfat <- read_excel("P:/BIST 5505/brownfat_projectdata.xlsx")
brownfat <- read.csv("//msfs-03.grove.ad.uconn.edu/home/cib17001/Downloads/brownfat (2).csv",
            header=TRUE)
# Data Manipulation
diabetes <- brownfat$Diabetes
bf <- brownfat$BrownFat
brown <- brownfat$Total_vol[which(brownfat$BrownFat == "1")]

YY <- bf[which(bf == "1" & diabetes == "1")]
NN <- bf[which(bf == "0" & diabetes == "0")]
YN <- bf[which(bf == "1" & diabetes == "0")]
NY <- bf[which(bf == "0" & diabetes == "1")]

length(YY)
length(NN)
length(YN)
length(NY)

Ydiab <- diabetes[which(diabetes == "1")]
Ndiab <- diabetes[which(diabetes == "0")]

length(Ydiab)
length(Ndiab)

Ybf <- bf[which(bf == "1")]
Nbf <- bf[which(bf == "0")]  
  
length(Ybf)
length(Nbf)


tot_vol <- brownfat$Total_vol
winter_fat <- tot_vol[which(brownfat$Season == "4")]
summer_fat <- tot_vol[which(brownfat$Season == "2")]

tot_adj <- tot_vol[which(tot_vol > 0)]
wint_adj <- winter_fat[which(winter_fat > 0)]
summ_adj <- summer_fat[which(summer_fat > 0)]



# Exploratory Data Analysis

hist(tot_vol, main="Histogram of Total Volume of Brown Fat", 
     xlab="Total Volume (mL)", col="blue")
shapiro.test(tot_vol)

hist(winter_fat, main="Histogram of Total Volume of Brown Fat in Winter", 
     xlab="Total Volume (mL)", col="purple")
shapiro.test(winter_fat)

hist(summer_fat, main="Histogram of Total Volume of Brown Fat in Summer", 
     xlab="Total Volume (mL)", col="red")
shapiro.test(summer_fat)

qqnorm(winter_fat)
qqnorm(summer_fat)


hist(wint_adj, main="Histogram of Total 
     Volume of Brown Fat in Winter 
     (Those with Brown Fat)", 
     xlab="Total Volume (mL)", col="blue")
shapiro.test(wint_adj)

hist(summ_adj, main="Histogram of Total Volume of Brown Fat in Summer
     (Those with Brown Fat)", 
     xlab="Total Volume (mL)", col="red")
shapiro.test(summ_adj)


boxplot(brown ~ brownfat$Season[which(brownfat$BrownFat == "1")], 
        names = unique(brownfat$Season[which(brownfat$BrownFat == "1")]), 
        xlab="Season", ylab="Total Volume of Brown Fat",
        col="lightblue", notch=T, main="Brown Fat Volume for Seasons
        (Of Those Who Have Brown Fat)")


library(car)

Y = wint_adj
bc=powerTransform(Y) # default method is Box-Cox
YBC=Y^bc$lambda
hist(YBC, main="Histogram of Total 
     Volume of Brown Fat Of Those Who have 
     Brown Fat in Winter (After Box-Cox Transformation)", 
     xlab="Total Volume (mL)", col="blue")
shapiro.test(YBC)
qqnorm(YBC)

X = summ_adj
bc=powerTransform(X) # default method is Box-Cox
XBC=X^bc$lambda
hist(XBC, main="Histogram of Total
     Volume of Brown Fat Of Those Who have 
     Brown Fat in Summer 
     (After Box-Cox Transformation)", 
     xlab="Total Volume (mL)", col="red")
shapiro.test(XBC)
qqnorm(XBC)



# Statistical Inference

var.test(YBC, XBC, alternative="two.sided", conf.level=0.95)
t.test(YBC,XBC, alternative = "greater", var.equal=FALSE)


ansari.test(wint_adj, summ_adj, alternative= "two.sided")
wilcox.test(wint_adj, summ_adj, alternative = "greater")




fat = matrix(c(303, 3695, 6, 469), nrow = 2,
                 dimnames = list(Diabetes = c("No", "Yes"),
                                 BrownFat = c("Yes", "No")))

E11 <- 309*3998/4473
E21 <- 4164*3998/4473
E12 <- 309*475/4473
E22 <- 4164*475/4473

expected = matrix(c(E11, E21, E12, E22), nrow = 2,
                  dimnames = list(Diabetes = c("No", "Yes"),
                                  BrownFat = c("Yes", "No")))
chisq.test(fat)

ORhat <- (303*469)/(6*3695)
