# Cicily Balachandar

data1 <- read.csv("P:/BIST 5605/Project/Balachandar-Cicily-Training Data.csv")

comp <- data1$ï..name

Y <- data1$logP


mod5 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
           +data1$RSO2NR+data1$RNSO2NR+data1$RSR
           +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
           +data1$HBA2+data1$HBD+data1$PSA+data1$MR,
           data=data1)

summary(mod5)


mod1 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
           +data1$RSO2NR+data1$RNSO2NR+data1$RSR
           +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$C+data1$RINGS+data1$AROMATIC,
           data=data1)

mod2 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
           +data1$RSO2NR+data1$RNSO2NR+data1$RSR
           +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
           +data1$HBA2+data1$HBD,
           data=data1)

mod3 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
           +data1$RSO2NR+data1$RNSO2NR+data1$RSR
           +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
           +data1$HBA2+data1$HBD+data1$PSA,
           data=data1)

mod4 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
           +data1$RSO2NR+data1$RNSO2NR+data1$RSR
           +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
           +data1$HBA2+data1$HBD+data1$MR,
           data=data1)



# Diagnostics


#Check normality of error terms
shapiro.test(mod1$residuals) 
shapiro.test(mod2$residuals) 
shapiro.test(mod3$residuals) 
shapiro.test(mod4$residuals) 
shapiro.test(mod5$residuals)


qqnorm(mod1$residuals,
       main="Normal Q-Q Plot for Residuals")
qqnorm(mod2$residuals,
       main="Normal Q-Q Plot for Residuals")
qqnorm(mod3$residuals,
       main="Normal Q-Q Plot for Residuals")
qqnorm(mod4$residuals,
       main="Normal Q-Q Plot for Residuals")
qqnorm(mod5$residuals,
       main="Normal Q-Q Plot for Residuals")

#Check for heteroscedasticity


plot(mod1$fitted.values, rstudent(mod1))
plot(mod2$fitted.values, rstudent(mod2))
plot(mod3$fitted.values, rstudent(mod3))
plot(mod4$fitted.values, rstudent(mod4))
plot(mod5$fitted.values, rstudent(mod5))

library(car)
ncvTest(mod1)
ncvTest(mod2)
ncvTest(mod3)
ncvTest(mod4)
ncvTest(mod5)

#Check for Serial Correlation
durbinWatsonTest(mod1,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(mod2,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(mod3,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(mod4,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(mod5,alternative="positive",
                 max.lag=1,simulate=F)


# Transformation for mod5
p5 <- powerTransform(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
                     +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
                     +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
                     +data1$RSO2NR+data1$RNSO2NR+data1$RSR
                     +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
                     +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
                     +data1$HBA2+data1$HBD+data1$PSA+data1$MR,
                     data1, family="yjPower")

m5 <- lm(yjPower(Y, p5$lambda)~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
         +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
         +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
         +data1$RSO2NR+data1$RNSO2NR+data1$RSR
         +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
         +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
         +data1$HBA2+data1$HBD+data1$PSA+data1$MR,
          data1)

summary(m5)
e5=rstudent(m5)
shapiro.test(m5$residuals)


# Transformation for mod1

p1 <- powerTransform(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
                     +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
                     +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
                     +data1$RSO2NR+data1$RNSO2NR+data1$RSR
                     +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
                     +data1$C+data1$RINGS+data1$AROMATIC,
                     data1, family="yjPower")

m1 <- lm(yjPower(Y, p1$lambda)~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
         +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
         +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
         +data1$RSO2NR+data1$RNSO2NR+data1$RSR
         +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
         +data1$C+data1$RINGS+data1$AROMATIC,
         data1)

summary(m1)
e1=rstudent(m1)
shapiro.test(m1$residuals)

# Transformation for mod2
p2 <- powerTransform(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
                     +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
                     +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
                     +data1$RSO2NR+data1$RNSO2NR+data1$RSR
                     +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
                     +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
                     +data1$HBA2+data1$HBD,
                     data1, family="yjPower")

m2 <- lm(yjPower(Y, p2$lambda)~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
         +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
         +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
         +data1$RSO2NR+data1$RNSO2NR+data1$RSR
         +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
         +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
         +data1$HBA2+data1$HBD,
         data1)

summary(m2)
e2=rstudent(m2)
shapiro.test(m2$residuals)


# Transformation for mod3

p3 <- powerTransform(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
                     +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
                     +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
                     +data1$RSO2NR+data1$RNSO2NR+data1$RSR
                     +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
                     +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
                     +data1$HBA2+data1$HBD+data1$PSA,
                     data1, family="yjPower")

m3 <- lm(yjPower(Y, p3$lambda)~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
         +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
         +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
         +data1$RSO2NR+data1$RNSO2NR+data1$RSR
         +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
         +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
         +data1$HBA2+data1$HBD+data1$PSA,
         data1)

summary(m3)
e3=rstudent(m3)
shapiro.test(m3$residuals)


# Transformation for mod4

p4 <- powerTransform(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
                     +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
                     +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
                     +data1$RSO2NR+data1$RNSO2NR+data1$RSR
                     +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
                     +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
                     +data1$HBA2+data1$HBD+data1$MR,
                     data1, family="yjPower")

m4 <- lm(yjPower(Y, p4$lambda)~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
         +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
         +data1$RCOOR+data1$ROR+data1$RCCH+data1$RCN
         +data1$RSO2NR+data1$RNSO2NR+data1$RSR
         +data1$RSH+data1$RF+data1$RCl+data1$RBr+data1$RSO2R
         +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
         +data1$HBA2+data1$HBD+data1$MR,
         data1)

summary(m4)
e4=rstudent(m4)
shapiro.test(m4$residuals)

ncvTest(m1)
ncvTest(m2)
ncvTest(m3)
ncvTest(m4)
ncvTest(m5)

durbinWatsonTest(m1,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(m2,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(m3,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(m4,alternative="positive",
                 max.lag=1,simulate=F)
durbinWatsonTest(m5,alternative="positive",
                 max.lag=1,simulate=F)


anova(mod1, mod2, test="F")

df <- data.frame(x, Y)
windows(rescale="fit")
r <- cor(df)
out <- which(r == 1)
out
vif(mod2)

library(MASS)


# General Linear Hypothesis Tests
anova(mod2, mod3)
anova(mod2, mod4)
anova(mod2, mod5)



mod6 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
                   +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
                   +data1$RCOOR+data1$ROR
                   +data1$RSO2NR+data1$RSR
                   +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
                   +data1$C+data1$RINGS+data1$AROMATIC+data1$HBA1
                   +data1$HBA2+data1$HBD+data1$PSA+data1$MR,
                   data=data1)
shapiro.test(mod6$residuals)
ncvTest(mod6)
durbinWatsonTest(m1,alternative="positive",
                 max.lag=1,simulate=F)
vif(mod6)



library(leaps)
out.cp <- leaps(x=data1[,1:28], y=data1[,28], method="Cp", nbest=2)


# Fixing Multicollinearity

HB <- data1$HBA1+data1$HBA2+data1$HBD
stdHB <- (HB-mean(HB))/sd(HB)

C <- (data1$C-mean(data1$C))/sd(data1$C)


cor(data1$C, data1$MR)
cor(data1$RINGS, data1$MR)
cor(data1$PSA, data1$MR)
cor(data1$C, data1$PSA)
cor(data1$RINGS, data1$PSA)
cor(data1$AROMATIC, data1$MR)
cor(data1$AROMATIC, data1$PSA)
cor(HB, data1$PSA)
cor(HB, data1$MR)
cor(HB, data1$AROMATIC)
# Only structural features

mod7 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR
           +data1$RSO2NR+data1$RSR
           +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$AROMATIC,
           data=data1)
vif(mod7)
summary(mod7)
anova(mod7)
shapiro.test(mod7$residuals)
ncvTest(mod7)
durbinWatsonTest(mod7,alternative="positive",
                 max.lag=1,simulate=F)

#HBA

mod8 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
            +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
            +data1$RCOOR+data1$ROR
            +data1$RSO2NR+data1$RSR
            +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
            +stdHB,
            data=data1)
vif(mod8)
summary(mod8)
anova(mod8)
shapiro.test(mod8$residuals)
ncvTest(mod8)
durbinWatsonTest(mod8,alternative="positive",
                 max.lag=1,simulate=F)


# C
cor(data1$C, data1$AROMATIC)
C_aro <- data1$AROMATIC+data1$C
mod9b <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
            +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
            +data1$RCOOR+data1$ROR
            +data1$RSO2NR+data1$RSR
            +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
            +C_aro,
            data=data1)
vif(mod9b)
summary(mod9b)
anova(mod9b)
shapiro.test(mod9b$residuals)
ncvTest(mod9b)
durbinWatsonTest(mod9b,alternative="positive",
                 max.lag=1,simulate=F)



# PSA
stdPSA <- (data1$PSA-mean(data1$PSA))/sd(data1$PSA)
cor(data1$PSA, data1$RSO2NR)

mod10 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
            +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
            +data1$RCOOR+data1$ROR
            +data1$RSR
            +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
            +stdPSA,
            data=data1)
vif(mod10)
summary(mod10)
anova(mod10)
shapiro.test(mod10$residuals)
ncvTest(mod10)
durbinWatsonTest(mod10,alternative="positive",
                 max.lag=1,simulate=F)



# MR
stdMR <- (data1$MR-mean(data1$MR))/sd(data1$MR)
mod11 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR
           +data1$RSO2NR+data1$RSR
           +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$MR,
           data=data1)
vif(mod11)
summary(mod11)
anova(mod11)
shapiro.test(mod11$residuals)
ncvTest(mod11)
durbinWatsonTest(mod11,alternative="positive",
                 max.lag=1,simulate=F)


#RINGS
mod12 <- lm(Y~data1$RNH2+data1$R2NH+data1$R3N+data1$ROPO3
           +data1$ROH+data1$RCHO+data1$RCOR+data1$RCOOH
           +data1$RCOOR+data1$ROR
           +data1$RSO2NR+data1$RSR
           +data1$RF+data1$RCl+data1$RBr+data1$RSO2R
           +data1$RINGS,
           data=data1)
vif(mod12)
summary(mod12)
anova(mod12)
shapiro.test(mod12$residuals)
ncvTest(mod12)
durbinWatsonTest(mod12,alternative="positive",
                 max.lag=1,simulate=F)


library(MPV)


extractAIC(mod9b) # Effective DF, AIC
extractAIC(mod9b, k=log(87)) # Effective DF, BIC

extractAIC(mod11) # Effective DF, AIC
extractAIC(mod11, k=log(87)) # Effective DF, BIC



# Model Validation

data2 <- 
  read.csv("P:/BIST 5605/Project/Balachandar-Cicily-Test Data.csv")


C_aro2 <- data2$AROMATIC+data2$C
modv <- lm(data2$logP~data2$RNH2+data2$R2NH+data2$R3N+data2$ROPO3
            +data2$ROH+data2$RCHO+data2$RCOR+data2$RCOOH
            +data2$RCOOR+data2$ROR+data2$RCCH+data2$RCN
            +data2$RSO2NR+data2$RNSO2NR+data2$RSR+data2$RSH
            +data2$RF+data2$RCl+data2$RBr+data2$RSO2R
            +C_aro2,
            data=data2)

summary(modv)
anova(modv)

MSPR <- sum((data2$logP - modv$fitted.values)^2)/40
MSPR




