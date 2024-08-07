setwd("C:/Users/cib17001/OneDrive - University of Connecticut/PDrive/BIST 5092/Background materials and data for the course project/Data/Phase3-data/data_phase 3")

ADPA <- read.csv("ADPA.csv")

ADSL <- read.csv("ADSL.csv")



#### libraries ####


library(dplyr)
library(ggplot2)
library(reshape2)
library(mmrm)
library(lme4)
library(nlme)
library(survival)
library(ggsurvfit)


#### Data Manipulation and Data Subsets ####

# Added WEIGHT, PASI, and ARMN columns to ADPA based on SUBJID

data1 <- left_join(ADPA, select(ADSL, SUBJID, WEIGHT, PASI, ARMN, AGEGR1N, SEXN))


# Filter to keep only PASI data

data1 <- data1 %>% filter(PARAMCD == "PASI")


# Recoded WEIGHT (0 if WEIGHT <= 100kg and 1 if WEIGHT > 100kg) 
# Recoded BASEPASI (0 if PASI <= median(PASI) and 1 if PASI > median(PASI))
# Created new col PASI75 (0 if PCHG < 75% and 1 if PCHG >= 75%)
# Create new col CLASS:
#   (1 if WEIGHT=0 & PASI = 0)
#   (2 if WEIGHT=1 & PASI = 0)
#   (3 if WEIGHT=0 & PASI = 1)
#   (4 if WEIGHT=1 & PASI = 1)
# Made TRTPN into a factor

data2 <- data1 %>%
  mutate(WEIGHT = if_else(WEIGHT <= 100, 0, 1)) %>%
  mutate(BASEPASI = if_else(BASE <= median(BASE), 0, 1)) %>%
  mutate(PASI75 = if_else(PCHG < 75, 0 , 1)) %>%
  mutate(CLASS = case_when(WEIGHT==0 & BASEPASI==0 ~ 1,
                           WEIGHT==1 & BASEPASI==0 ~ 2,
                           WEIGHT==0 & BASEPASI==1 ~ 3,
                           WEIGHT==1 & BASEPASI==1 ~ 4)) %>%
  mutate(TRTPN = as.factor(TRTPN))


data2 <- data1 %>%
  mutate(WEIGHT = if_else(WEIGHT <= 100, 0, 1)) %>%
  mutate(WEIGHT = factor(WEIGHT, levels=c(0,1))) %>% 
  mutate(BASEPASI = if_else(BASE <= median(BASE), 0, 1)) %>%
  mutate(PASI75 = if_else(PCHGCAT1 == '>=75%', 1, 0)) %>%
  mutate(CLASS = case_when(WEIGHT==0 & BASEPASI==0 ~ 1,
                           WEIGHT==1 & BASEPASI==0 ~ 2,
                           WEIGHT==0 & BASEPASI==1 ~ 3,
                           WEIGHT==1 & BASEPASI==1 ~ 4)) %>%
  mutate(TRTPN = as.factor(TRTPN))



length(data2$PASI75[which(data2$PASI75==1)])
identical(data2$TRTPN, data2$TRTAN)



# Data for the Different Hypotheses at Visit 6

dataH1 <- data2 %>% filter(TRTPN == 1 | TRTPN == 4) %>%
  droplevels() %>% filter(AVISITN == 6)

dataH2 <- data2 %>% filter(TRTPN == 2 | TRTPN == 4) %>%
  droplevels() %>% filter(AVISITN == 6)

dataH3 <- data2 %>% filter(TRTPN == 1 | TRTPN == 3) %>%
  droplevels() %>% filter(AVISITN == 6)
  
dataH4 <- data2 %>% filter(TRTPN == 2 | TRTPN == 3) %>%
  droplevels() %>% filter(AVISITN == 6)


# Data for the Different Hypotheses at all Visits (Linear Mixed Effect Models)
# Added WEEK col (Visit 2 = week 0, Visit 3 = week 1, ...)
# Set WEEK as factor
# Set SUBJID as factor

dataMMH1 <- data2 %>% filter(TRTPN == 1 | TRTPN == 4) %>%
  droplevels() %>% mutate(WEEK = 
                            case_when(AVISITN==2~0,
                                      AVISITN==3~1,
                                      AVISITN==4~2,
                                      AVISITN==5~3,
                                      AVISITN==6~4)) %>%
  mutate(WEEK = as.factor(WEEK)) %>%
  mutate(SUBJID = as.factor(SUBJID))

  
dataMMH2 <- data2 %>% filter(TRTPN == 2 | TRTPN == 4) %>%
  droplevels() %>% mutate(WEEK = 
                            case_when(AVISITN==2~0,
                                      AVISITN==3~1,
                                      AVISITN==4~2,
                                      AVISITN==5~3,
                                      AVISITN==6~4)) %>%
  mutate(WEEK = as.factor(WEEK)) %>%
  mutate(SUBJID = as.factor(SUBJID))


dataMMH3 <- data2 %>% filter(TRTPN == 1 | TRTPN == 3) %>%
  droplevels() %>% mutate(WEEK = 
                            case_when(AVISITN==2~0,
                                      AVISITN==3~1,
                                      AVISITN==4~2,
                                      AVISITN==5~3,
                                      AVISITN==6~4)) %>%
  mutate(WEEK = as.factor(WEEK)) %>%
  mutate(SUBJID = as.factor(SUBJID))



dataMMH4 <- data2 %>% filter(TRTPN == 2 | TRTPN == 3) %>%
  droplevels() %>% mutate(WEEK = 
                            case_when(AVISITN==2~0,
                                      AVISITN==3~1,
                                      AVISITN==4~2,
                                      AVISITN==5~3,
                                      AVISITN==6~4)) %>%
  mutate(WEEK = as.factor(WEEK)) %>%
  mutate(SUBJID = as.factor(SUBJID))




# Time-To-Event Data (For Kaplan-Meier Curve)
# Changed WEEK to numeric
# Filtered MMH Datasets to get:
# 1=Placebo, 2=Active Control, 3=140mg, 4=210mg

survdata1 <- dataMMH1 %>% filter(TRTPN == 1) %>% 
  droplevels() %>% mutate(WEEK = as.numeric(WEEK))

survdata2 <- dataMMH2 %>% filter(TRTPN == 2) %>% 
  droplevels() %>% mutate(WEEK = as.numeric(WEEK))

survdata3 <- dataMMH3 %>% filter(TRTPN == 3) %>% 
  droplevels() %>% mutate(WEEK = as.numeric(WEEK))

survdata4 <- dataMMH2 %>% filter(TRTPN == 4) %>% 
  droplevels() %>% mutate(WEEK = as.numeric(WEEK))


# Data for Log Rank Tests
# Change WEEK col from MMH datasets to numeric





#### Exploratory Analysis ####

### Binary Proportions
## Functions (1st value of vector is proportion with PASI75 and 2nd is prop not with PASI75)

# Function to get Proportion of PASI75 by Treatment randomization (trt = 1, 2, 3, 4)

trt_PASI <- function(data2, trt){
  prop_data <- data2 %>% filter(TRTPN == trt)
  n <- unique(prop_data$SUBJID)
  PASI75_count <- unique(prop_data$SUBJID[which(prop_data$PASI75 == 1)])
  prop_PASI75 <- round(length(PASI75_count)/length(n) * 100, 3)
  return(c(prop_PASI75, 100-prop_PASI75))
}


# Function to get Proportion of PASI75 by Sex (sex = 1, 2)

sex_PASI <- function(data2, trt, sex){
  prop_data <- data2 %>% filter(SEXN == sex) %>%
    filter(TRTPN == trt)
  n <- unique(prop_data$SUBJID)
  PASI75_count <- unique(prop_data$SUBJID[which(prop_data$PASI75 == 1)])
  prop_PASI75 <- round(length(PASI75_count)/length(n) * 100, 3)
  return(c(prop_PASI75, 100-prop_PASI75))
}

sex_PASI(data2, 1, 1)

# Function to get Proportion of PASI75 by Age (AGEGR1N = 1, 2, 3, 4, 5)
# 1="<20"; 2=">=20-<40"; 3=">=40-<60"; 4=">=60-<80"; 5=">=80"

age_PASI <- function(data2, trt, age){
  prop_data <- data2 %>% filter(AGEGR1N == age) %>%
    filter(TRTPN == trt)
  n <- unique(prop_data$SUBJID)
  PASI75_count <- unique(prop_data$SUBJID[which(prop_data$PASI75 == 1)])
  prop_PASI75 <- round(length(PASI75_count)/length(n) * 100, 3)
  return(c(prop_PASI75, 100-prop_PASI75))
}

age_PASI(data2, 1, 3)

unique(ADSL$AGEGR1)


# Function to get Proportion of PASI75 by Weight (weight = 0, 1)

weight_PASI <- function(data2, trt, weight){
  prop_data <- data2 %>% filter(WEIGHT == weight) %>%
    filter(TRTPN == trt)
  n <- unique(prop_data$SUBJID)
  PASI75_count <- unique(prop_data$SUBJID[which(prop_data$PASI75 == 1)])
  prop_PASI75 <- round(length(PASI75_count)/length(n) * 100, 3)
  return(c(prop_PASI75, 100-prop_PASI75))
}

weight_PASI(data2, 1, 0)

# Function to get Proportion of PASI75 by PASI Baseline Score (BASEPASI = 0, 1)

base_PASI <- function(data2, trt, base){
  prop_data <- data2 %>% filter(BASEPASI == base) %>%
    filter(TRTPN == trt)
  n <- unique(prop_data$SUBJID)
  PASI75_count <- unique(prop_data$SUBJID[which(prop_data$PASI75 == 1)])
  prop_PASI75 <- round(length(PASI75_count)/length(n) * 100, 3)
  return(c(prop_PASI75, 100-prop_PASI75))
}


base_PASI(data2, 1, 0)



## Functions for plotting


# Dataframe for randomization

x <- rep(c("Placebo", "Active Control", "140mg", "210mg"), each=2)
y <- rep(c('PASI 75=1','PASI 75=0'), 4)
z <- c(trt_PASI(data2, 1), trt_PASI(data2, 2),
       trt_PASI(data2, 3), trt_PASI(data2, 4))
ggdf_trt <- data.frame(x=x, y=y, z=z)




# df's for sex using sex_PASI function

ggdf_sex <- function(trt){
  x <- rep(c("M", "F"), each=2)
  y <- rep(c('PASI 75=1','PASI 75=0'), 2)
  z <- c(sex_PASI(data2, trt, 1), sex_PASI(data2, trt, 2))
  df <- data.frame(x=x, y=y, z=z)
}




# Get df's for age (trt = 1,2,3,4)
# Uses the age_PASI function to get values for z vector

ggdf_age <- function(trt){
  x <- rep(c("0-19", "20-39", "40-59", "60-79", "80+"), each=2)
  y <- rep(c('PASI 75=1','PASI 75=0'), 5)
  z <- c(age_PASI(data2, trt, 1), age_PASI(data2, trt, 2),
         age_PASI(data2, trt, 3), age_PASI(data2, trt, 4),
         age_PASI(data2, trt, 5))
  df <- data.frame(x=x, y=y, z=z)
}








### Plots (Binary Proportions of PASI75)

## PASI 75 by Randomization of treatment


ggplot(data=ggdf_trt, mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Treatment") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 by Treatment") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))



## Sex PASI 75 Plots

# Placebo

ggplot(data=ggdf_age(1), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Sex") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in Placebo by Sex") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))


# Active Control

ggplot(data=ggdf_age(2), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Sex") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in Active Control by Sex") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))


# 140mg of Drug X

ggplot(data=ggdf_age(3), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Sex") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in 140mg of Drug X by Sex") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))



# 210 mg of Drug X


ggplot(data=ggdf_age(4), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Sex") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in 210mg of Drug X by Sex") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))




## Age (Using ggdf_age function)

# PASI75 plot for Placebo (trt=1) by age

ggplot(data=ggdf_age(1), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Age") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in Placebo by Age") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))


# PASI75 plot for Active Control by age

ggplot(data=ggdf_age(2), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Age") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in Active Control by Age") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))


# PASI75 plot for 140mg by age

ggplot(data=ggdf_age(3), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Age") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in 140mg of Drug X by Age") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))


# PASI75 plot for 210mg by age

ggplot(data=ggdf_age(4), mapping = aes(x=x, y=z, fill=y)) +
  geom_bar(stat="identity", position="stack", width=0.6) +
  xlab("Age") + ylab("% with PASI 75") + 
  ggtitle("PASI 75 in 210mg of Drug X by Age") +
  geom_text(mapping = aes(label=z), position=position_dodge(width=0.4))






#### Primary Analysis ####

### Alpha Calculations  (Split Recycle Alpha)

# Comparisons with Placebo Alpha = 0.01 (Family 1)

alpha1 <- 0.01




# Comparisons with Active Control Alpha = 0.04 (Family 2)

alpha2 <- 0.04





### CMH
## Use dataH1, dataH2, dataH3, or dataH4 correspondinng to the hypoth being tested

# CMH Test for H1 (High Dose vs Placebo) (dataH1)


table1 <- ftable(CLASS ~ TRTPN + PASI75, data=dataH1)


array1 <- array(unlist(table1), 
                dim = c(2, 2, 4),
                dimnames = list("TRTPN" = c(1, 4),
                                "PASI75" = c(0, 1),
                                "CLASS" = c(1, 2, 3, 4)))

mantelhaen.test(array1)





# CMH Test for H3 (Low Dose vs Placebo) (dataH3)

table3 <- ftable(CLASS ~ TRTPN + PASI75, data=dataH3)


array3 <- array(unlist(table3), 
                dim = c(2, 2, 4),
                dimnames = list("TRTPN" = c(1, 3),
                                "PASI75" = c(0, 1),
                                "CLASS" = c(1, 2, 3, 4)))

mantelhaen.test(array3)


# CMH Test for H2 (High Dose vs Active Control) (dataH2)

table2 <- ftable(CLASS ~ TRTPN + PASI75, data=dataH2)


array2 <- array(unlist(table2), 
                dim = c(2, 2, 4),
                dimnames = list("TRTPN" = c(2, 4),
                                "PASI75" = c(0, 1),
                                "CLASS" = c(1, 2, 3, 4)))

mantelhaen.test(array2)



# CMH Test for H4 (Low Dose vs Active Control) (dataH4)

table4 <- ftable(CLASS ~ TRTPN + PASI75, data=dataH4)


array4 <- array(unlist(table4), 
                dim = c(2, 2, 4),
                dimnames = list("TRTPN" = c(2, 3),
                                "PASI75" = c(0, 1),
                                "CLASS" = c(1, 2, 3, 4)))

mantelhaen.test(array4)




### Logistic Regression
# Use dataH1, dataH2, dataH3, dataH4

# H1
mod1 <- glm(PASI75 ~ TRTPN + WEIGHT + BASEPASI, family=binomial, data=dataH1)
summary(mod1)

mod1 <- glm(PASI75 ~ TRTPN*WEIGHT*BASEPASI, family=binomial, data=dataH1)
summary(mod1)

mod0 <- glm(PASI75 ~ WEIGHT+BASEPASI, family=binomial, data=dataH1)
anova(mod0, mod1)

# H3

mod3 <- glm(PASI75 ~ TRTPN + WEIGHT + BASEPASI, family=binomial, data=dataH3)
summary(mod3)


# H2

mod2 <- glm(PASI75 ~ TRTPN + WEIGHT + BASEPASI, family=binomial, data=dataH2)
summary(mod2)


# H4

mod4 <- glm(PASI75 ~ TRTPN + WEIGHT + BASEPASI, family=binomial, data=dataH4)
summary(mod4)



#### Supplemental Analysis ####

### Linear Mixed Effect Models
## Use dataMMH1, dataMMH2, dataMMH3, dataMMH4

# H1

lme1 <- lmer(PCHG~WEIGHT+BASEPASI+TRTPN+(1|USUBJID), data=dataMMH1)
summary(lme1)

# H3

lme3 <- lmer(PCHG~WEIGHT+BASEPASI+TRTPN+(1|USUBJID), data=dataMMH3)
summary(lme3)

# H2

lme2 <- lmer(PCHG~WEIGHT+BASEPASI+TRTPN+(1|USUBJID), data=dataMMH2)
summary(lme2)

# H4

lme4 <- lmer(PCHG~WEIGHT+BASEPASI+TRTPN+(1|USUBJID), data=dataMMH4)
summary(lme4)




### Kaplan-Meier Curve
## Use survdata1, survdata2, survdata3, survdata4
## 1=Placebo; 2=Active Control; 3=140mg; 4=210mg

# Placebo

surv_object1 <- survfit(Surv(WEEK, PASI75)~1, data=survdata1)

plot(surv_object1, xlab="Time", ylab="Proportion of PASI 75", 
     main="Kaplan-Meier Curve for Placebo")


# Active control

surv_object2 <- survfit(Surv(WEEK, PASI75)~1, data=survdata2)

plot(surv_object2, xlab="Time", ylab="Proportion of PASI 75", 
     main="Kaplan-Meier Curve for Active Control")



# Test drug (3) (Low Dose - 140 mg)


surv_object3 <- survfit(Surv(WEEK, PASI75)~1, data=survdata3)

plot(surv_object3, xlab="Time", ylab="Proportion of PASI 75", 
     main="Kaplan-Meier Curve for Test Drug (140mg)")



# Test drug (4) (High Dose - 210 mg)


surv_object4 <- survfit(Surv(WEEK, PASI75)~1, data=survdata4)

plot(surv_object4, xlab="Time", ylab="Proportion of PASI 75", 
     main="Kaplan-Meier Curve for Test Drug (210mg)")




### Log-Rank Test
## Use Parallel Testing for Families of Hypotheses
# Fix Log Rank Test


data6 <- data2 %>% mutate(WEEK = 
                            case_when(AVISITN==2~0,
                                      AVISITN==3~1,
                                      AVISITN==4~2,
                                      AVISITN==5~3,
                                      AVISITN==6~4))

surv_obj <- Surv(data6$WEEK, data6$PASI75)

survdiff(surv_obj ~ TRTPN + strata(WEIGHT) + strata(BASEPASI), 
         data=data6)






