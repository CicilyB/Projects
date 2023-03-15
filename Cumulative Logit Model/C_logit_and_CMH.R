data2 <- read.table("C:/Users/cib17001/OneDrive - University of Connecticut/PDrive/BIST 5615/dumpingseverity.dat",
                    skip=1)
View(data2)
data3 <- read.table("C:/Users/cib17001/OneDrive - University of Connecticut/PDrive/BIST 5615/dumpingseverity.dat",
                    skip=2)
View(data3)

data4 <- subset(data3, select = -V1)
View(data4)


tab4 <- as.table(array(unlist(data4), dim=c(4, 3, 4),
                       dimnames=list("Operation"=c("A", "B", "C", "D"),
                                     "Dumping Severity"=c("N", "S", "M"),
                                     "Hospital"=c(1, 2, 3, 4))))
tab4

mantelhaen.test(tab4)


# b

dimnames(tab4)
ftable(tab4, row.vars = "Operation", 
       col.vars = c("Hospital", "Dumping Severity"))

df4 <- as.data.frame(tab4)

df4w <- dcast(df4, Operation+Hospital~Dumping.Severity,
              value.var = "Freq")
df4w

# Model with operation and hospital
df4w.fit1 <- vglm(cbind(N, S, M)~Operation+Hospital,
                  family=cumulative(parallel=TRUE), data=df4w)

summary(df4w.fit1)


# Model with Operation

df4w.fit2 <- vglm(cbind(N, S, M)~Operation,
                  family=cumulative(parallel=TRUE), data=df4w)
summary(df4w.fit2)


#H0 Operation only vs HA Operation & Hospital

pchisq(2.5829, 3, lower.tail=FALSE)