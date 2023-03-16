install.packages("mvtnorm")
install.packages("mmrm")


library(mvtnorm)
library(dplyr)
library(mmrm)


### Problem 1

n = 500
p = 3
wk = c(4, 12, 24)

change.time <- function(time) {
	10+1*time
}
plot(x=0:36, y=change.time(0:36), type= "l")
abline(v=12, lty=2)

mean.vec <- change.time(wk)

f.p <- function(p) {
  sin(p)*sqrt(p^2 / (1-p^2))
}


COV <- matrix(0, ncol=p, nrow=p, byrow=TRUE)

sigma=c(14, 10, 8)
rho = c(0.6, 0.5)

for (i in 1:3){
  for (j in i:3){
    if (i == j) COV[i, j] = log(sigma[i])
    else {COV[i, j] = prod(f.p(rho[i:(j-1)]))}
  }
}



COV <- COV + t (COV)
diag(COV) = diag(COV) / 2

COV


d <- rmvnorm(n=n, mean=mean.vec, sigma=COV)
View(d)


d.long <- data.frame(t(d)) %>% stack() %>% rename(change=values, usubjid=ind) %>%
  mutate(week=as.factor(rep(1:3, n)))

View(d.long)

options(max.print=10000)



### Problem 2
## a

# Unrestricted
fit.mmrm <- mmrm(data=d.long, 
                 formula=change~week+us(week|usubjid), method="Kenward-Roger", reml=TRUE)


# Toeplitz
fit.toeph <- mmrm(data=d.long, formula=change~week+toeph(week|usubjid), method="Kenward-Roger", reml=TRUE)

fit.toep <- mmrm(data=d.long, formula=change~week+toep(week|usubjid), method="Kenward-Roger", reml=TRUE)


# Coumpound Symmetry
fit.cs <- mmrm(data=d.long, formula=change~week+cs(week|usubjid), method="Kenward-Roger", reml=TRUE)


# Spatial Exponential

d.long2 <- data.frame(t(d)) %>% stack() %>% rename(change=values, usubjid=ind) %>%
  mutate(week=as.numeric(rep(1:3, n)))

fit.spat <- mmrm(data=d.long2, 
                   formula=change~week+sp_exp(week|usubjid), method="Kenward-Roger", reml=TRUE)


## b

fit.mmrm.sum <- summary(fit.mmrm)

contrast=c(0, 0, 1)

df_1d(fit.mmrm, contrast)      
df_1d(fit.mmrm, contrast)$p_val 



###3

power.mmrm = c(rep(0, 1000))

for (k in 1000) {
  d <- rmvnorm(n=n, mean=mean.vec, sigma=COV)
  d.long <- data.frame(t(d)) %>% stack() %>% rename(change=values, usubjid=ind) %>%
    mutate(week=as.factor(rep(1:3, n)))
  
  fit.mmrm <- mmrm(data=d.long, 
                   formula=change~week+us(week|usubjid), method="Kenward-Roger", reml=TRUE)
  
  fit.mmrm.sum <- summary(fit.mmrm)
  
  contrast=c(0, 0, 1)
  if (df_1d(fit.mmrm, contrast)$p_val < 0.05) {
     power.mmrm[k] <- 1
     k = k + 1
  }
  else{k = k + 1}
  
  return(power.mmrm)
 }

power.sim <- mean(power.mmrm)
power.sim



#library(tmvtnorm)
#library(matrixcalc)
#library(corpcor)

#is.positive.definite(COV)


# https://openpharma.github.io/mmrm/main/articles/covariance.html#homogeneous-ad-and-heterogeneous-ante-dependence-adh




