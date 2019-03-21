#====================================================================================
# HW 3 Multinomial Choices
# Yaxuan Jiao
# NetID: yj124
#====================================================================================
#load the data
install.packages("bayesm")
library(bayesm)
data(margarine)
#overview of the data
str(margarine)
#====================================================================================
#Exercise 2 First Model for Effect of Price on Demand 
#====================================================================================
#creat dummy matrix to select probability
dij <- matrix(0,nrow = 4470, ncol = 10)
for (i in 1:4470) {
  for (j in 1:10) {
    if (margarine$choicePrice[i,2]==j){
      dij[i,] <- 0
      dij[i,j] <- 1
    }
  }
}
#compute coefficients
beta <- -0.5
alpha <- rnorm(9)
theta <- c(beta,alpha)
#compute the conditional logit loglikelihood function
c_logit <- function(theta){
  pdata <- margarine$choicePrice[,3:12]
  pdata1 <- pdata - pdata[,1]
  xijb <- pdata1*theta[1]
  constant <- c(0,theta[2:10])
  a <- matrix(constant,nrow = 4470, ncol = length(constant),byrow = TRUE)
  vij <- xijb + a
  e_vij <- exp(vij)
  e_s_vij <- as.data.frame(apply(e_vij,1,sum)) 
  pij <- log(e_vij/e_s_vij[,1])
  prob <- pij*dij
  probability <- sum(prob)
  return(-probability)
}
c_logit(theta)
#optimize the conditional logit function
clogit_optim <- optim(par = theta, c_logit)$par
print(clogit_optim)

#======================================================================================
#Exercise 3 Second Model for Effect of Family Income on Demand 
#======================================================================================
#compute coefficients (a1-a10,b1-b10)
thetaa <- rnorm(20)
thetaa[1] <- 0
thetaa[11] <- 0
#compute income matrix by choice
Ii <- matrix(0,nrow = 4470, ncol = 1)
for (i in 1:4470) {
  for (j in 1:nrow(margarine$demos)) {
    if (margarine$choicePrice[i,1]==margarine$demos[j,1]){
      Ii[i,1] <- margarine$demos[j,2]
    }
  }
}
#compute the multinomial logit loglikelihood function
m_logit <- function(thetaa){
  betaa <- matrix(thetaa[11:20], nrow = 4470, ncol = length(thetaa[11:20]), byrow = TRUE)
  alphaa <- matrix(thetaa[1:10], nrow = 4470, ncol = length(thetaa[1:10]), byrow = TRUE)
  Iibj <- betaa*as.vector(Ii)
  uij <- as.data.frame(Iibj + alphaa) 
  e_uij <- exp(uij)
  e_s_uij <- as.data.frame(apply(e_uij,1,sum))
  Pij <-  log(e_uij/e_s_uij[,1])
  Prob <- Pij*dij
  Probability <- sum(Prob)
  return(-Probability)
}
m_logit(thetaa)
#optimize the multinomial logit function 
mlogit_optim <- optim(par = thetaa, m_logit)$par
print(mlogit_optim)

#===========================================================================================
#Exercise 4 Marginal Effects
#===========================================================================================
#Q1:calculate the marginal effect of conditional logit model
pdata4 <- margarine$choicePrice[,3:12]
pdata14 <- pdata - pdata[,1]
a1 <- c(0,clogit_optim[2:10])
a2 <- matrix(a1,nrow = 4470, ncol = length(a1),byrow = TRUE)
vij4 <- pdata14*clogit_optim[1]+a2
e_vij4 <- exp(vij4)
e_s_vij4 <- as.data.frame(apply(e_vij4,1,sum)) 
pij4 <- as.matrix(e_vij4/e_s_vij4[,1])
#construct the ten by ten ME matrix for each individual (4470 times)
save <- matrix(0,nrow = 10, ncol = 10)
I_matrix <- diag(x=1, nrow = 10, ncol = 10)
ten_by_ten <- matrix(0,nrow = 10, ncol = 10)
ME_c <- matrix(0,nrow = 10, ncol = 10)
for (i in 1:4470) {
  save <- matrix(rep(pij4[i,1:10],each=10),nrow = 10,ncol=10)
  ten_by_ten <- t(save)*(I_matrix-save)*clogit_optim[1]
  ME_c <- ME_c+ten_by_ten
}
#average the marginal effect
ME_clogit<-ME_c/4470
View(ME_clogit)

#Q2:calculate the marginal effect of multinomial logit model
bj <- as.matrix(mlogit_optim[11:20])
bbj <- t(bj)
Bj <- matrix(rep(bbj[1,1:10],each=10),nrow = 4470, ncol = 10)
Iibj <- Bj*as.vector(Ii)
alphai <- as.matrix(mlogit_optim[1:10])
aalphai <- t(alphai)
Alphai <- matrix(rep(aalphai[1,1:10],each=10),nrow = 4470, ncol = 10)
uij4 <- as.data.frame(Iibj + Alphai) 
e_uij4 <- exp(uij4)
e_s_uij4 <- as.data.frame(apply(e_uij4,1,sum))
Pij4 <- e_uij4/e_s_uij4[,1]
#construct the ten by one ME matrix for each individual (4470 times)
temp <- matrix(0,nrow = 1, ncol = 10)
ten_by_one <- matrix(0,nrow = 1, ncol = 10)
ME_m <- matrix(0,nrow = 1, ncol = 10)
for (i in 1:4470){
  temp <- as.matrix(Pij4[i,1:10],ncol=10,nrow=1)
  b_bar <- temp%*%bj
  b_bari <- rep(b_bar,each=10)
  b_barii <- t(b_bari)
  ten_by_one <- temp*(mlogit_optim[11:20]-b_barii)
  ME_m <- ME_m+ten_by_one
}
ME_mlogit<-ME_m/4470
View(ME_mlogit)
#============================================================================================
#Exercise 5 Testing the properties of IIA using a mixed logit model
#============================================================================================
#Q1:mixed logit likelihood function
#compute coefficients for xij and Iij (b,a1-a9;A1-A10,B1-B10)
beta_f <- rnorm(30)
#compute mixed logit loglikelihood function
mixed_logit <- function(beta_f){
  beta_f[11] <- 0
  beta_f[21] <- 0
  #price effect part
  pdata5 <- margarine$choicePrice[,3:12]
  pdata15 <- pdata5 - pdata5[,1]
  xijb5 <- pdata15*beta_f[1]
  constant5 <- c(0,beta_f[2:10])
  a5 <- matrix(constant5,nrow = 4470, ncol = length(constant5),byrow = TRUE)
  mij <- xijb5 + a5
  #income effect part
  betaa5 <- matrix(beta_f[21:30], nrow = 4470, ncol = length(beta_f[21:30]), byrow = TRUE)
  alphaa5 <- matrix(beta_f[11:20], nrow = 4470, ncol = length(beta_f[11:20]), byrow = TRUE)
  Iibj5 <- betaa5*as.vector(Ii)
  wij <- as.data.frame(Iibj5 + alphaa5) 
  #combine effects in a mixed equation
  zij <- mij+wij
  e_zij <- exp(zij)
  e_s_zij <- as.data.frame(apply(e_zij,1,sum))
  pij5 <-  log(e_zij/e_s_zij[,1])
  Prob5 <- Pij5*dij
  Probability5 <- sum(Prob5)
  return(-Probability5)
}
mixed_logit(beta_f)
#optimize the multinomial logit function 
mixed_logit_optim <- optim(par = beta_f, mixed_logit)$par
print(mixed_logit_optim)

#Q2:remove choice 2 from the data to estimate the effects of price and family income on demand
new_data <- margarine$choicePrice
for (i in 1:4470) {
  if (new_data[i,2]==2){
    new_data[i,2] <- NA}
}
less_data <- na.omit(new_data)
lessc_data <- subset(less_data,select=c(hhid,choice,PPk_Stk,PFl_Stk,PHse_Stk,PGen_Stk,PImp_Stk,PSS_Tub,PPk_Tub,PFl_Tub,PHse_Tub))
#creat new dummy matrix to select probability
Dij <- matrix(0,nrow = 3771, ncol = 9)
for (i in 1:3371) {
  for (j in 1:9) {
    if (lessc_data[i,2]==j){
      dij[i,] <- 0
      dij[i,j] <- 1
    }
  }
}
#compute coefficients 
beta_r <- rnorm(27)
#compute income matrix by less choice
Ii_lessc <- matrix(0,nrow = 3771, ncol = 1)
for (i in 1:3771) {
  for (j in 1:nrow(margarine$demos)) {
    if (lessc_data[i,1]==margarine$demos[j,1]){
      Ii_lessc[i,1] <- margarine$demos[j,2]
    }
  }
}
#compute mixed logit loglikelihood function
nmixed_logit <- function(beta_r){
  beta_r[10] <- 0
  beta_r[19] <- 0
  #price effect part
  pdata6 <- lessc_data[,3:11]
  pdata16 <- pdata6 - pdata6[,1]
  xijb6 <- pdata16*beta_r[1]
  constant6 <- c(0,beta_r[2:9])
  a6 <- matrix(constant6,nrow = 3771, ncol = length(constant6),byrow = TRUE)
  Eij <- xijb6 + a6
  #income effect part
  betaa6 <- matrix(beta_r[19:27], nrow = 3771, ncol = length(beta_r[19:27]), byrow = TRUE)
  alphaa6 <- matrix(beta_r[10:18], nrow = 3771, ncol = length(beta_r[10:18]), byrow = TRUE)
  Iibj6 <- betaa6*as.vector(Ii_lessc)
  Rij <- as.data.frame(Iibj6 + alphaa6) 
  #combine effects in a mixed equation
  oij <- Eij+Rij
  e_oij <- exp(oij)
  e_s_oij <- as.data.frame(apply(e_oij,1,sum))
  Pij6 <-  log(e_oij/e_s_oij[,1])
  Prob6 <- Pij6*dij
  Probability6 <- sum(Prob6)
  return(-Probability6)
}
nmixed_logit(beta_r)
#optimize the multinomial logit function 
nmixed_logit_optim <- optim(par = beta_r, nmixed_logit)$par
print(nmixed_logit_optim)

#Q3:compute test statistic for IIA test (test for independence of irrelevent alternatives)
l_betaf <- mixed_logit(mixed_logit_optim)
l_betar <- nmixed_logit(nmixed_logit_optim)
MTT <- -2*(l_betaf-l_betar)
print(MTT)
#conclude on IIA
a_betar <- abs(nmixed_logit_optim)
chisq.test(a_betar)

