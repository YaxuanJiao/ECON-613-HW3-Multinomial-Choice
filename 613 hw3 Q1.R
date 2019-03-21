#======================================================================================================================================================================
# HW 3 Multinomial Choices
# Yaxuan Jiao
# NetID: yj124
#======================================================================================================================================================================
#load the data
install.packages("bayesm")
library(bayesm)
data(margarine)
#overview of the data
str(margarine)
#======================================================================================================================================================================
#Exercise 1 product characteristics
#======================================================================================================================================================================
# Question 1
#calculate average, range, variance, and sd
avegprice <- as.matrix(apply(margarine$choicePrice[,3:12],2,mean))
prirange <- as.matrix(apply(margarine$choicePrice[,3:12],2,range))
rownames(prirange) <- c("min","max")
price_range <- t(prirange)
privariance <- as.matrix(apply (margarine$choicePrice[,3:12],2,var))
prisd <- as.matrix(apply (margarine$choicePrice[,3:12],2,sd))
#calculaye coefficient of variance
coef_variance <- as.matrix(prisd/privariance)
#compute interquartile range
interqr <- as.matrix(apply (margarine$choicePrice[,3:12],2,IQR))

#make a table to discribe the average and dispersion in product characteristics 
average_and_dispersion <- data.frame(
  Average <- avegprice,
  Range <- price_range,
  Variance <- privariance,
  SD <- prisd,
  IQR <- interqr
)
colnames(average_and_dispersion) <- c("Average","Range-min","Range-max","Variance","SD","IQR")
print(average_and_dispersion)

#Question 2
#(1)calculate market share by products
product_sales <- matrix(0,nrow = 11, ncol = 2)
rownames(product_sales) <- c("PPk_Stk","PBB_Stk","PFl_Stk","Phse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPK_Tub","PFl_Tub","PHse_Tub","M_Industry")
colnames(product_sales) <- c("product sales","market share")
for (i in 1:4470) {
  for (j in 1:10) {
    if (margarine$choicePrice[i,2]==j){
      x = margarine$choicePrice[i,j+2]
      product_sales[j,1] = x + product_sales[j,1]}
  }
}
product_sales[11,1] <- sum(product_sales[1:10],1)
product_sales[,2] <- product_sales[,1]/2485.51
print(product_sales)
#(2)calculate market share by brands
brand_sales <- matrix(0,nrow = 8,ncol = 2)
rownames(brand_sales) <- c("PPk","PBB","PFl","PHse","PGen","PImp","PSS","M_Industry")
colnames(brand_sales) <- c("brand sales","market share")
for (i in 1:4470) {
  #PPk
  if (margarine$choicePrice[i,2]==1){
    x1 = margarine$choicePrice[i,3]
    brand_sales[1,1] = x1 + brand_sales[1,1]}
  if (margarine$choicePrice[i,2]==8){
    x2 = margarine$choicePrice[i,10]
    brand_sales[1,1] = x2+ brand_sales[1,1]}
  #PBB
  if (margarine$choicePrice[i,2]==2){
    x3 = margarine$choicePrice[i,4]
    brand_sales[2,1] = x3 + brand_sales[2,1]} 
  #PFl
  if (margarine$choicePrice[i,2]==3){
    x4 = margarine$choicePrice[i,5]
    brand_sales[3,1] = x4 + brand_sales[3,1]}
  if (margarine$choicePrice[i,2]==9){
    x5 = margarine$choicePrice[i,11]
    brand_sales[3,1] = x5 + brand_sales[3,1]}
  #PHse
  if (margarine$choicePrice[i,2]==4){
    x6 = margarine$choicePrice[i,6]
    brand_sales[4,1] = x6 + brand_sales[4,1]}
  if (margarine$choicePrice[i,2]==10){
    x7 = margarine$choicePrice[i,12]
    brand_sales[4,1] = x7 + brand_sales[4,1]}
  #PGen
  if (margarine$choicePrice[i,2]==5){
    x8 = margarine$choicePrice[i,7]
    brand_sales[5,1] = x8 + brand_sales[5,1]}
  #PImp
  if (margarine$choicePrice[i,2]==6){
    x9 = margarine$choicePrice[i,8]
    brand_sales[6,1] = x9 + brand_sales[6,1]}
  #PSS
  if (margarine$choicePrice[i,2]==7){
    x10 = margarine$choicePrice[i,9]
    brand_sales[7,1] = x10 + brand_sales[7,1]}
}
brand_sales[8,1] <- sum(brand_sales[1:7],1)
brand_sales[,2] <- brand_sales[,1]/2485.51
print(brand_sales)
#(3)calculate market share by stick & tub
type_sales <- matrix(0,nrow = 3,ncol = 2)
rownames(type_sales) <- c("Stick","Tub","M_Industry")
colnames(type_sales) <- c("type sales","market share")
for (i in 1:4470) {
  for (j in 1:6) {
    if (margarine$choicePrice[i,2]==j){
      y1 = margarine$choicePrice[i,j+2]
      type_sales[1,1] = y1 + type_sales[1,1]}
  }
}
for (i in 1:4470) {
  for (m in 7:10) {
    if (margarine$choicePrice[i,2]==m){
      y2 = margarine$choicePrice[i,m+2]
      type_sales[2,1] = y2 + type_sales[2,1]}
  }
}
type_sales[3,1] <- sum(type_sales[1:2],1)
type_sales[,2] <- type_sales[,1]/2485.51
print(type_sales)

#Question 3 
str(margarine$demos$Income)
mdata <- merge(margarine$choicePrice, margarine$demos, by = "hhid", all.x = TRUE)
library(dplyr)
sdata <- mdata %>%
  select(hhid,choice,Income,Fam_Size,college,whtcollar,retired)
#measure the frequency of buying each product for each income level
Income_choice <- matrix(0,nrow = 14,ncol = 10)
rownames(Income_choice) <- c("2.5","7.5","12.5","17.5","22.5","27.5","32.5","37.5","42.5","47.5","55","67.5","87.5","130")
colnames(Income_choice) <- c("PPk_Stk","PBB_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPK_Tub","PFl_Tub","PHse_Tub")
for (i in 1:4470) {
  for (j in 1:10) {
    if (sdata[i,3]=="2.5" & sdata[i,2]==j){
        Income_choice[1,j] = 1+Income_choice[1,j]}
    if (sdata[i,3]=="7.5" & sdata[i,2]==j){
        Income_choice[2,j] = 1+Income_choice[2,j]}
    if (sdata[i,3]=="12.5" & sdata[i,2]==j){
        Income_choice[3,j] = 1+Income_choice[3,j]}
    if (sdata[i,3]=="17.5" & sdata[i,2]==j){
        Income_choice[4,j] = 1+Income_choice[4,j]}
    if (sdata[i,3]=="22.5" & sdata[i,2]==j){
        Income_choice[5,j] = 1+Income_choice[5,j]}
    if (sdata[i,3]=="27.5" & sdata[i,2]==j){
        Income_choice[6,j] = 1+Income_choice[6,j]}
    if (sdata[i,3]=="32.5" & sdata[i,2]==j){
        Income_choice[7,j] = 1+Income_choice[7,j]}
    if (sdata[i,3]=="37.5" & sdata[i,2]==j){
        Income_choice[8,j] = 1+Income_choice[8,j]}
    if (sdata[i,3]=="42.5" & sdata[i,2]==j){
        Income_choice[9,j] = 1+Income_choice[9,j]}
    if (sdata[i,3]=="47.5" & sdata[i,2]==j){
        Income_choice[10,j] = 1+Income_choice[10,j]}
    if (sdata[i,3]=="55"|sdata[i,3]=="55.0" & sdata[i,2]==j){
        Income_choice[11,j] = 1+Income_choice[11,j]}
    if (sdata[i,3]=="67.5" & sdata[i,2]==j){
        Income_choice[12,j] = 1+Income_choice[12,j]}
    if (sdata[i,3]=="87.5" & sdata[i,2]==j){
        Income_choice[13,j] = 1+Income_choice[13,j]}
    if (sdata[i,3]=="130.0"|sdata[i,3]=="130" & sdata[i,2]==j){
        Income_choice[14,j] = 1+Income_choice[14,j]}
  }
}
print(Income_choice)

#measure the frequency of buying each product for each income level
famsize_choice <- matrix(0,nrow = 8,ncol = 10)
rownames(famsize_choice) <- c("1","2","3","4","5","6","7","8")
colnames(famsize_choice) <- c("PPk_Stk","PBB_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPK_Tub","PFl_Tub","PHse_Tub")
for (i in 1:4470) {
  for (j in 1:10) {
    if (sdata[i,4]=="1" & sdata[i,2]==j){
        famsize_choice[1,j] = 1+famsize_choice[1,j]}
    if (sdata[i,4]=="2" & sdata[i,2]==j){
        famsize_choice[2,j] = 1+famsize_choice[2,j]}
    if (sdata[i,4]=="3" & sdata[i,2]==j){
        famsize_choice[3,j] = 1+famsize_choice[3,j]}
    if (sdata[i,4]=="4" & sdata[i,2]==j){
        famsize_choice[4,j] = 1+famsize_choice[4,j]}
    if (sdata[i,4]=="5" & sdata[i,2]==j){
        famsize_choice[5,j] = 1+famsize_choice[5,j]}
    if (sdata[i,4]=="6" & sdata[i,2]==j){
        famsize_choice[6,j] = 1+famsize_choice[6,j]}
    if (sdata[i,4]=="7" & sdata[i,2]==j){
        famsize_choice[7,j] = 1+famsize_choice[7,j]}
    if (sdata[i,4]=="8" & sdata[i,2]==j){
        famsize_choice[8,j] = 1+famsize_choice[8,j]}
  }
}
print(famsize_choice)
#measure the frequency of buying each product between college and non-college
college_choice <- matrix(0,nrow = 2,ncol = 10)
rownames(college_choice) <- c("college","non-college")
colnames(college_choice) <- c("PPk_Stk","PBB_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPK_Tub","PFl_Tub","PHse_Tub")
for (i in 1:4470) {
  for (j in 1:10) {
    if (sdata[i,5]=="1" & sdata[i,2]==j){
        college_choice[1,j] = 1+college_choice[1,j]}
    if (sdata[i,5]=="0" & sdata[i,2]==j){
        college_choice[2,j] = 1+college_choice[2,j]}
  }
}
print(college_choice)
#measure the frequency of buying each product between white-collar and not white-collar
whtcollar_choice <- matrix(0,nrow = 2,ncol = 10)
rownames(whtcollar_choice) <- c("white-collar","not white_collar")
colnames(whtcollar_choice) <- c("PPk_Stk","PBB_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPK_Tub","PFl_Tub","PHse_Tub")
for (i in 1:4470) {
  for (j in 1:10) {
    if (sdata[i,6]=="1" & sdata[i,2]==j){
        whtcollar_choice[1,j] = 1+whtcollar_choice[1,j]}
    if (sdata[i,6]=="0" & sdata[i,2]==j){
        whtcollar_choice[2,j] = 1+whtcollar_choice[2,j]}
  }
}
print(whtcollar_choice)
#measure the frequency of buying each product between college and non-college
retired_choice <- matrix(0,nrow = 2,ncol = 10)
rownames(retired_choice) <- c("retired","not-retired")
colnames(retired_choice) <- c("PPk_Stk","PBB_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPK_Tub","PFl_Tub","PHse_Tub")
for (i in 1:4470) {
  for (j in 1:10) {
    if (sdata[i,7]=="1" & sdata[i,2]==j){
        retired_choice[1,j] = 1+retired_choice[1,j]}
    if (sdata[i,7]=="0" & sdata[i,2]==j){
        retired_choice[2,j] = 1+retired_choice[2,j]}
  }
}
print(retired_choice)