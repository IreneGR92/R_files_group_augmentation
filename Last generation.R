rm(list=ls())
library(data.table)
getwd()
setwd('H:\\PhD\\CODE\\All_results\\txt_files\\last_generation')
#setwd('H:\\PhD\\CODE\\Group_augmentation_Cplusplus\\Group_augmentation_RN\\Group_augmentation_RN')
#setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\txt_files\\last_generation')
#setwd('C:\\Users\\igaru\\Documents\\GitHub\\Group_augmentation_Cplusplus\\Group_augmentation_RN\\Group_augmentation_RN')
GA2<-read.table("group_augmentation_last_generation_.txt",header = TRUE, skip=27)
GA2 <- subset(GA2, age>0)
setDF(GA2)


#head(GA2)
#str(GA2)
#View(GA2)
#summary(GA2)

##FORMULAS

replace_with_zero_if_below_zero <- function(x) {
  x <- ifelse(x<0,0,x)
  return(x)
}

###Help

help_Formula<-function(){
  help <- GA2$alpha+GA2$alphaAge*GA2$age+GA2$alphaAge2*GA2$age*GA2$age
  help <- sapply(help, replace_with_zero_if_below_zero)
  return(help)}
 
GA2$Help <- help_Formula()
GA2[GA2$type==0,]$Help<-NA
    

###Dispersal
dispersal_Formula<-function(){
  dispersal<-1 / (1 + exp(GA2$betaAge*GA2$age - GA2$beta))
  return(dispersal)}

GA2$Dispersal<-dispersal_Formula()
GA2[GA2$type==0,]$Dispersal<-NA

##Age
mean_age<-mean(GA2$age)


dichotonomic_age <- function(x) {
  x <- ifelse(x<mean_age,"< mean age","> mean age")
  return(x)
}

GA2$AgeDic<-dichotonomic_age(GA2$age)
GA2$AgeDic<-as.factor(GA2$AgeDic)

##PLOTS

plot(GA2$Help, GA2$Dispersal, col=c("blue","green")[GA2$AgeDic],  xlab="Help", ylab="Dispersal") 
legend(x="bottomright", legend = levels(GA2$AgeDic), col=c("blue","green"), pch=1)
title("Dispersal vs Help")

# library(e1071); library(ggplot2)
# qplot(Help, Dispersal, colour = AgeDic, shape = AgeDic, 
#       data = GA2)

