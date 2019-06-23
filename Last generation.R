rm(list=ls())

library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(formattable)
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')
library(rJava)
library(xlsx)
library(data.table)
library(dplyr)
library(tidyselect)
library(ggpubr)

#directory<-"~/Documents/Model/Results/" #Linux
#directory<-"H:\\PhD\\CODE\\All_results\\txt_files\\16.06.19\\NRN\\"  #Work 
#directory<-"C:\\Users\\ig17c521\\Documents\\Group-augmentation-Cplusplus\\results\\"  #Work
setwd("C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\txt_files\\NRN\\last_generation")  #Home
GA2<-read.table("group_augmentation_last_generation_X05-Xh02-Xn04.txt",header = TRUE, skip=30)
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

