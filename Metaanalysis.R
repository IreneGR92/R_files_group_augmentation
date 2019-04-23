rm(list=ls())

library(Matrix)
library(meta)
library(metafor)

#setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\txt_files\\15.04.19\\NRN')
setwd("H:\\PhD\\CODE\\All_results\\Excel_files")
#results<-read.table("Metaanalysis.csv",header = TRUE, sep=",")
results<-read.table("test.csv",header = TRUE, sep=";")
#names(results)[names(results) == 'ï..ID'] <- 'ID'

#head(results)
str(results)
#View(esmd)
#summary(results)

# results$X0<-as.factor(results$X0)
# results$Xh<-as.factor(results$Xh)
# results$Xn<-as.factor(results$Xn)
# results$K1<-as.factor(results$K1)
# results$Bias<-as.factor(results$Bias)


#Calculate Effect Sizes based on continuous variables

esmd <- escalc(measure="MD", m1i=m1i, m2i=m2i, sd1i=sd1i, sd2i=sd2i, n1i=n1i, n2i=n2i, data=results)
##This calculates "yi" (Effect Sizes) and vi (sampling variances) 


# Fixed Effects model GLM

FEmodel<-rma(yi=yi, vi=vi, data=esmd, method="FE") #FE:Fixed Effect
FEmodel
##negative z value means that we mostly had negative relationships (correlation coefficients)
##significant p-value for "Test for Heterogeneity" or "Cochran's Q-test is a bad thing, because it provides evidence that there is heterogeneity of intervention effects, or biases.

REmodel<-rma(yi=yi, vi=vi, data=esmd, method="REML")#REML:Random Effects 
FEmodel
##tau^2" represents the amount of (residual) heterogeneity in a random effects model. 
##"I^2" estimates how much of the total variability in the effect size estimates is attributed to heterogeneity. 
##"H^2" estimates the ratio of the amount of variability in the effect sizes to the amount of sampling variability. 


#Graphs

forest(REmodel)

funnel(REmodel)





#ANOVA

library(rpsychi)

with(q2data.frame, ind.oneway.second(mean,sd,n) )




