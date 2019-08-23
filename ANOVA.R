rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
#setwd('~/Documents/Model/excel_files')

library(readxl)
results_original <- read_excel("NRN-Low_survival_floaters.xlsx")

results_original$X0<-as.factor(results_original$X0)
results_original$Xh<-as.factor(results_original$Xh)
results_original$Xn<-as.ordered(results_original$Xn)
results_original$K1<-as.factor(results_original$K1)
results_original$Bias<-as.factor(results_original$Bias)
results_original$Evol_help<-as.factor(results_original$Evol_help)
results_original$Reaction_norm<-as.factor(results_original$Reaction_norm)
#results_original$Implementation<-as.factor(results_original$Implementation)

results<-results_original

#head(results)
str(results)
#View(esmd)
#summary(results)

results<-subset(results,  Implementation=="Normal")
results<-subset(results,  Evol_help=="yes")
resultsH<-subset(results,  Num_helpers>1)
resultsH<-subset(resultsH, Help<4)

#Means and summary statistics by group

library(Rmisc)
library(lattice)
library(plyr)
library(ggdark)

#Help
sumAll_H <- summarySE(resultsH, 
                measurevar="Help", 
                groupvars=c("X0","Xh","Xn","K1","Bias"))

sumX0_H <- summarySE(resultsH, 
                 measurevar="Help", 
                 groupvars=c("X0"))

sumXh_H <- summarySE(resultsH, 
                 measurevar="Help", 
                 groupvars=c("Xh"))

sumXn_H <- summarySE(resultsH, 
                 measurevar="Help", 
                 groupvars=c("Xn"))

sumK1_H <- summarySE(resultsH, 
                 measurevar="Help", 
                 groupvars=c("K1"))

sumBias_H <- summarySE(resultsH, 
                 measurevar="Help", 
                 groupvars=c("Bias"))

sumRN_H <- summarySE(resultsH, 
                       measurevar="Help", 
                       groupvars=c("Reaction_norm"))

sumX0_H
sumXh_H
sumXn_H
sumK1_H
sumBias_H
sumRN_H



#Dispersal

sumAll_D <- summarySE(results, 
                      measurevar="Dispersal", 
                      groupvars=c("X0","Xh","Xn","K1","Bias"))

sumX0_D <- summarySE(results, 
                     measurevar="Dispersal", 
                     groupvars=c("X0"))

sumXh_D <- summarySE(results, 
                     measurevar="Dispersal", 
                     groupvars=c("Xh"))

sumXn_D <- summarySE(results, 
                     measurevar="Dispersal", 
                     groupvars=c("Xn"))

sumK1_D <- summarySE(results, 
                     measurevar="Dispersal", 
                     groupvars=c("K1"))

sumBias_D <- summarySE(results, 
                       measurevar="Dispersal", 
                       groupvars=c("Bias"))

sumRN_D <- summarySE(results, 
                     measurevar="Dispersal", 
                     groupvars=c("Reaction_norm"))

sumX0_D
sumXh_D
sumXn_D
sumK1_D
sumBias_D
sumRN_D


#Relatedness
sumAll_R <- summarySE(results, 
                      measurevar="Relatedness", 
                      groupvars=c("X0","Xh","Xn","K1","Bias"))

sumX0_R <- summarySE(results, 
                     measurevar="Relatedness", 
                     groupvars=c("X0"))

sumXh_R <- summarySE(results, 
                     measurevar="Relatedness", 
                     groupvars=c("Xh"))

sumXn_R <- summarySE(results, 
                     measurevar="Relatedness", 
                     groupvars=c("Xn"))

sumK1_R <- summarySE(results, 
                     measurevar="Relatedness", 
                     groupvars=c("K1"))

sumBias_R <- summarySE(results, 
                       measurevar="Relatedness", 
                       groupvars=c("Bias"))

sumX0_R
sumXh_R
sumXn_R
sumK1_R
sumBias_R





#ANOVA

results_ANOVA_Help <- aov(Help ~ X0 + Xh + Xn + K1 + Bias, data = resultsH)
summary(results_ANOVA_Help)

results_ANOVA_Dispersal <- aov(Dispersal ~ X0 + Xh + Xn + K1 + Bias, data = results)
summary(results_ANOVA_Dispersal)

results_ANOVA_Relatedness <- aov(Relatedness ~ X0 + Xh + Xn + K1 + Bias, data = results)
summary(results_ANOVA_Relatedness)

#Tukey multiple pairwise-comparisons

TukeyHSD(results_ANOVA_Dispersal, which = "Xn")




