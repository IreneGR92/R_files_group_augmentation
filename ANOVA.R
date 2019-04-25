rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\txt_files\\15.04.19\\NRN\\Results')
#setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\Excel_files')
results<-read.table("ANOVA.csv",header = TRUE, sep=";")

#head(results)
str(results)
#View(esmd)
#summary(results)

results$X0<-as.factor(results$X0)
results$Xh<-as.factor(results$Xh)
results$Xn<-as.factor(results$Xn)
results$K1<-as.factor(results$K1)
results$Bias<-as.factor(results$Bias)

#Means and summary statistics by group

library(Rmisc)
library(lattice)
library(plyr)

#Help
sumAll_H <- summarySE(results, 
                measurevar="Help", 
                groupvars=c("X0","Xh","Xn","K1","Bias"))

sumX0_H <- summarySE(results, 
                 measurevar="Help", 
                 groupvars=c("X0"))

sumXh_H <- summarySE(results, 
                 measurevar="Help", 
                 groupvars=c("Xh"))

sumXn_H <- summarySE(results, 
                 measurevar="Help", 
                 groupvars=c("Xn"))

sumK1_H <- summarySE(results, 
                 measurevar="Help", 
                 groupvars=c("K1"))

sumBias_H <- summarySE(results, 
                 measurevar="Help", 
                 groupvars=c("Bias"))

sumAll
sumX0
sumXh
sumXn
sumK1
sumBias



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
sumAll_D
sumX0_D
sumXh_D
sumXn_D
sumK1_D
sumBias_D


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

sumAll_R
sumX0_R
sumXh_R
sumXn_R
sumK1_R
sumBias_R



#Interaction plot using summary statistics

library("ggpubr")
ggboxplot(results, x = "Xn", y = "Help", color = "X0",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "Xn", y = "Help", color = "Xh",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "Xn", y = "Help", color = "K1",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "Xn", y = "Help", color = "Bias",
          palette = c("#00AFBB", "#E7B800"))


#ANOVA

results_ANOVA_Help <- aov(Help ~ X0 + Xh + Xn + K1 + Bias, data = results)
summary(results_ANOVA_Help)

results_ANOVA_Dispersal <- aov(Dispersal ~ X0 + Xh + Xn + K1 + Bias, data = results)
summary(results_ANOVA_Dispersal)

results_ANOVA_Relatedness <- aov(Relatedness ~ X0 + Xh + Xn + K1 + Bias, data = results)
summary(results_ANOVA_Relatedness)

#Tukey multiple pairwise-comparisons

TukeyHSD(results_ANOVA, which = "Xn")


