rm(list=ls())
getwd()
#setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\Excel_files')
#setwd('~/Documents/Model/excel_files')

results<-read.table("SEM.csv",header = TRUE, sep=",")
names(results)[names(results) == 'ï..Replica'] <- 'Replica'


results$X0<-as.factor(results$X0)
results$Xh<-as.factor(results$Xh)
results$Xn<-as.ordered(results$Xn)
results$K1<-as.factor(results$K1)
results$Bias<-as.factor(results$Bias)


#head(results)
str(results)
#View(esmd)
#summary(results)

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

sumAll_H
sumX0_H
sumXh_H
sumXn_H
sumK1_H
sumBias_H



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




#Correlations

library(ggpubr)
library("ggplot2")
library("gtable")
library("grid")
library("gridExtra")



g1<-ggscatter(results, x = "Relatedness", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Help")


g2<-ggscatter(results, x = "Relatedness", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Dispersal")
g2<-ggpar(g2, ylim = c(0.0, 1))


g3<-ggscatter(results, x = "Help", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Help", ylab = "Dispersal")
g3<-ggpar(g3, ylim = c(0.0, 1))


g4<-ggscatter(results, x = "Help", y = "Group_size", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Help", ylab = "Group size")


g5<-ggscatter(results, x = "Group_size", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Group size", ylab = "Dispersal")
g5<-ggpar(g5, ylim = c(0, 1))


g6<-ggscatter(results, x = "Relatedness", y = "Group_size", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Group size")

g7<-ggscatter(results, x = "Relatedness", y = "Help_Disp", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Help-Dispersal")

g8<-ggscatter(results, x = "Survival", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Survival", ylab = "Help")

g9<-ggscatter(results, x = "Survival", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Survival", ylab = "Dispersal")

g10<-ggscatter(results, x = "Survival", y = "Relatedness", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Survival", ylab = "Relatedness")


grid.arrange(g1, g2, g3, g4, g5, g6, g7,g8, g9,g10, nrow = 5)