rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\txt_files\\15.04.19\\NRN\\Excels')
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

sum <- summarySE(results, 
                measurevar="Help", 
                groupvars=c("X0","Xh","Xn","K1","Bias"))
sum


#Interaction plot using summary statistics

library("ggpubr")
ggboxplot(results, x = "Xn", y = "Help", color = "X0",
          palette = c("#00AFBB", "#E7B800"))


#ANOVA

results_ANOVA <- aov(Help ~ X0 + Xh + Xn + K1, data = results)
summary(results_ANOVA)

#Tukey multiple pairwise-comparisons

TukeyHSD(results_ANOVA, which = "Xn")


