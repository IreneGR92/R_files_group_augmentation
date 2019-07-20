rm(list=ls())
getwd()
#setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\Excel_files')
#setwd('~/Documents/Model/excel_files')

results<-read.table("SEM-NRN.csv",header = TRUE, sep=",")
names(results)[names(results) == 'ï..Replica'] <- 'Replica'


results$X0<-as.factor(results$X0)
results$Xh<-as.factor(results$Xh)
results$Xn<-as.ordered(results$Xn)
results$K1<-as.factor(results$K1)
results$Bias<-as.factor(results$Bias)
str(results)

resultsH<-subset(results,  Num_helpers>1)
resultsH<-subset(resultsH,  Help<4.1)
resultsH<-subset(resultsH,  CumHelp<4.1)

#sub<-subset(results, Help >1.75)
#View(sub)




########################################   CORRELATION GRAPHS   ####################################################3

library(ggpubr)
library("ggplot2")
library("gtable")
library("grid")
library("gridExtra")

invert_geom_defaults()


g1<-ggscatter(resultsH, x = "Relatedness", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.2),
              xlab = "Relatedness", ylab = "Help")
g1

g2<-ggscatter(results, x = "Relatedness", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.18),
              xlab = "Relatedness", ylab = "Dispersal")
g2<-ggpar(g2, ylim = c(0.0, 1))
g2

g3<-ggscatter(resultsH, x = "Help", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0),
              #cor.coeff.args = list(label.x.npc = "right",label.x = 3, label.y.npc = "bottom"),
              xlab = "Help", ylab = "Dispersal")
g3<-ggpar(g3, ylim = c(0.0, 1))
g3


g4<-ggscatter(resultsH, x = "Num_helpers", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 3.5),
              xlab = "Number of helpers", ylab = "Help")


g4.1<-ggscatter(resultsH, x = "Num_helpers", y = "CumHelp", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 4.5),
              xlab = "Number of helpers", ylab = "Cumulative level of help")



g5<-ggscatter(results, x = "Num_helpers", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 3.5),
              xlab = "Number of helpers", ylab = "Dispersal")
g5<-ggpar(g5, ylim = c(0, 1))
g5


g6<-ggscatter(results, x = "Relatedness", y = "Num_helpers", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Number of helpers")

g7<-ggscatter(resultsH, x = "Relatedness", y = "Help_Disp", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.2),
              xlab = "Relatedness", ylab = "Help-Dispersal")

g8<-ggscatter(resultsH, x = "Survival", y = "Help", 
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

resultsH$propHelperB<-1-resultsH$propFloaterB

ggscatter(resultsH, x = "propHelperB", y = "Help", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(label.x = 0.6),
          xlab = "Proportion of helpers that become breeders", ylab = "Help")

ggscatter(resultsH, x = "propFloaterB", y = "Help_Disp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(label.x = 0.2),
          xlab = "Proportion of floaters that become breeders", ylab = "Help_Disp")


grid.arrange(g1, g2, g3, g4, g5, g6, g7,g8, g9,g10, nrow = 5)



ggscatter(results, x = "Age", y = "Dispersal", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(label.x = 6.5),
          xlab = "Age", ylab = "Dispersal")

ggscatter(resultsH, x = "Age", y = "Help", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(label.x = 6.5),
          xlab = "Age", ylab = "Help")

g11<-ggscatter(results, x = "Age", y = "Relatedness", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               cor.coeff.args = list(label.x = 3),
               xlab = "Age", ylab = "Relatedness")
g11<-ggpar(g11, ylim = c(0.05, 0.55))
g11
