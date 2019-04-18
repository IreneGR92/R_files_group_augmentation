rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
#setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\Excel_files')
results<-read.table("Correlations-NRN.csv",header = TRUE, sep=";")


library(ggpubr)
library("ggplot2")
library("gtable")
library("grid")
library("gridExtra")



g1<-ggscatter(results, x = "RELATEDNESS", y = "HELP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Relatedness", ylab = "Help")


g2<-ggscatter(results, x = "RELATEDNESS", y = "DISPERSAL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Relatedness", ylab = "Dispersal")
g2<-ggpar(g2, ylim = c(0.0, 1))


g3<-ggscatter(results, x = "HELP", y = "DISPERSAL", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Help", ylab = "Dispersal")
g3<-ggpar(g3, ylim = c(0.0, 1))


g4<-ggscatter(results, x = "HELP", y = "GROUP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Help", ylab = "Group size")


g5<-ggscatter(results, x = "GROUP", y = "DISPERSAL", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Group size", ylab = "Dispersal")
g5<-ggpar(g5, ylim = c(0, 1))


g6<-ggscatter(results, x = "RELATEDNESS", y = "GROUP", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Group size")

g7<-ggscatter(results, x = "RELATEDNESS", y = "HELPDISP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Relatedness", ylab = "Help-Dispersal")



grid.arrange(g1, g2, g3, g4, g5, g6, g7, nrow = 4)