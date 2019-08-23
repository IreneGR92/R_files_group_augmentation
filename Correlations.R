rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
#setwd('~/Documents/Model/excel_files')

library(readxl)
results_original <- read_excel("NRN-RN.xlsx")


#results<-subset(results, Reaction_norm=="RN")

results_original$X0<-as.factor(results_original$X0)
results_original$Xh<-as.factor(results_original$Xh)
results_original$Xn<-as.ordered(results_original$Xn)
results_original$K1<-as.factor(results_original$K1)
results_original$Bias<-as.factor(results_original$Bias)
results_original$Reaction_norm<-as.factor(results_original$Reaction_norm)

str(results_original)

results<-results_original
results<-subset(results, Reaction_norm %in% c("NRN") )
resultsH<-subset(results,  Num_helpers>1)

resultsH<-subset(resultsH,  Help<4.1)
#resultsH<-subset(resultsH,  CumHelp<4.1)



########################################   CORRELATION GRAPHS   ####################################################3

library("ggpubr")
library("ggplot2")
library("gtable")
library("grid")
library("gridExtra")
library("ggExtra")
library("magrittr")
library(ggdark)

#invert_geom_defaults()


g1<-ggscatter(resultsH, x = "Relatedness", y = "Help", 
              color = "Reaction_norm",  palette =  c("#FF0000", "#FF9900","#FF9900"),
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.2, label.y = 1.1),
              xlab = "Relatedness", ylab = "Help")+ dark_theme_gray(base_size = 14)
g1              


g2<-ggscatter(results, x = "Relatedness", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              cor.coeff.args = list(label.x = 0.18),
              xlab = "Relatedness", ylab = "Dispersal")
g2<-ggpar(g2, ylim = c(0.0, 1))
g2

g3<-ggscatter(resultsH, x = "Help", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              cor.coeff.args = list(label.x = 0),
              #cor.coeff.args = list(label.x.npc = "right",label.x = 3, label.y.npc = "bottom"),
              xlab = "Help", ylab = "Dispersal")
g3<-ggpar(g3, ylim = c(0.0, 1))
g3


g4<-ggscatter(resultsH, x = "Num_helpers", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",  palette = c("#FF0000", "#FF9900"),
              cor.coeff.args = list(label.x = 7, label.y = 1.1),
              xlab = "Number of helpers", ylab = "Help")+
              dark_theme_gray(base_size = 16)
g4

g4.1<-ggscatter(resultsH, x = "Num_helpers", y = "CumHelp", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",  palette = c("#FF0000", "#FF9900"),
              cor.coeff.args = list(label.x = 7, label.y = 3.5),
              xlab = "Number of helpers", ylab = "Cumulative level of help")+
              dark_theme_gray(base_size = 16)
g4.1


g5<-ggscatter(results, x = "Num_helpers", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              cor.coeff.args = list(label.x = 3.5),
              xlab = "Number of helpers", ylab = "Dispersal")
g5<-ggpar(g5, ylim = c(0, 1))
g5


g6<-ggscatter(results, x = "Relatedness", y = "Num_helpers", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              xlab = "Relatedness", ylab = "Number of helpers")
g6

g7<-ggscatter(resultsH, x = "Relatedness", y = "Help_Disp", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              #cor.coeff.args = list(label.x = 0.2),
              xlab = "Relatedness", ylab = "Help-Dispersal")
g7

g8<-ggscatter(resultsH, x = "Survival", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              xlab = "Survival", ylab = "Help")
g8

g9<-ggscatter(results, x = "Survival", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              color = "Reaction_norm",
              xlab = "Survival", ylab = "Dispersal")
g9

g10<-ggscatter(results, x = "Survival", y = "Relatedness", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               color = "Reaction_norm",
               xlab = "Survival", ylab = "Relatedness")
g10

resultsH$propHelperB<-1-resultsH$propFloaterB

ggscatter(resultsH, x = "propHelperB", y = "Help", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          #color = "Reaction_norm",  palette = c("#FF0000", "#FF9900"),
          cor.coeff.args = list(label.x = 0.6),
          xlab = "Proportion of helpers that become breeders", ylab = "Help") +
          dark_theme_gray(base_size = 16)


#grid.arrange(g1, g2, g3, g4, g5, g6, g7,g8, g9,g10, nrow = 5)



ggscatter(results, x = "Age", y = "Dispersal", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          color = "Reaction_norm", palette = c("#0000CC", "#33CCFF"),
          cor.coeff.args = list(label.x = 6.5),
          xlab = "Age", ylab = "Dispersal") + dark_theme_gray(base_size = 14)

ggscatter(resultsH, x = "Age", y = "Help", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          color = "Reaction_norm", palette = c("#FF0000", "#FF6600"),
          cor.coeff.args = list(label.x = 7),
          xlab = "Age", ylab = "Help")+dark_theme_gray(base_size = 14)

g11<-ggscatter(results, x = "Age", y = "Relatedness", 
               add = "reg.line", conf.int = TRUE, 
               cor.coef = TRUE, cor.method = "spearman",
               color = "Reaction_norm",  palette = c("#FF9900", "#FFFF00"),
               cor.coeff.args = list(label.x = 3, label.y=0.52),
               xlab = "Age", ylab = "Relatedness")+ dark_theme_gray(base_size = 14)
g11<-ggpar(g11, ylim = c(0.05, 0.55))
g11


#Correlations with boxplots

ggMarginal(g1, type = "boxplot")
ggMarginal(g2, type = "boxplot")
ggMarginal(g3, type = "boxplot")
ggMarginal(g4, type = "boxplot")
ggMarginal(g5, type = "boxplot")
ggMarginal(g6, type = "boxplot")
ggMarginal(g7, type = "boxplot")
ggMarginal(g8, type = "boxplot")
ggMarginal(g9, type = "boxplot")
ggMarginal(g10, type = "boxplot")
ggMarginal(g11, type = "boxplot")

