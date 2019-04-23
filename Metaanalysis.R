rm(list=ls())

library(Matrix)
library(metafor)

setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\txt_files\\15.04.19\\NRN')
results<-read.table("Metaanalysis.csv",header = TRUE, sep=",")
names(results)[names(results) == 'ï..ID'] <- 'ID'

#head(results)
str(results)
#View(results)
#summary(results)

results$X0<-as.factor(results$X0)
results$Xh<-as.factor(results$Xh)
results$Xn<-as.factor(results$Xn)
results$K1<-as.factor(results$K1)
results$Bias<-as.factor(results$Bias)


#Calculate Effect Sizes based on continious variables
esmd <- escalc(measure="MD", Help=Help, Dispersal=Dispersal, Relatedness=Relatedness, GroupSize=GroupSize, HelpDisp=HelpDisp,
               SD_Help=SD_Help, SD_Dispersal=SD_Dispersal, SD_Relatedness=SD_Relatedness, SD_GroupSize=SD_GroupSize, SD_HelpDisp=SD_HelpDisp, data=results)


