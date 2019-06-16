rm(list=ls())
getwd()
#setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\Excel_files')
results<-read.table("SEM.csv",header = TRUE, sep=",")



###########################################   BAR GRAPHS   #####################################################################


#Function to calculate man and SD per variable per parameter
library(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

SE_formula<-function(sd, n){
  SE<-sd / sqrt(n)
  return(SE)
}



library(ggplot2)

##############  HELP ######################

#Help vs Bias

help_summary<-data_summary(results, varname="Help", groupnames=c("Bias"))
str(help_summary)
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Bias, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Help vs X0

help_summary<-data_summary(results, varname="Help", groupnames=c("X0")) 
str(help_summary)
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=X0, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Help vs Xh

help_summary<-data_summary(results, varname="Help", groupnames=c("Xh")) 
str(help_summary)
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Xh, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



#Help vs Xn

help_summary<-data_summary(results, varname="Help", groupnames=c("Xn")) 
str(help_summary)
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Xn, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



#Help vs K1

help_summary<-data_summary(results, varname="Help", groupnames=c("K1")) 
str(help_summary)
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=K1, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



##############  DISPERSAL ######################

#Dispersal vs Bias

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("Bias")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Bias, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Dispersal vs X0

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("X0")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=X0, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Dispersal vs Xh

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("Xh")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xh, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



#Dispersal vs Xn

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("Xn")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xn, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



#Dispersal vs K1

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("K1")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=K1, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



##############  RELATEDNESS ######################

#Relatedness vs Bias

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("Bias")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Bias, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Relatedness vs X0

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("X0")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=X0, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Relatedness vs Xh

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("Xh")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xh, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



#Relatedness vs Xn

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("Xn")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xn, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()



#Relatedness vs K1

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("K1"))
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=K1, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()





########################################   CORRELATION GRAPHS   ####################################################3

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



grid.arrange(g1, g2, g3, g4, g5, g6, g7, nrow = 4)