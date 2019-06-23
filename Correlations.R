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
str(results)


#sub<-subset(results, Help >1.75)
#View(sub)


######################################### T TEST  #####################################################3

t.test(results$Help~results$Bias)
t.test(results$Dispersal~results$Bias)
t.test(results$Relatedness~results$Bias)
t.test(results$Group_size~results$Bias)
t.test(results$Survival~results$Bias)

t.test(results$Help~results$X0)
t.test(results$Dispersal~results$X0)
t.test(results$Relatedness~results$X0)

t.test(results$Help~results$Xh)
t.test(results$Dispersal~results$Xh)
t.test(results$Relatedness~results$Xh)

t.test(results$Help~results$K1)
t.test(results$Dispersal~results$K1)
t.test(results$Relatedness~results$K1)


res.aov2 <- aov(Help ~ Xn, data = results)
summary(res.aov2)
pairwise.t.test(results$Help, results$Xn, p.adjust="none", pool.sd = T) 

res.aov2 <- aov(Dispersal ~ Xn, data = results)
summary(res.aov2)
pairwise.t.test(results$Dispersal, results$Xn, p.adjust="none", pool.sd = T) 

res.aov2 <- aov(Relatedness ~ Xn, data = results)
summary(res.aov2)
pairwise.t.test(results$Relatedness, results$Xn, p.adjust="none", pool.sd = T) 

res.aov2 <- aov(Group_size ~ Xn, data = results)
summary(res.aov2)
pairwise.t.test(results$Group_size, results$Xn, p.adjust="none", pool.sd = T) 

res.aov2 <- aov(Survival ~ Xn, data = results)
summary(res.aov2)
pairwise.t.test(results$Survival, results$Xn, p.adjust="none", pool.sd = T) 




###########################################   BAR GRAPHS   #####################################################################


#Function to calculate man and SD per variable per parameter
library(plyr)
library(tidyselect)

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
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Bias, y=Help, width=0.5)) +
  geom_bar(stat="identity", position=position_dodge(), fill="red")+
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()




help_summary<-data_summary(results, varname="Group_size", groupnames=c("Bias"))
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Bias, y=Group_size, width=0.5)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Group_size-se, ymax=Group_size+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  ylab("Group size")


help_summary<-data_summary(results, varname="Survival", groupnames=c("Bias"))
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Bias, y=Survival, width=0.5)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Survival-se, ymax=Survival+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  coord_cartesian(ylim = c(0.049, 1))




#Help vs X0

help_summary<-data_summary(results, varname="Help", groupnames=c("X0")) 
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=X0, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Base survival")


#Help vs Xh

help_summary<-data_summary(results, varname="Help", groupnames=c("Xh")) 
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Xh, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Cost help survival")



#Help vs Xn

help_summary<-data_summary(results, varname="Help", groupnames=c("Xn")) 
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Xn, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit group size survival")



help_summary<-data_summary(results, varname="Group_size", groupnames=c("Xn")) 
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Xn, y=Group_size, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Group_size-se, ymax=Group_size+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit group size survival")+
  ylab("Group size")



help_summary<-data_summary(results, varname="Survival", groupnames=c("Xn")) 
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=Xn, y=Survival, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Survival-se, ymax=Survival+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit group size survival")





#Help vs K1

help_summary<-data_summary(results, varname="Help", groupnames=c("K1")) 
help_summary$se<-SE_formula(help_summary$sd,10)

ggplot(help_summary, aes(x=K1, y=Help, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  geom_errorbar(aes(ymin=Help-se, ymax=Help+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit help fecundity")



##############  DISPERSAL ######################

#Dispersal vs Bias

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("Bias")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Bias, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="blue") +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  coord_cartesian(ylim = c(0.049, 1))


#Dispersal vs X0

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("X0")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=X0, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="blue") +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Base survival")+
  coord_cartesian(ylim = c(0.049, 1))


#Dispersal vs Xh

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("Xh")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xh, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="blue") +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Cost help survival")+
  coord_cartesian(ylim = c(0.049, 1))



#Dispersal vs Xn

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("Xn")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xn, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="blue") +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit group size survival")+
  coord_cartesian(ylim = c(0.049, 1))



#Dispersal vs K1

summary_results<-data_summary(results, varname="Dispersal", groupnames=c("K1")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=K1, y=Dispersal, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="blue") +
  geom_errorbar(aes(ymin=Dispersal-se, ymax=Dispersal+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit help fecundity")+
  coord_cartesian(ylim = c(0.049, 1))



##############  RELATEDNESS ######################

#Relatedness vs Bias

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("Bias")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Bias, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="orange") +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Relatedness vs X0

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("X0")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=X0, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="orange") +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Base survival")



#Relatedness vs Xh

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("Xh")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xh, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="orange") +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Cost help survival")



#Relatedness vs Xn

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("Xn")) 
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=Xn, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="orange") +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit group size survival")



#Relatedness vs K1

summary_results<-data_summary(results, varname="Relatedness", groupnames=c("K1"))
str(summary_results)
summary_results$se<-SE_formula(summary_results$sd,10)

ggplot(summary_results, aes(x=K1, y=Relatedness, width=0.5)) + 
  geom_bar(stat="identity", position=position_dodge(), fill="orange") +
  geom_errorbar(aes(ymin=Relatedness-se, ymax=Relatedness+se), width=0.1,
                position=position_dodge(0.9))+
  scale_fill_brewer(palette="Paired") + theme_minimal()+
  xlab("Benefit help fecundity")





########################################   CORRELATION GRAPHS   ####################################################3

library(ggpubr)
library("ggplot2")
library("gtable")
library("grid")
library("gridExtra")



g1<-ggscatter(results, x = "Relatedness", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.25),
              xlab = "Relatedness", ylab = "Help")


g2<-ggscatter(results, x = "Relatedness", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.25),
              xlab = "Relatedness", ylab = "Dispersal")
g2<-ggpar(g2, ylim = c(0.0, 1))


g3<-ggscatter(results, x = "Help", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x.npc = "right",label.x = 3, label.y.npc = "bottom"),
              xlab = "Help", ylab = "Dispersal")
g3<-ggpar(g3, ylim = c(0.0, 1))


g4<-ggscatter(results, x = "Help", y = "Group_size", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 3.5),
              xlab = "Help", ylab = "Group size")


g5<-ggscatter(results, x = "Dispersal", y = "Group_size", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0.6),
              xlab = "Dispersal", ylab = "Group size")
g5<-ggpar(g5, xlim = c(0, 1))


g6<-ggscatter(results, x = "Relatedness", y = "Group_size", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Relatedness", ylab = "Group size")

g7<-ggscatter(results, x = "Relatedness", y = "Help_Disp", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              cor.coeff.args = list(label.x = 0),
              xlab = "Relatedness", ylab = "Help-Dispersal")

g8<-ggscatter(results, x = "Survival", y = "Help", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Survival", ylab = "Help")

g9<-ggscatter(results, x = "Survival", y = "Dispersal", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",
              xlab = "Survival", ylab = "Dispersal")




grid.arrange(g1, g2, g3, g4, g5, g6, g8, g9, nrow = 3)
