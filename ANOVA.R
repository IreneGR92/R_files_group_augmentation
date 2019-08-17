rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
#setwd('~/Documents/Model/excel_files')

results<-read.table("Results_default.csv",header = TRUE, sep=",")
names(results)[names(results) == 'ï..Replica'] <- 'Replica'


results$X0<-as.factor(results$X0)
results$Xh<-as.factor(results$Xh)
results$Xn<-as.ordered(results$Xn)
results$K1<-as.factor(results$K1)
results$Bias<-as.factor(results$Bias)

resultsH<-subset(results,  Num_helpers>1)

# results$Bias<-as.numeric(results$Bias)
# resultsH<-subset(results,  Bias<10)
# mean(resultsH$Bias)
# results$Bias<-as.factor(results$Bias)
# resultsH$Bias<-as.factor(resultsH$Bias)


#head(results)
str(results)
#View(esmd)
#summary(results)

results<-subset(results,  Implementation=="Normal")
results<-subset(results,  Evol_help=="yes")
results<-subset(results, Help<4)

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




#Interaction plot using summary statistics

library("ggpubr")

resultsH$Num_helpers<-round(results$Num_helpers, digits=0)
resultsH$Num_helpers<-as.ordered(results$Num_helpers)

ggboxplot(resultsH, x = "Num_helpers", y = "Help", color = "X0",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "Num_helpers", y = "Help", color = "Xh",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "Num_helpers", y = "Help", color = "K1",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "Num_helpers", y = "Help", color = "Bias",
          palette = c("#00AFBB", "#E7B800"))




ggboxplot(resultsH, x = "Bias", y = "Help", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "X0", y = "Help", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "Xn", y = "Help", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "Xh", y = "Help", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(resultsH, x = "K1", y = "Help", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

results_ANOVA_Help <- aov(Help ~ X0 + Xh + Xn + K1 + Bias + Reaction_norm, data = resultsH)
summary(results_ANOVA_Help)



ggboxplot(results, x = "Bias", y = "Dispersal", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "X0", y = "Dispersal", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "Xn", y = "Dispersal", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "Xh", y = "Dispersal", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(results, x = "K1", y = "Dispersal", color = "Reaction_norm",
          palette = c("#00AFBB", "#E7B800"))

results_ANOVA_Dispersal <- aov(Dispersal ~ X0 + Xh + Xn + K1 + Bias + Reaction_norm, data = results)
summary(results_ANOVA_Dispersal)

results_ANOVA_Relatedness <- aov(Relatedness ~ X0 + Xh + Xn + K1 + Bias + Reaction_norm, data = results)
summary(results_ANOVA_Relatedness)



############################## MORE GRAPHS #####################################

library(ggplot2)

ggplot(results, aes(x = Bias, y = Dispersal)) +
  geom_boxplot(aes(fill = Evol_help)) +
  scale_fill_manual(values = c("#d11141", "#ffc425"))+
  labs(fill ="Evolution of help?") +
  dark_theme_gray(base_size = 14)


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


df2 <- data_summary(results, varname="Dispersal", 
                    groupnames=c("Bias", "Evol_help"))



ggplot(df2, aes(x = Bias, y = Dispersal, group = Evol_help, color=Evol_help)) + 
  geom_line(size=1.2) +
  geom_point()+
  geom_errorbar(aes(ymin=Dispersal-sd, ymax=Dispersal+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0, 1))+
  scale_color_manual(values=c('#0000CC','#009999'))+
  dark_theme_gray(base_size = 14)



ggboxplot(results, x = "Reaction_norm", y = "Dispersal")

ggplot(results, aes(x = Reaction_norm, y = Dispersal)) +
  geom_boxplot(fill="blue") +
  xlab("Reaction norm to age")+
  #coord_cartesian(ylim = c(0, 1))+
  dark_theme_gray(base_size = 14)

ggplot(results, aes(x = Reaction_norm, y = Help)) +
  geom_boxplot(fill="red") +
  xlab("Reaction norm to age")+
  coord_cartesian(ylim = c(0, 2))+
  dark_theme_gray(base_size = 14)

