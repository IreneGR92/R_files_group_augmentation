rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results\\Excel_files')
#setwd('~/smb4k/IEEESHFS01.UNIBE.CH/homes/ig17c521/PhD/CODE/All_results/Excel_files/')

library(readxl)
results_original <- read_excel("NRN-Low_survival_floaters.xlsx")

#levels(results_original$Reaction_norm)[levels(results_original$Reaction_norm)=="RN"] <- "RN-Dispersal+Help"
library(plyr)
#results_original$Reaction_norm<-revalue(results_original$Reaction_norm, c("RN"="RN-Dispersal+Help"))

results_original$X0<-as.factor(results_original$X0)
results_original$Xh<-as.factor(results_original$Xh)
results_original$Xn<-as.ordered(results_original$Xn)
results_original$K1<-as.factor(results_original$K1)
results_original$Bias<-as.factor(results_original$Bias)
results_original$Evol_help<-as.factor(results_original$Evol_help)
results_original$Reaction_norm<-as.factor(results_original$Reaction_norm)
#results_original$Implementation<-as.factor(results_original$Implementation)


results<-results_original
resultsH<-subset(results,  Num_helpers>1)


#head(results)
str(results)
#View(esmd)
#summary(results)



############################## MORE GRAPHS #####################################

library(ggplot2)
#library(dplyr)
library(ggdark)

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


## Comparison between before / after evolution help

results<-results_original
results<-subset(results,  Implementation=="Normal")
results<-subset(results, X0==0.5)
results<-subset(results, Reaction_norm %in% c("RN") )


df_Bias <- data_summary(results, varname="Dispersal", 
                    groupnames=c("Bias", "Evol_help"))



ggplot(df_Bias, aes(x = Bias, y = Dispersal, group = Evol_help, color=Evol_help)) + 
  geom_line(size=1.2) +
  geom_point()+
  geom_errorbar(aes(ymin=Dispersal-sd, ymax=Dispersal+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0, 1))+
  scale_color_manual(values=c('#0000CC','#009999'))+
  dark_theme_gray(base_size = 14)



results<-results_original
results<-subset(results,  Implementation=="Normal")
results<-subset(results, Bias==2)
results<-subset(results, Reaction_norm %in% c("RN") )


df_Bias <- data_summary(results, varname="Dispersal", 
                        groupnames=c("X0", "Evol_help"))



ggplot(df_Bias, aes(x = X0, y = Dispersal, group = Evol_help, color=Evol_help)) + 
  geom_line(size=1.2) +
  geom_point()+
  geom_errorbar(aes(ymin=Dispersal-sd, ymax=Dispersal+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0, 1))+
  scale_color_manual(values=c('#0000CC','#009999'))+
  dark_theme_gray(base_size = 14)



## Comparison reaction norms

results<-results_original
results<-subset(results,  Implementation=="Normal")
results<-subset(results,  Evol_help=="yes")
results<-subset(results, Bias==2)
results<-subset(results, X0==0.5)
resultsH<-subset(results,  Num_helpers>1)
resultsH<-subset(resultsH, Help<4)

ggplot(results, aes(x = Reaction_norm, y = Dispersal)) +
  geom_boxplot(fill="blue") +
  xlab("Reaction norm to age")+
  #coord_cartesian(ylim = c(0, 1))+
  dark_theme_gray(base_size = 14)

ggplot(resultsH, aes(x = Reaction_norm, y = Help)) +
  geom_boxplot(fill="red") +
  xlab("Reaction norm to age")+
  coord_cartesian(ylim = c(0, 2))+
  dark_theme_gray(base_size = 14)



## Comparison X0 predation risk

results<-results_original
results<-subset(results,  Implementation=="Normal")
results<-subset(results,  Evol_help=="yes")
results<-subset(results, Bias==2)
results<-subset(results, Reaction_norm %in% c("NRN","RN") )
resultsH<-subset(results,  Num_helpers>1)
resultsH<-subset(resultsH, Help<4)


df_X0 <- data_summary(results, varname="Num_helpers", 
                        groupnames=c("X0", "Reaction_norm"))


ggplot(df_X0, aes(x = X0, y = Num_helpers, group = Reaction_norm, color=Reaction_norm, linetype =Reaction_norm)) + 
  geom_line(size=1.2) +
  geom_point()+
  #coord_cartesian(ylim = c(0, 1))+
  geom_errorbar(aes(ymin=Num_helpers-sd, ymax=Num_helpers+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  scale_color_manual(values=c('#0000CC','#009999'))+
  xlab("Baseline survival")+
  dark_theme_gray(base_size = 14)


df_X0 <- data_summary(resultsH, varname="Help", 
                      groupnames=c("X0", "Reaction_norm"))

ggplot(df_X0, aes(x = X0, y = Help, group = Reaction_norm, color=Reaction_norm, linetype =Reaction_norm)) + 
  geom_line(size=1.2) +
  geom_point()+
  coord_cartesian(ylim = c(0, 2))+
  geom_errorbar(aes(ymin=Help-sd, ymax=Help+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  scale_color_manual(values=c('#FF0000','#FF6600'))+
  xlab("Baseline survival")+
  dark_theme_gray(base_size = 14)


## Comparison opside options / bias


results<-results_original
results<-subset(results,  Implementation=="Normal")
results<-subset(results,  Evol_help=="yes")
results<-subset(results, X0==0.5)
results<-subset(results, Reaction_norm %in% c("NRN","RN") )
resultsH <- filter(results,Num_helpers>1)
resultsH <- filter(resultsH,Help<4)


df_Bias <- data_summary(results, varname="Dispersal", 
                      groupnames=c("Bias", "Reaction_norm"))


ggplot(df_Bias, aes(x = Bias, y = Dispersal, group = Reaction_norm, color=Reaction_norm, linetype =Reaction_norm)) + 
  geom_line(size=1.2) +
  geom_point()+
  coord_cartesian(ylim = c(0, 1))+
  geom_errorbar(aes(ymin=Dispersal-sd, ymax=Dispersal+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  scale_color_manual(values=c('#0000CC','#009999'))+
  xlab("Bias")+
  dark_theme_gray(base_size = 14)


df_Bias_Help <- data_summary(resultsH, varname="Help", 
                      groupnames=c("Bias", "Reaction_norm"))

ggplot(df_Bias_Help, aes(x = Bias, y = Help, group = Reaction_norm, color=Reaction_norm, linetype =Reaction_norm)) + 
  geom_line(size=1.2) +
  geom_point()+
  coord_cartesian(ylim = c(0, 2))+
  geom_errorbar(aes(ymin=Help-sd, ymax=Help+sd), width=0.2, size=1,
                position=position_dodge(0.05))+
  scale_color_manual(values=c('#FF0000','#FF6600'))+
  xlab("Bias")+
  dark_theme_gray(base_size = 14)











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




ggboxplot(resultsH, x = "Bias", y = "Help", color = "Reaction_norm")

ggboxplot(resultsH, x = "X0", y = "Help", color = "Reaction_norm")

ggboxplot(resultsH, x = "Xn", y = "Help", color = "Reaction_norm")

ggboxplot(resultsH, x = "Xh", y = "Help", color = "Reaction_norm")

ggboxplot(resultsH, x = "K1", y = "Help", color = "Reaction_norm")




ggboxplot(results, x = "Bias", y = "Dispersal", color = "Reaction_norm")

ggboxplot(results, x = "X0", y = "Dispersal", color = "Reaction_norm")

ggboxplot(results, x = "Xn", y = "Dispersal", color = "Reaction_norm")

ggboxplot(results, x = "Xh", y = "Dispersal", color = "Reaction_norm")

ggboxplot(results, x = "K1", y = "Dispersal", color = "Reaction_norm")


ggplot(results, aes(x = Bias, y = Dispersal)) +
  geom_boxplot(aes(fill = Evol_help)) +
  scale_fill_manual(values = c("#d11141", "#ffc425"))+
  labs(fill ="Evolution of help?") +
  dark_theme_gray(base_size = 14)
