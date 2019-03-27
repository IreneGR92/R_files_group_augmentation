rm(list=ls())
getwd()
setwd('H:\\PhD\\CODE\\All_results')
#setwd('C:\\Users\\igaru\\OneDrive\\PhD\\CODE\\R_files')
results<-read.table("results_R-NRN.csv",header = TRUE, sep=";")

#head(results)
str(results)
#View(results)
#summary(results)

names(results)[names(results) == 'ï..Parameters_num'] <- 'Parameters_num'
results$Parameters_num<-as.factor(results$Parameters_num)
results$Parameters<-as.factor(results$Parameters)
str(results)

library("ggplot2")
library("gtable")
library("grid")
library("gridExtra")


### HELP ####

results$sd_help_low<-results$sd_help*-1

p1<-ggplot(results,                
       aes(x = Parameters,
           y = mean_help)) +
  geom_errorbar(aes(ymin = sd_help+mean_help,
                    ymax = sd_help_low+mean_help),
                width = 0.15, 
                size  = 0.5) +
  geom_point(shape = 15, 
             size  = 1.5) +
  #theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  
  ylab("level of help")+
  xlab("parameters")+
  coord_cartesian(ylim = c(0.06, 1)) 


### DISPERSAL ####

results$sd_dispersal_low<-results$sd_dispersal*-1

p2<-ggplot(results,                
       aes(x = Parameters,
           y = mean_dispersal)) +
  geom_errorbar(aes(ymin = sd_dispersal_low+mean_dispersal,
                    ymax = sd_dispersal+mean_dispersal),
                width = 0.15, 
                size  = 0.5) +
  geom_point(shape = 15, 
             size  = 1.5) +
  #theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  
  ylab("dispersal")+
  xlab("parameters")+
  coord_cartesian(ylim = c(0.06, 1)) 

### RELATEDNESS ####

results$sd_relat_low<-results$sd_relat*-1

p3<-ggplot(results,                
       aes(x = Parameters,
           y = mean_relat)) +
  geom_errorbar(aes(ymin = sd_relat+mean_relat,
                    ymax = sd_relat_low+mean_relat),
                width = 0.15, 
                size  = 0.5) +
  geom_point(shape = 15, 
             size  = 1.5) +
  #theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  
  ylab("relatedness")+
  xlab("parameters")+ 
  coord_cartesian(ylim = c(0.06, 1)) 


grid.arrange(p1, p2, p3, nrow = 3)
