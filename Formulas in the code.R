rm(list=ls())

#DISPERSAL

library(ggplot2)
library(reshape2)
library(ggdark)

age<-seq(from=1,by=1, length=15)

dispersal_Formula<-function(beta, betaAge){
  dispersal<-1 / (1 + exp(betaAge*age - beta))
  return(dispersal)}


DF.dispersal<-data.frame(age, dispersal_Formula(0,0), dispersal_Formula(0.25,0.25), dispersal_Formula(-0.25,0.25),dispersal_Formula(0.25,-0.25), dispersal_Formula(-0.25,-0.25))
names(DF.dispersal) <- c("Age","Bo=0,Br=0", "Bo=+,Br=+", "Bo=-,Br=+", "Bo=+,Br=-", "Bo=-,Br=-")
mdf <- melt(DF.dispersal, id="Age")
names(mdf)<-c("Age", "Input", "Dispersal.values")

ggplot(mdf, aes(x=Age, y=Dispersal.values,col=Input)) +
  geom_line(size=1)+
  xlab("Age")+ ylab("Dispersal")+
  scale_color_manual(values = c("green","blue", "purple", "red", "orange"))+
  dark_theme_gray(base_size = 20)
  

#HELP

library(ggplot2)
library(reshape2)

age<-seq(from=1,by=1, length=15)

help_Formula<-function(alpha, alphaAge, alphaAge2){
  help<-alpha + alphaAge*age + alphaAge2*age*age
  return(help)}


DF.help<-data.frame(age, help_Formula(1,0.05,0.005), help_Formula(1,0.05,-0.005), help_Formula(1,-0.05,0.005),help_Formula(1,-0.05,-0.005))
names(DF.help) <- c("Age","linear=+,quadratic=+", "linear= +,quadratic=-", "linear= -,quadratic=+", "linear=-,quadratic=-")
mdf <- melt(DF.help, id="Age")
names(mdf)<-c("Age", "Input", "Help.values")

ggplot(mdf, aes(x=Age, y=Help.values,col=Input)) +
  geom_line(size=1)+
  xlab("Age")+ ylab("Help")+
  scale_color_manual(values = c("green","blue", "purple", "red"))+
  coord_cartesian(ylim = c(0.049, 3))+
  dark_theme_gray(base_size = 20)



#SURVIVAL

##Interaction between help and group size

library("lattice")

m<-0.1
Xsh<-4
Xsn<-1

survival<-function(parameters,x,y){
  survival_Formula<-(1-parameters[1])/(1 + exp(parameters[2]*x - parameters[3]*(y+1)))
  return(survival_Formula)
}

rangx<-seq(from=0,by=0.15, length=16)
rangy<-c(0:15)
param<-c(m,Xsh,Xsn)
gridxy<-expand.grid(rangx,rangy)
names(gridxy)<-c('x',"y")

gridxy$help<-survival(param,gridxy$x,gridxy$y)

levelplot(help~x*y,data=gridxy, xlab="Help", ylab="Group size", main="Survival")


##Alternative survival
X0<-0.7
Xsh<-0.4
Xsn<-0.4

survival<-function(parameters,x,y){
  survival_Formula<- parameters[1] - parameters[2] /(1 + exp(-x)) + parameters[3] / (1 + exp(-y))
  return(survival_Formula)
}

rangx<-seq(from=0,by=0.1, length=11)
rangy<-c(1:11)
param<-c(X0,Xsh,Xsn)
gridxy<-expand.grid(rangx,rangy)
names(gridxy)<-c('x',"y")

gridxy$help<-survival(param,gridxy$x,gridxy$y)

levelplot(help~x*y,data=gridxy, xlab="Help", ylab="Group size", main="Survival")


#Value of help from which no effect on survival

help=seq(from=0,by=0.5, length=16)

survivalHelp<-function(Xh,help){
  survival_Formula<-(Xh/(1 + exp(-help)))
}

DF.help<-data.frame(help, survivalHelp(0.4,help), survivalHelp(0.2,help))
names(DF.help) <- c("Help","Survival_Xh=0.4", "Survival_Xh=0.2")
mdf <- melt(DF.help, id="Help")
names(mdf)<-c("Help", "Label", "Survival")

ggplot(mdf, aes(x=Help, y=Survival,col=Label)) +
  geom_line(size=1)+
  scale_color_manual(values = c("green","purple"))




#FECUNDITY

library(ggplot2)
library(reshape2)

K0<-1
cumhelp<-c(0:10)

fecundity_Formula <- function(K0,K1) {
  fecundity <- (K0 + cumhelp*K1 / (1 + cumhelp*K1))
  return(fecundity)
}

DF.fecundity<-data.frame(cumhelp, fecundity_Formula(K0,2), fecundity_Formula(K0,1), fecundity_Formula(K0,0.5))
names(DF.fecundity) <- c("Cumulative_help","K1=2","K1=1", "K1=0.5")
mdf <- melt(DF.fecundity, id="Cumulative_help")
names(mdf)<-c("Cumulative_help", "Input", "Fecundity.values")

ggplot(mdf, aes(x=Cumulative_help, y=Fecundity.values,col=Input)) +
  geom_line(size=1)+
  xlab("Cummulative help in group")+ ylab("Fecundity")+
  scale_color_manual(values = c("green","blue", "red", "orange"))+
  coord_cartesian(ylim = c(1, 2)) +
  dark_theme_gray(base_size = 20)





