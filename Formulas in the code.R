rm(list=ls())

#DISPERSAL

age<-seq(from=1,by=1, length=15)

dispersal_Formula<-function(beta, betaAge){
  dispersal<-1 / (1 + exp(betaAge*age - beta))
  return(dispersal)}

plot(age,dispersal_Formula(0,0), type="l", col="green", lwd=3, xlab="age", ylab="dispersal", ylim=range(min=0, max=1))
lines(age,dispersal_Formula(0.25,0.25), type="l", col="blue", lwd=3)
lines(age,dispersal_Formula(-0.25,0.25), type="l", col="purple", lwd=4)
lines(age,dispersal_Formula(0.25,-0.25), type="l", col="red", lwd=4)
lines(age,dispersal_Formula(-0.25,-0.25), type="l", col="orange", lwd=4)
legend(locator(1),c("Bo= 0, Br= 0",
                    "Bo= +, Br= +",
                    "Bo= -, Br= +", 
                    "Bo= +, Br= -",
                    "Bo= -, Br= -"), 
       lwd=c(2,2,2,2,2), col=c("green","blue","purple","red","orange"), y.intersp=1)
title("Dispersal")



#HELP

age<-seq(from=1,by=1, length=15)

help_Formula<-function(alpha, alphaAge, alphaAge2){
  help<-alpha + alphaAge*age + alphaAge2*age*age
  return(help)}

plot(age, help_Formula(2,0.3,0.03), type="l", col="red", lwd=4, xlab="Age", ylab="Help", ylim=range(min=0, max=10))
lines(age,help_Formula(2,0.3,-0.03), type="l", col="blue", lwd=4)
lines(age,help_Formula(2,-0.3,0.03), type="l", col="green", lwd=4)
lines(age,help_Formula(2,-0.3,-0.03), type="l", col="purple", lwd=4)
title("Help")
legend(locator(1),c("linear= +, quadratic= +",
                    "linear= +, quadratic= -", 
                    "linear= -, quadratic= +",
                    "linear= -, quadratic= -"), 
       lwd=c(2,2,2,2), col=c("red","blue","green","purple"), y.intersp=1)



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
m<-0.7
Xsh<-0.4
Xsn<-0.4

survival<-function(parameters,x,y){
  survival_Formula<- parameters[1] - parameters[2] /(1 + exp(-x)) + parameters[3] / (1 + exp(-y))
  return(survival_Formula)
}

rangx<-seq(from=0,by=0.1, length=11)
rangy<-c(1:11)
param<-c(m,Xsh,Xsn)
gridxy<-expand.grid(rangx,rangy)
names(gridxy)<-c('x',"y")

gridxy$help<-survival(param,gridxy$x,gridxy$y)

levelplot(help~x*y,data=gridxy, xlab="Help", ylab="Group size", main="Survival")


##Alternative survival mixed both
m<-0.9
Xsh<-1
Xsn<-1

survival<-function(parameters,x,y){
  survival_Formula<-parameters[1]- (parameters[1]/(1 + exp(parameters[2]- parameters[3]))-
    parameters[1]/(1 + exp(parameters[2] /(1 + exp(-x)) - parameters[3] / (1 + exp(-(y+1))))))
  
  return(survival_Formula)
}

rangx<-seq(from=0,by=0.15, length=16)
rangy<-c(0:15)
param<-c(m,Xsh,Xsn)
gridxy<-expand.grid(rangx,rangy)
names(gridxy)<-c('x',"y")

gridxy$help<-survival(param,gridxy$x,gridxy$y)

levelplot(help~x*y,data=gridxy, xlab="Help", ylab="Group size", main="Survival")

help<-seq(from=0,by=0.1, length=20)
groupsize<-seq(from=0,by=1, length=15)
Xsh /(1 + exp(-help))
Xsn / (1 + exp(-(groupsize)))
survival(param,gridxy$x,gridxy$y)

survivalTest<-m-((m/(1 + exp(Xsh-Xsn)))- m/(1 + exp(Xsh /(1 + exp(-2)) - Xsn / (1 + exp(-(10+1))))))
survivalTest




#FECUNDITY

K0<-1
cumhelp<-c(0:5)

fecundity_Formula <- function(K0,K1) {
  fecundity <- (K0 + cumhelp*K1 / (1 + cumhelp*K1))
  return(fecundity)
}

plot(cumhelp,fecundity_Formula(1,2), type="l", col="red", lwd=3, xlab="cumhelp", ylab="fecundity")
lines(cumhelp, fecundity_Formula(1,1), type="l", col="blue", lwd=3)
lines(cumhelp, fecundity_Formula(1,0.5), type="l", col="green", lwd=3)
title("Fecundity: Type II functional response")
legend(locator(1),c("K1=2","K1=1", "K1=0.5"), lwd=c(2,2,2), col=c("red","blue","green"), y.intersp=1)



