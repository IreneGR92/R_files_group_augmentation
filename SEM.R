rm(list=ls())
getwd()
#setwd('H:\\PhD\\CODE\\All_results\\txt_files\\15.04.19\\NRN\\Results')
setwd('C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\txt_files\\15.04.19\\NRN\\Results')
results<-read.table("SEM.csv",header = TRUE, sep=";")


# results$X0<-as.factor(results$X0)
# results$Xh<-as.factor(results$Xh)
# results$Xn<-as.ordered(results$Xn)
# results$K1<-as.factor(results$K1)
# results$Bias<-as.factor(results$Bias)
names(results)
#attach(results)
str(results)


library(lavaan)


# Adjust scale of variables for convenience
results2 <- results
results2$Bias  <- results$Bias/10
results2$Age  <- results$Age/10
results2$Group_size  <- results$Group_size/10
results2$K1  <- results$K1/10
str(results2)
summary(results2)


### Model 1: Initial Hypothesis
## Specify model (~regression, ~~residual correlations)
mod1 <- '
Dispersal ~ Bias 
Help ~ Xh 
Survival ~ X0 + Xh + Xn + Group_size + Help 
Group_size ~ Help + Dispersal + Survival
Relatedness ~~ Dispersal
Help ~~ Relatedness
Dispersal ~~ Help
'

## Fit model
mod1.fit <- sem(mod1, data=results2, sample.nobs = 20) # nobs: number of observations in each group that were used in the analysis.


## Examine GOF
show(mod1.fit) #high level of discrepancy between reality and prediction  if p-value is significant
fitMeasures(mod1.fit, "cfi") # CFI(goodness of fit)

## Examine Modification Indices
subset(modindices(mod1.fit), mi > 3.8) #3.84(criterion),  measures how much chi-square will drop if included. 
# between-indicator effects only
ord.dat.reg<-subset(modindices(mod1.fit), mi > 3.8 & op == "~")
ord.dat.reg[order(ord.dat.reg$mi),]
# error correlations only
ord.dat.cor<-subset(modindices(mod1.fit), mi > 3.8 & op == "~~")
ord.dat.cor[order(ord.dat.cor$mi),]

##Non significant
# between-indicator effects only
ord.dat.reg<-subset(modindices(mod1.fit), mi < 3.8 & op == "~")
ord.dat.reg[order(ord.dat.reg$mi),]
# error correlations only
ord.dat.cor<-subset(modindices(mod1.fit), mi < 3.8 & op == "~~")
ord.dat.cor[order(ord.dat.cor$mi),]





### Model 2
## Specify model
mod2 <- '
Bias ~ Dispersal
Xn ~ Group_size + Survival
Xh ~ Help + Survival
X0 ~ Survival 
K1 ~ Group_size + Relatedness
Dispersal ~ Help + Relatedness'

## Fit model
mod2.fit <- sem(mod2, data=results2, sample.nobs = 20) # nobs: number of observations in each group that were used in the analysis.


## Examine GOF
show(mod2.fit) #high level of discrepancy between reality and prediction  if p-value is significant
fitMeasures(mod2.fit, "cfi") # CFI(goodness of fit)

### Test for significant improvement
anova(mod1.fit, mod2.fit) #model with lower AIC is better

## Examine Modification Indices
subset(modindices(mod2.fit), mi > 3.8) #3.84(criterion),  measures how much chi-square will drop if included. 
# between-indicator effects only
ord.dat.reg<-subset(modindices(mod2.fit), mi > 3.8 & op == "~")
ord.dat.reg[order(ord.dat.reg$mi),]
# error correlations only
ord.dat.cor<-subset(modindices(mod2.fit), mi > 3.8 & op == "~~")
ord.dat.cor[order(ord.dat.cor$mi),]





