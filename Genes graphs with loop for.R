rm(list=ls())

library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(formattable)
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')
library(rJava)
library(xlsx)
library(data.table)
library(dplyr)
library(tidyselect)
library(magrittr)
library(ggpubr)


#directory<-"~/Documents/Model/Results/RN-No_help/" #Linux
directory<-"H:\\PhD\\CODE\\All_results\\txt_files\\NEW\\"  #Home/Work

getwd()

#Main
files_main<-list.files(paste(directory, "main",sep=""), pattern=NULL, all.files=FALSE,full.names=FALSE)
dim_files_main<-dim.data.frame(files_main)


i=0
while (i < dim_files_main[2]) {

i=i+1

nameFile<-files_main[i] #delete the .txt
nameFile<-gsub('.{4}$', '', nameFile)
nameFile<-substring(nameFile, 17) #20 for group_augmentation, 17 for main_parameters


setwd(paste(directory, "main",sep=""))
Parameters<-read.table(paste("main_parameters_", nameFile, ".txt",sep=""), skip=1, nrows=29) #group_augmentation_
Parameters <- mutate(Parameters, V3 = paste(Parameters[,1], Parameters[,2])) 
GA<-read.table(paste("main_parameters_", nameFile, ".txt",sep=""),header = TRUE,skip=31)


#Last generation
setwd(paste(directory, "last_generation",sep=""))
GA2<-read.table(paste("last_generation_parameters_", nameFile, ".txt",sep=""), header = TRUE, skip=31) #group_augmentation_last_generation_
GA2 <- subset(GA2, age>0)
setDF(GA2)


#head(GA)
#tail(GA2)
#str(GA)
#dim(files_main)
#View(Parameters)
#summary(GA)



proportions_floaters_breeder<-function(){
  propFloatBreeder <- GA$newBreederFloater / (GA$newBreederFloater + GA$newBreederHelper)
  return(propFloatBreeder)}

GA$propFloatBreeder <- proportions_floaters_breeder()


GA$meanDispersal <- as.character(GA$meanDispersal)
GA$meanDispersal[GA$meanDispersal == "-1"] <- NA
GA$meanDispersal <- as.numeric(GA$meanDispersal)

GA$meanSurvival <- as.character(GA$meanSurvival)
GA$meanSurvival[GA$meanSurvival == "-1"] <- NA
GA$meanSurvival <- as.numeric(GA$meanSurvival)

GA$meanSurvival_H <- as.character(GA$meanSurvival_H)
GA$meanSurvival_H[GA$meanSurvival_H == "-1"] <- NA
GA$meanSurvival_H[GA$meanSurvival_H == "NaN"] <- NA
GA$meanSurvival_H <- as.numeric(GA$meanSurvival_H)

GA$meanSurvival_B <- as.character(GA$meanSurvival_B)
GA$meanSurvival_B[GA$meanSurvival_B == "-1"] <- NA
GA$meanSurvival_B[GA$meanSurvival_B == "NaN"] <- NA
GA$meanSurvival_B <- as.numeric(GA$meanSurvival_B)

GA$meanSurvival_F <- as.character(GA$meanSurvival_F)
GA$meanSurvival_F[GA$meanSurvival_F == "-1"] <- NA
GA$meanSurvival_F[GA$meanSurvival_F == "NaN"] <- NA
GA$meanSurvival_F <- as.numeric(GA$meanSurvival_F)




#apply(GA, 2, function(meanHelp) ifelse(meanHelp > 4, 4, meanHelp))


##Means between replicas

do_mean<-function(x){
  x<-aggregate(x, list(GA$Generation), mean, na.rm=TRUE) 
  return(x)
}
do_sd<-function(x){
  x<-aggregate(x, list(GA$Generation), sd, na.rm=TRUE) 
  return(x)
}

GA_means<-do_mean(GA)
GA_SD<-do_sd(GA)

GA$Replica<-as.factor(GA$Replica)

################## STATS LAST GENERATION FROM MAIN ############################

##Means and SD of the means of the variables between the different replicas

do_mean_LG<-function(x,y){
  x<-round(mean(x[GA$Generation==y], na.rm=TRUE), digits = 4)
  return(x)
}

do_SD_LG<-function(x,y){
  x<-round(sd(x[GA$Generation==y], na.rm=TRUE), digits = 4)
  return(x)
}

gen_with_help<-100000
gen_without_help<-25000

#For each replica last generation
meanAlphaR<-GA$meanAlpha[GA$Generation==gen_with_help]
meanAlphaAgeR<-GA$meanAlphaAge[GA$Generation==gen_with_help]
#meanAlphaAge2R<-GA$meanAlphaAge2[GA$Generation==gen_with_help]
meanBetaR<-GA$meanBeta[GA$Generation==gen_with_help]
meanBetaAgeR<-GA$meanBetaAge[GA$Generation==gen_with_help]
meanAgeR<-GA$Age[GA$Generation==gen_with_help]
meanAgeH_R<-GA$Age_H[GA$Generation==gen_with_help]
meanAgeB_R<-GA$Age_B[GA$Generation==gen_with_help]
meanAgeF_R<-GA$Age_F[GA$Generation==gen_with_help]
meanGroupSizeR<-GA$Group_size[GA$Generation==gen_with_help]
meanHelpR<-GA$meanHelp[GA$Generation==gen_with_help]
meanCumHelpR<-GA$meanCumHelp[GA$Generation==gen_with_help]
meanDispersalR<-GA$meanDispersal[GA$Generation==gen_with_help]
meanSurvivalR<-GA$meanSurvival[GA$Generation==gen_with_help]
meanSurvivalH_R<-GA$meanSurvival_H[GA$Generation==gen_with_help]
meanSurvivalB_R<-GA$meanSurvival_B[GA$Generation==gen_with_help]
meanSurvivalF_R<-GA$meanSurvival_F[GA$Generation==gen_with_help]
meanRelatednessR<-GA$Relatedness[GA$Generation==gen_with_help]
meanCorr_Help_DispR<-GA$corr_Help_Disp[GA$Generation==gen_with_help]
meanPropFloatBreederR<-GA$propFloatBreeder[GA$Generation==gen_with_help]

#For each replica before evolution help
meanAlphaRNH<-GA$meanAlpha[GA$Generation==gen_without_help]
meanAlphaAgeRNH<-GA$meanAlphaAge[GA$Generation==gen_without_help]
#meanAlphaAge2RNH<-GA$meanAlphaAge2[GA$Generation==gen_without_help]
meanBetaRNH<-GA$meanBeta[GA$Generation==gen_without_help]
meanBetaAgeRNH<-GA$meanBetaAge[GA$Generation==gen_without_help]
meanAgeRNH<-GA$Age[GA$Generation==gen_without_help]
meanAgeH_RNH<-GA$Age_H[GA$Generation==gen_without_help]
meanAgeB_RNH<-GA$Age_B[GA$Generation==gen_without_help]
meanAgeF_RNH<-GA$Age_F[GA$Generation==gen_without_help]
meanGroupSizeRNH<-GA$Group_size[GA$Generation==gen_without_help]
meanHelpRNH<-GA$meanHelp[GA$Generation==gen_without_help]
meanCumHelpRNH<-GA$meanCumHelp[GA$Generation==gen_without_help]
meanDispersalRNH<-GA$meanDispersal[GA$Generation==gen_without_help]
meanSurvivalRNH<-GA$meanSurvival[GA$Generation==gen_without_help]
meanSurvivalH_RNH<-GA$meanSurvival_H[GA$Generation==gen_without_help]
meanSurvivalB_RNH<-GA$meanSurvival_B[GA$Generation==gen_without_help]
meanSurvivalF_RNH<-GA$meanSurvival_F[GA$Generation==gen_without_help]
meanRelatednessRNH<-GA$Relatedness[GA$Generation==gen_without_help]
meanCorr_Help_DispRNH<-GA$corr_Help_Disp[GA$Generation==gen_without_help]
meanPropFloatBreederRNH<-GA$propFloatBreeder[GA$Generation==gen_without_help]

#For the last generation, after the evolution of help
meanAlpha<-do_mean_LG(GA$meanAlpha, gen_with_help)
meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, gen_with_help)
#meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, gen_with_help)
meanBeta<-do_mean_LG(GA$meanBeta, gen_with_help)
meanBetaAge<-do_mean_LG(GA$meanBetaAge, gen_with_help)
meanAge<-do_mean_LG(GA$Age, gen_with_help)
meanAgeH<-do_mean_LG(GA$Age_H, gen_with_help)
meanAgeB<-do_mean_LG(GA$Age_B, gen_with_help)
meanAgeF<-do_mean_LG(GA$Age_F, gen_with_help)
meanGroupSize<-do_mean_LG(GA$Group_size, gen_with_help)
meanHelp<-do_mean_LG(GA$meanHelp, gen_with_help)
meanCumHelp<-do_mean_LG(GA$meanCumHelp, gen_with_help)
meanDispersal<-do_mean_LG(GA$meanDispersal, gen_with_help)
meanSurvival<-do_mean_LG(GA$meanSurvival, gen_with_help)
meanSurvivalH<-do_mean_LG(GA$meanSurvival_H, gen_with_help)
meanSurvivalB<-do_mean_LG(GA$meanSurvival_B, gen_with_help)
meanSurvivalF<-do_mean_LG(GA$meanSurvival_F, gen_with_help)
meanRelatedness<-do_mean_LG(GA$Relatedness, gen_with_help)
meanCorr_Help_Disp<-do_mean_LG(GA$corr_Help_Disp, gen_with_help)
meanPropFloatBreeder<-do_mean_LG(GA$propFloatBreeder, gen_with_help)

SD_Alpha<-do_SD_LG(GA$meanAlpha, gen_with_help)
SD_AlphaAge<-do_SD_LG(GA$meanAlphaAge, gen_with_help)
#SD_AlphaAge2<-do_SD_LG(GA$meanAlphaAge2, gen_with_help)
SD_Beta<-do_SD_LG(GA$meanBeta, gen_with_help)
SD_BetaAge<-do_SD_LG(GA$meanBetaAge, gen_with_help)
SD_Age<-do_SD_LG(GA$Age, gen_with_help)
SD_AgeH<-do_SD_LG(GA$Age_H, gen_with_help)
SD_AgeB<-do_SD_LG(GA$Age_B, gen_with_help)
SD_AgeF<-do_SD_LG(GA$Age_F, gen_with_help)
SD_GroupSize<-do_SD_LG(GA$Group_size, gen_with_help)
SD_Help<-do_SD_LG(GA$meanHelp, gen_with_help)
SD_CumHelp<-do_SD_LG(GA$meanCumHelp, gen_with_help)
SD_Dispersal<-do_SD_LG(GA$meanDispersal, gen_with_help)
SD_Survival<-do_SD_LG(GA$meanSurvival, gen_with_help)
SD_SurvivalH<-do_SD_LG(GA$meanSurvival_H, gen_with_help)
SD_SurvivalB<-do_SD_LG(GA$meanSurvival_B, gen_with_help)
SD_SurvivalF<-do_SD_LG(GA$meanSurvival_F, gen_with_help)
SD_Relatedness<-do_SD_LG(GA$Relatedness, gen_with_help)
SDcorr_Help_Disp<-do_SD_LG(GA$corr_Help_Disp, gen_with_help)
SD_PropFloatBreeder<-do_SD_LG(GA$propFloatBreeder, gen_with_help)

#For the las generation before the evolution of help
meanBetaNH<-do_mean_LG(GA$meanBeta, gen_without_help)
meanBetaAgeNH<-do_mean_LG(GA$meanBetaAge, gen_without_help)
meanAgeNH<-do_mean_LG(GA$Age, gen_without_help)
meanAgeH_NH<-do_mean_LG(GA$Age_H, gen_without_help)
meanAgeB_NH<-do_mean_LG(GA$Age_B, gen_without_help)
meanAgeF_NH<-do_mean_LG(GA$Age_F, gen_without_help)
meanGroupSizeNH<-do_mean_LG(GA$Group_size, gen_without_help)
meanHelpNH<-do_mean_LG(GA$meanHelp, gen_without_help)
meanDispersalNH<-do_mean_LG(GA$meanDispersal, gen_without_help)
meanSurvivalNH<-do_mean_LG(GA$meanSurvival, gen_without_help)
meanSurvivalH_NH<-do_mean_LG(GA$meanSurvival_H, gen_without_help)
meanSurvivalB_NH<-do_mean_LG(GA$meanSurvival_B, gen_without_help)
meanSurvivalF_NH<-do_mean_LG(GA$meanSurvival_F, gen_without_help)
meanRelatednessNH<-do_mean_LG(GA$Relatedness, gen_without_help)
meanPropFloatBreederNH<-do_mean_LG(GA$propFloatBreeder, gen_without_help)

SD_BetaNH<-do_SD_LG(GA$meanBeta, gen_without_help)
SD_BetaAgeNH<-do_SD_LG(GA$meanBetaAge, gen_without_help)
SD_AgeNH<-do_SD_LG(GA$Age, gen_without_help)
SD_AgeH_NH<-do_SD_LG(GA$Age_H, gen_without_help)
SD_AgeB_NH<-do_SD_LG(GA$Age_B, gen_without_help)
SD_AgeF_NH<-do_SD_LG(GA$Age_F, gen_without_help)
SD_GroupSizeNH<-do_SD_LG(GA$Group_size, gen_without_help)
SD_HelpNH<-do_SD_LG(GA$meanHelp, gen_without_help)
SD_DispersalNH<-do_SD_LG(GA$meanDispersal, gen_without_help)
SD_SurvivalNH<-do_SD_LG(GA$meanSurvival, gen_without_help)
SD_SurvivalH_NH<-do_SD_LG(GA$meanSurvival_H, gen_without_help)
SD_SurvivalB_NH<-do_SD_LG(GA$meanSurvival_B, gen_without_help)
SD_SurvivalF_NH<-do_SD_LG(GA$meanSurvival_F, gen_without_help)
SD_RelatednessNH<-do_SD_LG(GA$Relatedness, gen_without_help)
SD_PropFloatBreederNH<-do_SD_LG(GA$propFloatBreeder, gen_without_help)



descriptivesR <- data.frame( ID=c(nameFile),
                             X0=c(Parameters[13,2]),
                             Xh=c(Parameters[14,2]),
                             Xn=c(Parameters[15,2]),
                             K1=c(Parameters[17,2]),
                             Bias=c(Parameters[12,2]),
                             Help=c(meanHelpR),
                             CumHelp=c(meanCumHelpR),
                             Dispersal=c(meanDispersalR),
                             Survival=c(meanSurvivalR),
                             SurvivalH=c(meanSurvivalH_R),
                             SurvivalB=c(meanSurvivalB_R),
                             SurvivalF=c(meanSurvivalF_R),
                             Relatedness=c(meanRelatednessR),
                             Age=c(meanAgeR),
                             AgeH=c(meanAgeH_R),
                             AgeB=c(meanAgeB_R),
                             AgeF=c(meanAgeF_R),
                             Group_size=c(meanGroupSizeR),
                             Help_Disp=c(meanCorr_Help_DispR),
                             propFloaterB=c(meanPropFloatBreederR))

descriptivesRNH <- data.frame( ID=c(nameFile),
                             X0=c(Parameters[13,2]),
                             Xh=c(Parameters[14,2]),
                             Xn=c(Parameters[15,2]),
                             K1=c(Parameters[17,2]),
                             Bias=c(Parameters[12,2]),
                             Help=c(meanHelpRNH),
                             CumHelp=c(meanCumHelpRNH),
                             Dispersal=c(meanDispersalRNH),
                             Survival=c(meanSurvivalRNH),
                             SurvivalH=c(meanSurvivalH_RNH),
                             SurvivalB=c(meanSurvivalB_RNH),
                             SurvivalF=c(meanSurvivalF_RNH),
                             Relatedness=c(meanRelatednessRNH),
                             Age=c(meanAgeRNH),
                             AgeH=c(meanAgeH_RNH),
                             AgeB=c(meanAgeB_RNH),
                             AgeF=c(meanAgeF_RNH),
                             Group_size=c(meanGroupSizeRNH),
                             Help_Disp=c(meanCorr_Help_DispRNH),
                             propFloaterB=c(meanPropFloatBreederRNH))


descriptives <- data.frame(Variable=c("alpha", "alphaAge", #"alphaAge2",
                                      "beta", "betaAge",
                                      "Help","CumHelp", "Dispersal", 
                                      "Survival", "SurvivalH","SurvivalB","SurvivalF",
                                      "Relatedness",
                                      "age","ageH","ageB","ageF",
                                      "Group_size",
                                      "Help_Disp", "propFloaterB"),
                           Mean=c(meanAlpha, meanAlphaAge, #meanAlphaAge2,
                                  meanBeta, meanBetaAge,
                                  meanHelp,meanCumHelp, meanDispersal,
                                  meanSurvival,meanSurvivalH, meanSurvivalB, meanSurvivalF,
                                  meanRelatedness,
                                  meanAge, meanAgeH, meanAgeB, meanAgeF,
                                  meanGroupSize,
                                  meanCorr_Help_Disp, meanPropFloatBreeder),
                           SD=c(SD_Alpha,SD_AlphaAge,#SD_AlphaAge2,
                                SD_Beta,SD_BetaAge,
                                SD_Help,SD_CumHelp, SD_Dispersal,
                                SD_Survival, SD_SurvivalH, SD_SurvivalB, SD_SurvivalF,
                                SD_Relatedness,
                                SD_Age, SD_AgeH, SD_AgeB, SD_AgeF,
                                SD_GroupSize,
                                SDcorr_Help_Disp, SD_PropFloatBreeder))


descriptivesNH <- data.frame(Variable=c("beta", "betaAge",
                                      "Help","Dispersal", 
                                      "Survival", "SurvivalH","SurvivalB","SurvivalF",
                                      "Relatedness",
                                      "age","ageH","ageB","ageF",
                                      "Group_size","propFloaterB"),
                           Mean=c(meanBetaNH, meanBetaAgeNH,
                                  meanHelpNH,meanDispersalNH,
                                  meanSurvivalNH, meanSurvivalH_NH, meanSurvivalB_NH, meanSurvivalF_NH,
                                  meanRelatednessNH,
                                  meanAgeNH, meanAgeH_NH, meanAgeB_NH, meanAgeF_NH,
                                  meanGroupSizeNH,meanPropFloatBreederNH),
                           SD=c(SD_BetaNH,SD_BetaAgeNH,
                                SD_HelpNH,SD_DispersalNH,
                                SD_SurvivalNH, SD_SurvivalH_NH, SD_SurvivalB_NH, SD_SurvivalF_NH,
                                SD_RelatednessNH,
                                SD_AgeNH, SD_AgeH_NH, SD_AgeB_NH, SD_AgeF_NH,
                                SD_GroupSizeNH,SD_PropFloatBreederNH))


write.xlsx(descriptivesR, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Result per replica", append = FALSE)
write.xlsx(descriptivesRNH, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Result per replica bef help", append = TRUE) # append TRUE to create a new sheet in the same file
write.xlsx(descriptives, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Results", append = TRUE)
write.xlsx(descriptivesNH, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Result before help", append = TRUE)
write.xlsx(Parameters, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Parameters", append = TRUE)




###################### FORMULAS LAST GENERATION #################################


replace_with_zero_if_below_zero <- function(x) {
  x <- ifelse(x<0,0,x)
  return(x)
}

###Help

help_Formula<-function(){
  help <- GA2$alpha+GA2$alphaAge*GA2$age#+GA2$alphaAge2*GA2$age*GA2$age
  help <- sapply(help, replace_with_zero_if_below_zero)
  return(help)}

GA2$Help <- help_Formula()
GA2[GA2$type==0,]$Help<-NA


###Dispersal
dispersal_Formula<-function(){
  dispersal<-1 / (1 + exp(GA2$betaAge*GA2$age - GA2$beta))
  return(dispersal)}

GA2$Dispersal<-dispersal_Formula()
GA2[GA2$type==0,]$Dispersal<-NA

##Age
mean_age<-mean(GA2$age)
dichotonomic_age <- function(x) {
  x <- ifelse(x<mean_age,"< mean age","> mean age")
  return(x)
}
GA2$AgeDic<-dichotonomic_age(GA2$age)
GA2$AgeDic<-as.factor(GA2$AgeDic)




############################################### PLOTS ########################################################################

pdf(paste(directory, "graphs_", nameFile, ".pdf", sep="")) # Open a pdf file

#Dummy plot to print initial parameters in the simulation
par(mfrow = c(1, 1))
plot(0.5, 0.5,  xlab=" ", ylab=" ", type="n")
op <- par(cex = 0.79)
legend(x="bottomleft", legend = Parameters[,3], pch=1)
title(nameFile)
par(mfrow = c(1, 1))


# ########## PLOTS MEAN AND SD ############
# ##Help plot
# pHelp<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanHelp)) +
#   geom_ribbon(aes(ymin=GA_means$meanHelp-GA_SD$meanHelp, ymax=GA_means$meanHelp+GA_SD$meanHelp),
#               alpha=0.3) +
#   geom_line(color="red", size=1)+
#   xlab("Generation")+ ylab("Help") 
#   #coord_cartesian(ylim = c(0.049, 1))
# 
# 
# ##Dispersal plot
# pDispersal<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanDispersal)) +
#   geom_ribbon(aes(ymin=GA_means$meanDispersal-GA_SD$meanDispersal, ymax=GA_means$meanDispersal+GA_SD$meanDispersal),
#               alpha=0.3) +
#   geom_line(color="blue", size=1)+
#   xlab("Generation")+ ylab("Dispersal")+
#   coord_cartesian(ylim = c(0.049, 1))
# 
# 
# ##Survival
# pSurvival<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanSurvival)) +
#   geom_ribbon(aes(ymin=GA_means$meanSurvival-GA_SD$meanSurvival, ymax=GA_means$meanSurvival+GA_SD$meanSurvival),
#               alpha=0.3) +
#   geom_line(color="black", size=1)+
#   xlab("Generation")+ ylab("Survival")+
#   coord_cartesian(ylim = c(0.049, 1))
# 
# ##Relatedness plot
# pRelatedness<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$Relatedness)) +
#   geom_ribbon(aes(ymin=GA_means$Relatedness-GA_SD$Relatedness, ymax=GA_means$Relatedness+GA_SD$Relatedness),
#               alpha=0.3) +
#   geom_line(color="orange", size=1)+
#   xlab("Generation")+ ylab("Relatedness")+
#   coord_cartesian(ylim = c(0.049, 1))
# 
# 
# ##Population stability?
# pGroupSize<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$Group_size)) +
#   geom_ribbon(aes(ymin=GA_means$Group_size-GA_SD$Group_size, ymax=GA_means$Group_size+GA_SD$Group_size),
#               alpha=0.3) +
#   geom_line(color="purple", size=1)+
#   xlab("Generation")+ ylab("Group size")
#   #coord_cartesian(ylim = c(0.049, 5))
# 
# 
# ##Cummulative help in fecundity
# pCumHelp<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanCumHelp)) +
#   geom_ribbon(aes(ymin=GA_means$meanCumHelp-GA_SD$meanCumHelp, ymax=GA_means$meanCumHelp+GA_SD$meanCumHelp),
#               alpha=0.3) +
#   geom_line(color="red", size=1)+
#   xlab("Generation")+ ylab("Cummulative help")
# 
# ##Proportion of floaters that become breeders compared to helpers plot
# pPropFloatBreeder<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$propFloatBreeder)) +
#   geom_ribbon(aes(ymin=GA_means$propFloatBreeder-GA_SD$propFloatBreeder, ymax=GA_means$propFloatBreeder+GA_SD$propFloatBreeder),
#               alpha=0.3) +
#   geom_line(color="green", size=1)+
#   xlab("Generation")+ ylab("Prop. floaters->breeders")+
#   coord_cartesian(ylim = c(0.049, 1))
# 
# 
# 
# grid.arrange(pHelp, pDispersal, pCumHelp, pPropFloatBreeder, pRelatedness, pGroupSize, pSurvival, nrow = 4)


########## PLOTS PER REPLICA ############
##Help plot
pHelp<-ggplot(GA, aes(x=GA$Generation, y=GA$meanHelp, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="red",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Help")
#coord_cartesian(ylim = c(0.049, 4))


##Dispersal plot
pDispersal<-ggplot(GA, aes(x=GA$Generation, y=GA$meanDispersal, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="blue",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Dispersal")+
  coord_cartesian(ylim = c(0.049, 1))


##Survival
pSurvival<-ggplot(GA, aes(x=GA$Generation, y=GA$meanSurvival, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="black",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Survival")+
  coord_cartesian(ylim = c(0.049, 1))


##Relatedness plot
pRelatedness<-ggplot(GA, aes(x=GA$Generation, y=GA$Relatedness, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="orange",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Relatedness")+
  coord_cartesian(ylim = c(0.049, 1))


##Population stability?
pGroupSize<-ggplot(GA, aes(x=GA$Generation, y=GA$Group_size, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="purple",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Group_size")


##Cummulative help in fecundity
pCumHelp<-ggplot(GA, aes(x=GA$Generation, y=GA$meanCumHelp, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="red",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Cummulative help")

##Proportion of floaters that become breeders compared to helpers plot
pPropFloatBreeder<-ggplot(GA, aes(x=GA$Generation, y=GA$propFloatBreeder, by=GA$Replica))+
  geom_line(color="grey", size=0.5)+
  stat_summary(fun.y=mean, geom="line", colour="green",lwd=1,aes(group=1))+
  xlab("Generation")+ ylab("Prop. floaters->breeders")+
  coord_cartesian(ylim = c(0.049, 1))


grid.arrange(pHelp, pDispersal, pCumHelp, pPropFloatBreeder, pRelatedness, pGroupSize, pSurvival, nrow = 4)




########### REACTION NORMS ####################

par(mfrow = c(3, 4))
age<-seq(from=1,by=1, length=11)

# HELP
if(Parameters[1,2]==1){
  replace_with_zero_if_below_zero <- function(x) {
    x <- ifelse(x<0,0,x)
    return(x)
  }

  help_Formula<-function(meanAlpha, meanAlphaAge){ #meanAlphaAge2
    help<-meanAlpha + meanAlphaAge*age# + meanAlphaAge2*age*age
    help<-ifelse(help<0,0,help)
    return(help)}

  meanAlpha<-do_mean_LG(GA$meanAlpha, 10000)
  meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, 10000)
  #meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, 10000)
  helpP2<-plot(age, help_Formula(meanAlpha, meanAlphaAge), type="l", col="red", lwd=4, xlab="Age", ylab="Help",main="Generation 10000", ylim=range(min=0, max=1))

  meanAlpha<-do_mean_LG(GA$meanAlpha, 25000)
  meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, 25000)
  #meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, 25000)
  helpP3<-plot(age, help_Formula(meanAlpha, meanAlphaAge), type="l", col="red", lwd=4, xlab="Age", ylab="Help",main="Generation 25000", ylim=range(min=0, max=1))

  meanAlpha<-do_mean_LG(GA$meanAlpha, 50000)
  meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, 50000)
  #meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, 50000)
  helpP4<-plot(age, help_Formula(meanAlpha, meanAlphaAge), type="l", col="red", lwd=4, xlab="Age", ylab="Help",main="Generation 50000", ylim=range(min=0, max=1))

  meanAlpha<-do_mean_LG(GA$meanAlpha, 100000)
  meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, 100000)
  #meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, 100000)
  helpP<-plot(age, help_Formula(meanAlpha, meanAlphaAge), type="l", col="red", lwd=4, xlab="Age", ylab="Help",main="Last generation", ylim=range(min=0, max=1))#, ylim=range(min=0, max=1.5)

}

# DISPERSAL

if(Parameters[2,2]==1){
  dispersal_Formula<-function(meanBeta, meanBetaAge){
    dispersal = 1 / (1 + exp(meanBetaAge*age - meanBeta))
    return(dispersal)
  }

  meanBeta<-do_mean_LG(GA$meanBeta, 10000)
  meanBetaAge<-do_mean_LG(GA$meanBetaAge, 10000)
  dispersalP<-plot(age,dispersal_Formula(meanBeta, meanBetaAge), type="l", col="blue", lwd=3, xlab="Age", ylab="Dispersal", main="Generation 10000", ylim=range(min=0, max=1))
  meanBeta<-do_mean_LG(GA$meanBeta, 25000)
  meanBetaAge<-do_mean_LG(GA$meanBetaAge, 25000)
  dispersalP<-plot(age,dispersal_Formula(meanBeta, meanBetaAge), type="l", col="blue", lwd=3, xlab="Age", ylab="Dispersal", main="Generation 25000", ylim=range(min=0, max=1))
  meanBeta<-do_mean_LG(GA$meanBeta, 50000)
  meanBetaAge<-do_mean_LG(GA$meanBetaAge, 50000)
  dispersalP<-plot(age,dispersal_Formula(meanBeta, meanBetaAge), type="l", col="blue", lwd=3, xlab="Age", ylab="Dispersal", main="Generation 50000", ylim=range(min=0, max=1))
  meanBeta<-do_mean_LG(GA$meanBeta, 100000)
  meanBetaAge<-do_mean_LG(GA$meanBetaAge, 100000)
  dispersalP<-plot(age,dispersal_Formula(meanBeta, meanBetaAge), type="l", col="blue", lwd=3, xlab="Age", ylab="Dispersal", main="Last generation", ylim=range(min=0, max=1))

}


########## LAST GENRATION ##########

# GA2$age_f<-as.factor(GA2$age)
# GA2<-na.omit(GA2)
#
#
# helpBoxP<-ggboxplot(GA2, x = "age_f", y = "Help",
#           add = "jitter", add.params = list(color = "grey", alpha=0.2),#instead of grey you can put a factor
#           color = "black",
#           xlab = "Age", ylab = "Help", title =  "Level of help by age",ylim=c(0.1,2),
#           palette = "aaas")
#
#
# dispersalBoxP<-ggboxplot(GA2, x = "age_f", y = "dispersal",
#           add = "jitter", add.params = list(color = "grey", alpha=0.2),#instead of grey you can put a factor
#           color = "black",
#           xlab = "Age", ylab = "Dispersal", title =  "Level of dispersal by age",ylim=c(0,1),
#           palette = "aaas")
#
# grid.arrange(helpBoxP, dispersalBoxP, nrow = 2, ncol=2)
#
# j=1
# relatedness_values<-GA$Relatedness[GA$Generation==100000]
# while(j<21){
#     GA2$Relatedness[GA2$replica == j] <- relatedness_values[j]
#   j<-j+1
# }
#
# plot(GA2$Help, GA2$dispersal, col=GA2$replica,  xlab="Help", ylab="Dispersal")
# title("Dispersal vs Help")
#
# plot(GA2$Help, GA2$Relatedness, col=GA2$replica,  xlab="Help", ylab="Relatedness")
# title("Relatedness vs Help")

#plot(GA2$Help, GA2$Dispersal, col=c("blue","green")[GA2$AgeDic],  xlab="Help", ylab="Dispersal")
#legend(x="bottomright", legend = levels(GA2$AgeDic), col=c("blue","green"), pch=1)
#title("Dispersal vs Help")

#plot(GA2$age, GA2$Help, col=GA2$replica,  xlab="Age", ylab="Help")
#title("Help vs Age")

#plot(GA2$age, GA2$dispersal, col=GA2$replica,  xlab="Age", ylab="dispersal")
#title("Dispersal vs Age")


par(mfrow = c(1, 1))

dev.off() # Close the pdf file


}

  #########################################################################################################################################


# ########################## DEVELOPMENT 
# 
# replace_with_zero_if_below_zero <- function(x) {
#   x <- ifelse(x<0,0,x)
#   return(x)
# }
# 
# help_Formula<-function(meanAlpha, meanAlphaAge, meanAlphaAge2,age){
#   help<-meanAlpha + meanAlphaAge*age + meanAlphaAge2*age*age
#   help<-ifelse(help<0,0,help)
#   return(help)}
# 
# dispersal_Formula<-function(meanBeta, meanBetaAge){
#   dispersal = 1 / (1 + exp(meanBetaAge*age - meanBeta))
#   return(dispersal)}
# 
#   age<-seq(from=1,by=1, length=11)
# 
#   meanAlpha<-do_mean_LG(GA$meanAlpha, 100000)
#   meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, 100000)
#   meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, 100000)
#   meanBeta<-do_mean_LG(GA$meanBeta, 100000)
#   meanBetaAge<-do_mean_LG(GA$meanBetaAge, 100000)
# 
# ggplot(data.frame(x=seq(from=1,by=1, length=11)),aes(age))+
#   stat_function(fun = help_Formula(meanAlpha, meanAlphaAge, meanAlphaAge2,age))
# 
# 
# 
# 
# 
# plot(age, help_Formula(meanAlpha, meanAlphaAge, meanAlphaAge2), type="l", col="red", lwd=2, xlab="Age", ylab="",main="Generation 10000", ylim=range(min=0, max=1))
# mtext("Help", side=2, col="red", line=2.5)
# par(new = T)
# dispersalP<-plot(age,dispersal_Formula(meanBeta, meanBetaAge), type="l", col="blue", axes = FALSE, lwd=2,  bty = "n", lty = 5, xlab=NA, ylab=NA, ylim=range(min=0, max=1))
# mtext("Dispersal", side=4, col="blue", line=10)
# axis(side = 4)
# 
# plot(age, help_Formula(meanAlpha, meanAlphaAge, meanAlphaAge2), by= GA$Replica, type="l", col="red", lwd=4, xlab="Age", ylab="Help",main="Last generation", ylim=range(min=0, max=1))#, ylim=range(min=0, max=1.5)
# 
# plot(age ~ help_Formula(meanAlpha, meanAlphaAge, meanAlphaAge2,age), data=GA)


