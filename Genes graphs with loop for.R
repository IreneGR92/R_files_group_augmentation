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
library(ggpubr)
library(magrittr)

#directory<-"~/Documents/Model/Results/" #Linux
#directory<-"H:\\PhD\\CODE\\All_results\\txt_files\\16.06.19\\NRN\\"  #Work 
#directory<-"C:\\Users\\ig17c521\\Documents\\Group-augmentation-Cplusplus\\results\\"  #Work
directory<-"C:\\Users\\igaru\\Documents\\PhD\\CODE\\All_results\\txt_files\\RN\\"  #Home

getwd()

#Main
files_main<-list.files(paste(directory, "main",sep=""), pattern=NULL, all.files=FALSE,full.names=FALSE)
dim_files_main<-dim.data.frame(files_main)


i=0
while (i < dim_files_main[2]) {

i=i+1

nameFile<-files_main[i] #delete the .txt
nameFile<-gsub('.{4}$', '', nameFile)
nameFile<-substring(nameFile, 20)


setwd(paste(directory, "main",sep=""))
Parameters<-read.table(paste("group_augmentation_", nameFile, ".txt",sep=""), skip=1, nrows=29)
Parameters <- mutate(Parameters, V3 = paste(Parameters[,1], Parameters[,2])) 
GA<-read.table(paste("group_augmentation_", nameFile, ".txt",sep=""),header = TRUE,skip=31)


#Last generation
setwd(paste(directory, "last_generation",sep=""))
GA2<-read.table(paste("group_augmentation_last_generation_", nameFile, ".txt",sep=""), header = TRUE, skip=31) 
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

#For the last generation, after the evolution of help
meanAlpha<-do_mean_LG(GA$meanAlpha, gen_with_help)
meanAlphaAge<-do_mean_LG(GA$meanAlphaAge, gen_with_help)
meanAlphaAge2<-do_mean_LG(GA$meanAlphaAge2, gen_with_help)
meanBeta<-do_mean_LG(GA$meanBeta, gen_with_help)
meanBetaAge<-do_mean_LG(GA$meanBetaAge, gen_with_help)
meanAge<-do_mean_LG(GA$Age, gen_with_help)
meanGroupSize<-do_mean_LG(GA$Group_size, gen_with_help)
meanHelp<-do_mean_LG(GA$meanHelp, gen_with_help)
meanDispersal<-do_mean_LG(GA$meanDispersal, gen_with_help)
meanSurvival<-do_mean_LG(GA$meanSurvival, gen_with_help)
meanRelatedness<-do_mean_LG(GA$Relatedness, gen_with_help)
meanCorr_Help_Disp<-do_mean_LG(GA$corr_Help_Disp, gen_with_help)
meanPropFloatBreeder<-do_mean_LG(GA$propFloatBreeder, gen_with_help)

SD_Alpha<-do_SD_LG(GA$meanAlpha, gen_with_help)
SD_AlphaAge<-do_SD_LG(GA$meanAlphaAge, gen_with_help)
SD_AlphaAge2<-do_SD_LG(GA$meanAlphaAge2, gen_with_help)
SD_Beta<-do_SD_LG(GA$meanBeta, gen_with_help)
SD_BetaAge<-do_SD_LG(GA$meanBetaAge, gen_with_help)
SD_Age<-do_SD_LG(GA$Age, gen_with_help)
SD_GroupSize<-do_SD_LG(GA$Group_size, gen_with_help)
SD_Help<-do_SD_LG(GA$meanHelp, gen_with_help)
SD_Dispersal<-do_SD_LG(GA$meanDispersal, gen_with_help)
SD_Survival<-do_SD_LG(GA$meanSurvival, gen_with_help)
SD_Relatedness<-do_SD_LG(GA$Relatedness, gen_with_help)
SDcorr_Help_Disp<-do_SD_LG(GA$corr_Help_Disp, gen_with_help)
SD_PropFloatBreeder<-do_SD_LG(GA$propFloatBreeder, gen_with_help)

#For the las generation before the evolution of help
# meanBetaNH<-do_mean_LG(GA$meanBeta, gen_without_help)
# meanBetaAgeNH<-do_mean_LG(GA$meanBetaAge, gen_without_help)
# meanAgeNH<-do_mean_LG(GA$Age, gen_without_help)
# meanGroupSizeNH<-do_mean_LG(GA$Group_size, gen_without_help)
# meanHelpNH<-do_mean_LG(GA$meanHelp, gen_without_help)
# meanDispersalNH<-do_mean_LG(GA$meanDispersal, gen_without_help)
# meanSurvivalNH<-do_mean_LG(GA$meanSurvival, gen_without_help)
# meanRelatednessNH<-do_mean_LG(GA$Relatedness, gen_without_help)
# meanPropFloatBreederNH<-do_mean_LG(GA$propFloatBreeder, gen_without_help)
# 
# SD_BetaNH<-do_SD_LG(GA$meanBeta, gen_without_help)
# SD_BetaAgeNH<-do_SD_LG(GA$meanBetaAge, gen_without_help)
# SD_AgeNH<-do_SD_LG(GA$Age, gen_without_help)
# SD_GroupSizeNH<-do_SD_LG(GA$Group_size, gen_without_help)
# SD_HelpNH<-do_SD_LG(GA$meanHelp, gen_without_help)
# SD_DispersalNH<-do_SD_LG(GA$meanDispersal, gen_without_help)
# SD_SurvivalNH<-do_SD_LG(GA$meanSurvival, gen_without_help)
# SD_RelatednessNH<-do_SD_LG(GA$Relatedness, gen_without_help)
# SD_PropFloatBreederNH<-do_SD_LG(GA$propFloatBreeder, gen_without_help)

#For each replica
meanAlphaR<-GA$meanAlpha[GA$Generation==100000]
meanAlphaAgeR<-GA$meanAlphaAge[GA$Generation==100000]
meanAlphaAge2R<-GA$meanAlphaAge2[GA$Generation==100000]
meanBetaR<-GA$meanBeta[GA$Generation==100000]
meanBetaAgeR<-GA$meanBetaAge[GA$Generation==100000]
meanAgeR<-GA$Age[GA$Generation==100000]
meanGroupSizeR<-GA$Group_size[GA$Generation==100000]
meanHelpR<-GA$meanHelp[GA$Generation==100000]
meanDispersalR<-GA$meanDispersal[GA$Generation==100000]
meanSurvivalR<-GA$meanSurvival[GA$Generation==100000]
meanRelatednessR<-GA$Relatedness[GA$Generation==100000]
meanCorr_Help_DispR<-GA$corr_Help_Disp[GA$Generation==100000]
meanPropFloatBreederR<-GA$propFloatBreeder[GA$Generation==100000]



descriptives <- data.frame(Variable=c("alpha", "alphaAge", "alphaAge2",
                                      "beta", "betaAge", 
                                      "Help","Dispersal", "Survival", "Relatedness",
                                      "age", "Group_size",
                                      "Help_Disp", "propFloaterB"),
                           Mean=c(meanAlpha, meanAlphaAge, meanAlphaAge2,
                                  meanBeta, meanBetaAge, 
                                  meanHelp,meanDispersal,meanSurvival,meanRelatedness,
                                  meanAge,meanGroupSize,
                                  meanCorr_Help_Disp, meanPropFloatBreeder),
                           SD=c(SD_Alpha,SD_AlphaAge,SD_AlphaAge2,
                                SD_Beta,SD_BetaAge,
                                SD_Help,SD_Dispersal,SD_Survival,SD_Relatedness,
                                SD_Age, SD_GroupSize,
                                SDcorr_Help_Disp, SD_PropFloatBreeder))

# descriptivesNH <- data.frame(Variable=c("beta", "betaAge", 
#                                       "Help","Dispersal", "Survival", "Relatedness",
#                                       "age", "Group_size","propFloaterB"),
#                            Mean=c(meanBetaNH, meanBetaAgeNH, 
#                                   meanHelpNH,meanDispersalNH,meanSurvivalNH,meanRelatednessNH,
#                                   meanAgeNH,meanGroupSizeNH,meanPropFloatBreederNH),
#                            SD=c(SD_BetaNH,SD_BetaAgeNH,
#                                 SD_HelpNH,SD_DispersalNH,SD_SurvivalNH,SD_RelatednessNH,
#                                 SD_AgeNH, SD_GroupSizeNH,SD_PropFloatBreederNH))



descriptivesR <- data.frame( ID=c(nameFile),
                             X0=c(Parameters[11,2]),
                             Xh=c(Parameters[12,2]),
                             Xn=c(Parameters[13,2]),
                             K1=c(Parameters[15,2]),
                             Bias=c(Parameters[10,2]),
                             Help=c(meanHelpR),
                             Dispersal=c(meanDispersalR),
                             Survival=c(meanSurvivalR),
                             Relatedness=c(meanRelatednessR),
                             Age=c(meanAgeR),
                             Group_size=c(meanGroupSizeR),
                             Help_Disp=c(meanCorr_Help_DispR),
                             propFloaterB=c(meanPropFloatBreederR))


write.xlsx(descriptives, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Results", append = FALSE)# append TRUE to create a new sheet in the same file
#write.xlsx(descriptivesNH, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Result before help", append = TRUE)
write.xlsx(descriptivesR, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Result per replica", append = TRUE)
write.xlsx(Parameters, paste(directory, "results_", nameFile, ".xlsx",sep=""), sheetName = "Parameters", append = TRUE)




###################### FORMULAS LAST GENERATION #################################


replace_with_zero_if_below_zero <- function(x) {
  x <- ifelse(x<0,0,x)
  return(x)
}

###Help

help_Formula<-function(){
  help <- GA2$alpha+GA2$alphaAge*GA2$age+GA2$alphaAge2*GA2$age*GA2$age
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




########################################## PLOTS ##############################################################

pdf(paste(directory, "graphs_", nameFile, ".pdf", sep="")) # Open a pdf file

#Dummy plot to print initial parameters in the simulation
par(mfrow = c(1, 1))
plot(0.5, 0.5,  xlab=" ", ylab=" ", type="n")
op <- par(cex = 0.79)
legend(x="bottomleft", legend = Parameters[,3], pch=1)
title(nameFile)
par(mfrow = c(1, 1))

##Help plot
p1<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanHelp)) +
  geom_ribbon(aes(ymin=GA_means$meanHelp-GA_SD$meanHelp, ymax=GA_means$meanHelp+GA_SD$meanHelp),
              alpha=0.3) +
  geom_line(color="red", size=1)+
  xlab("Generation")+ ylab("Help")
  #+coord_cartesian(ylim = c(0.049, 1))


##Dispersal plot
p2<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanDispersal)) +
  geom_ribbon(aes(ymin=GA_means$meanDispersal-GA_SD$meanDispersal, ymax=GA_means$meanDispersal+GA_SD$meanDispersal),
              alpha=0.3) +
  geom_line(color="blue", size=1)+
  xlab("Generation")+ ylab("Dispersal")+
  coord_cartesian(ylim = c(0.049, 1))

##Survival
p3<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanSurvival)) +
  geom_ribbon(aes(ymin=GA_means$meanSurvival-GA_SD$meanSurvival, ymax=GA_means$meanSurvival+GA_SD$meanSurvival),
              alpha=0.3) +
  geom_line(color="black", size=1)+
  xlab("Generation")+ ylab("Survival")+
  coord_cartesian(ylim = c(0.049, 1))

##Relatedness plot
p4<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$Relatedness)) +
  geom_ribbon(aes(ymin=GA_means$Relatedness-GA_SD$Relatedness, ymax=GA_means$Relatedness+GA_SD$Relatedness),
              alpha=0.3) +
  geom_line(color="orange", size=1)+
  xlab("Generation")+ ylab("Relatedness")+
  coord_cartesian(ylim = c(0.049, 1))


##Population stability?
p5<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$Group_size)) +
  geom_ribbon(aes(ymin=GA_means$Group_size-GA_SD$Group_size, ymax=GA_means$Group_size+GA_SD$Group_size),
              alpha=0.3) +
  geom_line(color="purple", size=1)+
  xlab("Generation")+ ylab("Group size")


##Cummulative help in fecundity
p6<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$meanCumHelp)) +
  geom_ribbon(aes(ymin=GA_means$meanCumHelp-GA_SD$meanCumHelp, ymax=GA_means$meanCumHelp+GA_SD$meanCumHelp),
              alpha=0.3) +
  geom_line(color="red", size=1)+
  xlab("Generation")+ ylab("Cummulative help")

##Proportion of floaters that become breeders compared to helpers plot
p7<-ggplot(GA_means, aes(x=GA_means$Generation, y=GA_means$propFloatBreeder)) +
  geom_ribbon(aes(ymin=GA_means$propFloatBreeder-GA_SD$propFloatBreeder, ymax=GA_means$propFloatBreeder+GA_SD$propFloatBreeder),
              alpha=0.3) +
  geom_line(color="green", size=1)+
  xlab("Generation")+ ylab("Prop. floaters->breeders")+
  coord_cartesian(ylim = c(0.049, 1))




grid.arrange(p1, p2, p6, p4, p5, p3, nrow = 3)


########### REACTION NORMS ####################

# par(mfrow = c(3, 2))
# age<-seq(from=1,by=1, length=11)
# 
# # HELP
# if(Parameters[1,2]==1){
# replace_with_zero_if_below_zero <- function(x) {
#   x <- ifelse(x<0,0,x)
#   return(x)
# }
# 
# help_Formula<-function(meanAlpha, meanAlphaAge, meanAlphaAge2){
#   help<-meanAlpha + meanAlphaAge*age + meanAlphaAge2*age*age
#   help<-ifelse(help<0,0,help)
#   return(help)}
# 
# helpP<-plot(age, help_Formula(meanAlpha, meanAlphaAge, meanAlphaAge2), type="l", col="red", lwd=4, xlab="Age", ylab="Help", ylim=range(min=0, max=1))#, ylim=range(min=0, max=1.5)
# }
# 
# # DISPERSAL
# 
# if(Parameters[2,2]==1){
# dispersal<-1 / (1 + exp(meanBetaAge*age - meanBeta))
# dispersalP<-plot(age,dispersal, type="l", col="blue", lwd=3, xlab="Age", ylab="Dispersal", ylim=range(min=0, max=1))
# }



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

