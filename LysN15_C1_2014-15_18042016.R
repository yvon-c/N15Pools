#########################################################
########## LYSIMETER N15 ################################
#########################################################

setwd("I:/EXPERIMENTS/N15 Expt")

#########################################################

LYS<-read.csv("Data/DeepDrainage/LysimeterChemistry.csv")
Lys<-aggregate(cbind(N15Volume.mL,N15TN.mg,X..AtomN15,TN.mg.L,Water.Volume..mL.)~SampleID+Crop+Tray,data=LYS,FUN=mean,na.action=na.pass,na.rm=TRUE)
Lys$N15.TNmg.mL<-Lys$N15TN.mg/(Lys$N15Volume.mL/1000) #mg/L #### THESE VALUES ARE WAY TOO HIGH, DISCARD

Lys$TotN.kg<-Lys$TN.mg.L*(Lys$Water.Volume..mL./1000)/1000/1000 # kg

Ao<-0.3663 #(% N15 natural abundance)
Lys$Ndff<-(Lys$X..AtomN15-Ao)/(35.16-Ao)*100
Lys$N15N.kg<-Lys$TotN.kg*(Lys$X..AtomN15-Ao) # kg
Lys$N15N.kgha<-Lys$N15N.kg/1.575254*10000
N15applied.kgha<-180*(35.16/100) # 35.16% enrichment (kg/ha)
Lys$PercentageFertN<-Lys$N15N.kgha/N15applied.kgha
Lys$FertN.kgha<-Lys$PercentageFertN/100*(180+52)


FertNdrainage<-sum(Lys$FertN.kgha,na.rm=TRUE) # total Fert N lost up to N15 anode replacement
totalNdrainage<-sum(Lys$TotN.kg,na.rm=TRUE)/(1.575254)*10000 # total N lost through drainage up to anode replacement
soilNdrainage<-totalNdrainage-FertNdrainage # soil N lost (total - fert)
