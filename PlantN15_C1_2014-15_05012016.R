#########################################################
########## PLANT UPTAKE N15 ##############################
#########################################################

setwd("I:/EXPERIMENTS/N15 Expt")

#plantN15<-read.csv("Data/Plant/cottonN15.csv")
plantN15<-read.csv("Data/Plant/plantN15.csv")
AA<-aggregate(cbind(Sample.Weight.g, TN.mg, TC.mg)~Plot+Plant+Plant.Section,data=plantN15, FUN=sum)
BB<-aggregate(cbind(X.TN, X.AtomN15, X.TC, X.AtomC13)~Plot+Plant+Plant.Section,data=plantN15, FUN=mean)
PlantN15<-merge(AA, BB)

cottonbiomass<-read.csv("Data/Plant/cottonbiomassN15.csv")
cottonbiomass$DW.g<-cottonbiomass$DW.Tare.g-cottonbiomass$Tare.g
cottonbiomass$WW.g<-cottonbiomass$WW.Tare.g-cottonbiomass$Tare.g
head(cottonbiomass)
PlantBiomass<-aggregate(cbind(DW.g, WW.g)~Plot+Plant.Section,data=cottonbiomass, FUN=sum)

cottonlintseed<-read.csv("Data/Plant/cottonlintseedN15.csv")
PlantLS<-aggregate(cbind(DW.LS.g, Lint.g, Seed.g)~Plot+Plant.Section,data=cottonlintseed, FUN=sum)
PlantL<-PlantLS[,c("Plot","Plant.Section","Lint.g")]
names(PlantL)<-c("Plot","Plant.Section","DW.g")
PlantL$Plant.Section<-c("Lint","Lint")
PlantS<-PlantLS[,c("Plot","Plant.Section","Seed.g")]
names(PlantS)<-c("Plot","Plant.Section","DW.g")
PlantS$Plant.Section<-c("Seed","Seed")
plantLS<-rbind(PlantL, PlantS)
plantLS$WW.g<-NA

PlantBio<-rbind(PlantBiomass, plantLS)

### NOTE: CHECK N15 natural abundance
CottonN15<-merge(PlantBio,PlantN15[PlantN15$Plant=="Cotton",])

#############################
Ao<-0.3663 #(% N15 natural abundance)
#############################
CottonN15$Fert<-NA
CottonN15$Fert[CottonN15$Plot=="LYS"]<-27.28-Ao #Over 9m2 above lysimeter (35.06% N15 enrichment over lysimeter)
  # dilution of fertiliser with additional 52N added => 27.28 % (35.16% original application N15)
CottonN15$Fert[CottonN15$Plot=="N15"]<-8.57-Ao #Over 3m2 (N15 plot) (11.04% N15 enrichment over N15 plot)
  # dilution of fertiliser with additional 52N (average plot rate 80N) (11.04% original application N15)
CottonN15$Ndff<-100*(CottonN15$X.AtomN15-Ao)/CottonN15$Fert # %N derived from fertiliser
CottonN15$Ndfs<-100-CottonN15$Ndff
CottonN15$Yield[CottonN15$Plot=="LYS"]<-(CottonN15$DW.g[CottonN15$Plot=="LYS"]/1000)/(9/10000) #kg/ha
CottonN15$Yield[CottonN15$Plot=="N15"]<-(CottonN15$DW.g[CottonN15$Plot=="N15"]/1000)/(6/10000) #kg/ha
CottonN15$TotNyield<-(CottonN15$Yield)*(CottonN15$X.TN/100) #kgN/ha
CottonN15$FertNyield<-CottonN15$TotNyield*(CottonN15$Ndff/100) #kg fert N/ha

write.csv(CottonN15[,c('Plot','Plant.Section','DW.g','Sample.Weight.g','TN.mg','X.TN','X.AtomN15','Fert','Ndff','Yield','TotNyield','FertNyield')],
          "Outputs/N15cottonsummary2.csv")

plantplot<-aggregate(cbind(Yield,TotNyield,FertNyield)~Plot,data=CottonN15,FUN=sum,na.action=na.pass) #Total plant biomass, N uptake

mean(plantplot$FertNyield)


##### PLOTS
CottPlot<-cbind(CottonN15[CottonN15$Plot=="LYS",c("Plant.Section","Plot","FertNyield")],CottonN15[CottonN15$Plot=="N15",c("Plant.Section","Plot","FertNyield")])
names(CottPlot)<-c("Plant.Section","LYS","LYS.NFert","Plant.Section","N15","N15.NFert")
t(CottPlot[,c("Plant.Section","LYS.NFert","N15.NFert")])

png(file="Outputs/CottonN.png",
    width=160,height=80,units="mm",res=300,bg="white")
#png(file="Outputs/CottonN.png",
#    width=160,height=80,units="mm",res=1200,bg="white")
#jpeg(file="Outputs/CottonN.jpg",
#    width=160,height=80,units="mm",res=1200,bg="white")

par(mfrow=c(1,1),mar=c(2,3,0.7,0.5),mgp=c(1.7,0.3,0),family="serif",bg=NA)
barplot(height=t(CottPlot[,c("LYS.NFert","N15.NFert")]),beside=T,
        ylim=c(0,60),
        names.arg=CottPlot$Plant.Section,
        col=c("sky blue","orange"),
        cex.names=0.8,cex.axis=0.8,
        yaxt="n"
        )
box(lwd=0.6)
axis(2,at = seq(from=0,to=60,by=10),labels = c("0","10","20","30","40","50","60"),line=0,
     lwd = 0.6,tick=FALSE,cex.axis=0.8)
axis(2,at = seq(from=0,to=60,by=10),labels = NA,
     lwd = 0.6,tck=-0.01)
mtext(text=expression(paste("Fertiliser N Uptake (kg/ha)")),side=2,
      cex=0.9,outer = FALSE,line=1.5)
legend("topleft",inset=c(0.02,0.02),legend=c("Lysimeter Plot","N15 Plot"),col=c("sky blue","light orange"),cex=0.8,fill = c("sky blue","orange"))
dev.off()

#################
###########################################
##########################################

####### UPDATE TO INCLUDE N15 COTTON STUBBLE RETURNED TO THE PLOT



### WHEAT
WheatBio<-read.csv("Data/Plant/wheatbiomassN15.csv")
WheatN15<-merge(WheatBio,PlantN15[PlantN15$Plant=="Wheat",])

Ao<-0.3663 #(% N15 natural abundance)
#############################
WheatN15$Fert<-NA
WheatN15$Fert[WheatN15$Plot=="LYS"]<-27.28-Ao #Over 9m2 above lysimeter (35.06% N15 enrichment over lysimeter)
WheatN15$Fert[WheatN15$Plot=="N15"]<-8.57-Ao #Over 3m2 (N15 plot) (11.04% N15 enrichment over N15 plot)
WheatN15$Ndff<-100*(WheatN15$X.AtomN15-Ao)/WheatN15$Fert # %N derived from fertiliser
WheatN15$Ndfs<-100-WheatN15$Ndff
WheatN15$Yield[WheatN15$Plot=="LYS"]<-(WheatN15$DW.g[WheatN15$Plot=="LYS"]/1000)/(9/10000) #kg/ha
WheatN15$Yield[WheatN15$Plot=="N15"]<-(WheatN15$DW.g[WheatN15$Plot=="N15"]/1000)/(4/10000) #kg/ha
WheatN15$TotNyield<-(WheatN15$Yield)*(WheatN15$X.TN/100) #kgN/ha
WheatN15$FertNyield<-WheatN15$TotNyield*(WheatN15$Ndff/100) #kg fert N/ha

WheatPlot<-cbind(WheatN15[WheatN15$Plot=="LYS",c("Plant.Section","Plot","FertNyield")],WheatN15[WheatN15$Plot=="N15",c("Plant.Section","Plot","FertNyield")])
names(WheatPlot)<-c("Plant.Section","LYS","LYS.NFert","Plant.Section","N15","N15.NFert")
t(WheatPlot[,c("Plant.Section","LYS.NFert","N15.NFert")])

png(file="Outputs/WheatN.png",
    width=160,height=80,units="mm",res=300,bg="white")
#png(file="Outputs/WheatN.png",
#    width=160,height=80,units="mm",res=1200,bg="white")
#jpeg(file="Outputs/WheatN.jpg",
#    width=160,height=80,units="mm",res=1200,bg="white")

par(mfrow=c(1,1),mar=c(2,3,0.7,0.5),mgp=c(1.7,0.3,0),family="serif",bg=NA)
barplot(height=t(WheatPlot[,c("LYS.NFert","N15.NFert")]),beside=T,
        ylim=c(0,10),
        names.arg=WheatPlot$Plant.Section,
        col=c("sky blue","orange"),
        cex.names=0.8,cex.axis=0.8,
        yaxt="n"
)
box(lwd=0.6)
axis(2,at = seq(from=0,to=10,by=2),labels = c("0","2","4","6","8","10"),line=0,
     lwd = 0.6,tick=FALSE,cex.axis=0.8)
axis(2,at = seq(from=0,to=10,by=2),labels = NA,
     lwd = 0.6,tck=-0.01)
mtext(text=expression(paste("Fertiliser N Uptake (kg/ha)")),side=2,
      cex=0.9,outer = FALSE,line=1.5)
legend("topleft",inset=c(0.02,0.02),legend=c("Lysimeter Plot","N15 Plot"),col=c("sky blue","light orange"),cex=0.8,fill = c("sky blue","orange"))
dev.off()

write.csv(WheatN15[,c('Plot','Plant.Section','DW.g','Sample.Weight.g','TN.mg','X.TN','X.AtomN15','Fert','Ndff','Yield','TotNyield','FertNyield')],
          "Outputs/N15wheatsummary2.csv")
