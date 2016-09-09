#########################################################
########## SOIL N15 #####################################
#########################################################

setwd("I:/EXPERIMENTS/N15 Expt")

soilN15<-read.csv('Data/Soil/N15soilcores.csv')

### Bag Weights (g)
Bag1<-8.07 #g
Bag2<-17.73 #g
###################
Bag3<- 14.6 # UNKNOWN
###################
Bag4<-13.29 #g
Bag5<-25.8 #g
Bag6<-32.95 #g
Bag7<-35.46 #g

### % N derived from fertiliser ########### THIS IS WHAT YOU WANT
Ao<-0.3663
soilN15$X.FertN15<-NA
soilN15$X.FertN15[soilN15$Plot=="N15"]<-8.57-Ao #N15 application rate %N15 over N15 plot 
  # dilution by 52N (original enrichment 11.04%)
soilN15$X.FertN15[soilN15$Plot=="QUT"]<-35.22-Ao #N15 application rate %N15 over QUT plots 

soilN15$Ndff<-(soilN15$X.AtomN15-Ao)/(soilN15$X.FertN15) # proportion of N in soil fractions that was derived from the labelled fertiliser

soilN15$Tare.g<-NA
soilN15$Tare.g[soilN15$TareBag==1]<-Bag1
soilN15$Tare.g[soilN15$TareBag==2]<-Bag2
soilN15$Tare.g[soilN15$TareBag==3]<-Bag3
soilN15$Tare.g[soilN15$TareBag==4]<-Bag4
soilN15$Tare.g[soilN15$TareBag==5]<-Bag5
soilN15$Tare.g[soilN15$TareBag==6]<-Bag6
soilN15$Tare.g[soilN15$TareBag==7]<-Bag7

soilN15$WW.g<-soilN15$Tare.WW.g-soilN15$Tare.g
soilN15$DW.g<-soilN15$Tare.DW.g-soilN15$Tare.g
#not all DWs are available... so use proxy ratios? I assume that soil cores collected at the same time will have similar moisture contents at the same depth

######### PROXYS
soilN15[is.na(soilN15$DW.g),]
soilN15$Depth

#mean(soilN15[soilN15$Depth=="0-10"|soilN15$Depth=="10-20"|soilN15$Depth=="20-30"|soilN15$Depth=="30-40"|soilN15$Depth=="40-50",
#        c("Ndff","Depth")]$Ndff,na.rm=TRUE)

#########
soilN15$TN.g.g<-(soilN15$TN.mg/1000)/soilN15$N15Wt.g # %TN in samples (g/g)
soilN15$totalTN<-soilN15$TN.g.g*soilN15$DW.g # (g)
soilN15$FertN<-soilN15$totalTN*soilN15$Ndff # (g)
soilN15$Ndfs<-1-soilN15$Ndff
soilN15$SoilN<-soilN15$totalTN*soilN15$Ndfs # (g)


#########
soilN15cores<-aggregate(cbind(FertN,SoilN)~Site+Plot+Core,data=soilN15,FUN=sum,na.action=na.pass,na.rm=TRUE)
soilSA<-pi*((44.5/1000)/2)^2 #surface area of core (m2) # inner diameter of soil corer: 44.5mm
soilSA.ha<-soilSA/10000
soilN15cores$FertNkg.ha<-(soilN15cores$FertN/1000)/soilSA.ha # kg/ha
soilN15cores$SoilNkg.ha<-(soilN15cores$SoilN/1000)/soilSA.ha # kg/ha

write.csv(soilN15cores,"Outputs/SoilN15.csv")

NSOIL<-aggregate(cbind(FertNkg.ha,SoilNkg.ha)~Plot+Site,data=soilN15cores,FUN=mean)
((NSOIL[NSOIL$Plot=="N15"&NSOIL$Site=="hill",]$FertNkg.ha*0.5+
  NSOIL[NSOIL$Plot=="N15"&NSOIL$Site=="water",]$FertNkg.ha*0.25+
  NSOIL[NSOIL$Plot=="N15"&NSOIL$Site=="wheel",]$FertNkg.ha*0.25)+NSOIL[NSOIL$Plot=="QUT",]$FertNkg.ha)/2

soilN<-aggregate(FertNkg.ha~Plot,data=soilN15cores,FUN=mean)

#########
SOIL<-soilN15[,c("Site","Plot","Depth","FertN")]
SOIL$FertNkg.ha<-(SOIL$FertN/1000)/soilSA.ha # kg/ha

SOIL$Depth<-as.character(SOIL$Depth)
SOIL$D<-gsub("-.*","",SOIL$Depth)
SOIL$D<-paste("-",SOIL$D,sep="")
SOIL$D<-as.numeric(SOIL$D)

soil<-aggregate(FertNkg.ha~Depth+Site+D,data=SOIL,FUN=mean)
soil.se<-aggregate(FertNkg.ha~Depth+Site+D,data=SOIL,FUN=sd)
soil.se$FertN.se<-soil.se$FertNkg.ha/3

library("RColorBrewer")
sequential<-brewer.pal(4,"RdYlBu")

png(file="Outputs/Ndistributionsoil_.png",
    width=90,height=160,units="mm",res=300,bg="white")
#png(file="Outputs/Ndistributionsoil_.png",
#    width=80,height=160,units="mm",res=1200,bg="white")
#jpeg(file="Outputs/Ndistributionsoil.jpg",
#    width=100,height=160,units="mm",res=1200,bg="white")
par(mfrow=c(1,1),mar=c(3,4,0.7,0.5),mgp=c(1.7,0.3,0),family="serif",bg=NA)
plot(D~FertNkg.ha,data=soil,
     type="n",ylab="",xlab="Fertiliser N (kg/ha)",
     yaxt="n",xaxt="n",
     ylim=c(-200,0),xlim=c(0,140))
axis(2,at = seq(from=-190,to=0,by=10),labels = c("190-200","180-190","170-180","160-170","150-160","140-150","130-140","120-130","110-120",
                                                 "100-110","90-100","80-90","70-80","60-70","50-60","40-50","30-40","20-30","10-20","0-10"),line=0.1,
     lwd =0.6,tick=FALSE,cex.axis=0.8,las=1)
axis(2,at = seq(from=-190,to=0,by=10),labels = NA,
     lwd = 0.6,tck=-0.01)
mtext(side = 2,text = "Depth (m)",line = 3)
axis(1,at=seq(from=0,to=140,by=20),labels=c("0","20","40","60","80","100","120","140"),
     lwd =0.6,tick=FALSE,cex.axis=0.8,las=1)
axis(1,at = seq(from=0,to=140,by=20),labels = NA,
     lwd = 0.6,tck=-0.01)
arrows(length=0.05,code = 3,angle = 90,x0 = soil$FertNkg.ha[soil$Site=="hill"]-soil.se$FertN.se[soil$Site=="hill"],x1= soil$FertNkg.ha[soil$Site=="hill"]+soil.se$FertN.se[soil$Site=="hill"],
       y0 = soil$D[soil$Site=="hill"],y1=soil$D[soil$Site=="hill"])
points(D~FertNkg.ha,data=soil[soil$Site=="hill",],col=sequential[1],pch=19)
arrows(length=0.05,code = 3,angle = 90,x0 = soil$FertNkg.ha[soil$Site=="water"]-soil.se$FertN.se[soil$Site=="water"],x1= soil$FertNkg.ha[soil$Site=="water"]+soil.se$FertN.se[soil$Site=="water"],
       y0 = soil$D[soil$Site=="water"],y1=soil$D[soil$Site=="water"])
points(D~FertNkg.ha,data=soil[soil$Site=="water",],col=sequential[3],pch=19)
arrows(length=0.05,code = 3,angle = 90,x0 = soil$FertNkg.ha[soil$Site=="wheel"]-soil.se$FertN.se[soil$Site=="wheel"],x1= soil$FertNkg.ha[soil$Site=="wheel"]+soil.se$FertN.se[soil$Site=="wheel"],
       y0 = soil$D[soil$Site=="wheel"],y1=soil$D[soil$Site=="wheel"])
points(D~FertNkg.ha,data=soil[soil$Site=="wheel",],col=sequential[4],pch=19)
legend("bottomright",inset=c(0.02,0.02),col = c(sequential[1],sequential[3],sequential[4]),pch=19,
       legend = c("Hill","Water furrow","Wheel furrow"),cex=0.8)
dev.off()

