#########################################################
########## SURFACE WATER N15 ############################
#########################################################

setwd("I:/EXPERIMENTS/N15 Expt")

#########################################################
############## FLUME DATA ###############################
#########################################################

##### #1 IMPORT DATA ####################################
flume.cal<-read.csv("Flumes/Data/flume.cal.csv")
flume.field<-read.csv("Flumes/Data/flume.fieldnotes1.csv")
guna.flume<-read.csv("Flumes/Data/Guna_Flumedatatimes.csv")

##### #1a FLUME CAL DATA ################################
flume.cal$AirReading<-rowMeans(flume.cal[,c("AirReading1..units.","AirReading2..units.","AirReading3..units.")],na.rm=TRUE)
flume.cal$CountZero<-rowMeans(flume.cal[,c("CountZeroHt1","CountZeroHt2")],na.rm=TRUE)
flume.cal$Slope<-(flume.cal$Depth.P2..mm.-flume.cal$Depth.P1..mm.)/(flume.cal$Reading2..units.-flume.cal$Reading1..units.)
flume.cal$int<- 
  # intercept calculated in R same as excel, looks fine
  flume.cal$Depth.P2..mm.-flume.cal$Reading2..units.*flume.cal$Slope

flume<-merge(flume.cal,flume.field,by="Probe.SN",all.x=TRUE)

flume$Zero.Ht.mm<-flume$CountZeroSill*flume$Slope+flume$int
flume$Ftype<-flume$Furrow.x
flume$Ftype[flume$Furrow.x==4|flume$Furrow.x==6|flume$Furrow.x==8]<-"skip" #These stayed the same throughout the season
flume$Ftype[flume$Furrow.x==5|flume$Furrow.x==7|flume$Furrow.x==9]<-"water" #These stayed the same throughout the season
flume$id<-paste(flume$Irrigation,"_",flume$Probe.SN,sep="")

Y.flume<-flume[!flume$Remove=="Y",]  #good flume data
Y.flume$fullid<-paste(Y.flume$Irrigation,"_",Y.flume$Ftype," ",Y.flume$Up.Down," ",Y.flume$Probe.SN,sep="")
Y.flume<-Y.flume[!is.na(Y.flume$Probe.SN),]
#Y.flume<-na.omit(Y.flume)


##### #1b IRRIGATION START/END ##########################
guna.flume$Date<-as.character(guna.flume$Date)
guna.flume$Start<-as.character(guna.flume$Start)
guna.flume$End<-as.character(guna.flume$End)
guna.flume$Start<-as.POSIXct(paste(guna.flume$Date,guna.flume$Start,sep=""),format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")
guna.flume$End<-as.POSIXct(paste(guna.flume$Date,guna.flume$End,sep=""),format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")


##### #1c CALCULTING FLUME FLOWS ########################
# Dataset:List: flow (list of dataframes for each record of logged flume data)
# Creates a list where each list object's name is based on irrigation and probe number
flow<-list()
for(i in 1:length(Y.flume$id)){
  names(flow[i])<-(Y.flume$id[i])
}
for(i in 1:length(Y.flume$id)){
  names(flow)[i]<-(Y.flume$fullid[i])
}

# Associates the list object with the logger data
for(i in 1:length(Y.flume$id)){
  flow[[i]]<-read.csv(paste("Flumes/","Data/","FlumeData/",Y.flume$id[i],".csv",sep=""))
}

# Join date and time elements and convert to as.POSIX
# NOTE: make sure the logger files have the date and time columns formated as date and time elements
for(i in 1:length(Y.flume$id)){
  flow[[i]]$Date.Time<-as.POSIXct(paste(flow[[i]]$Date,flow[[i]]$Time,sep=""),format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")
}

# New column converted counts to depth (mm)- using calibration constants
for(i in 1:length(Y.flume$id)){
  flow[[i]]$RawHt.mm<-
    flow[[i]]$Counts*Y.flume$Slope[Y.flume$id==Y.flume$id[i]]+Y.flume$int[Y.flume$id==Y.flume$id[i]] #mm
}

for(i in 1:length(Y.flume$id)){
  flow[[i]]$Ht.mm<-ifelse(
    flow[[i]]$RawHt.mm-Y.flume$Zero.Ht.mm[Y.flume$id==Y.flume$id[i]]<0,
    0,
    flow[[i]]$RawHt.mm-Y.flume$Zero.Ht.mm[Y.flume$id==Y.flume$id[i]]
  )
}  #mm

# Conversion of depth data to flowrate (L/s)
    # Constants (from manufacturer)
    m3<-#0.0007709149
  0.000725288865546218
    m2<-#0.0093841546
  0.0127458508403362
    m1<-#0.0078246935
  -0.0418138655462188

for(i in 1:length(Y.flume$id)){
  flow[[i]]$flowrate<- ifelse(flow[[i]]$Ht.mm>0,
                              m3*flow[[i]]$Ht.mm^2+m2*flow[[i]]$Ht.mm+m1, #L/s
                              0
  )
}

# Convert flowrate to incremental inflow volume (L)
# Time difference (s)
for (j in 1:length(Y.flume$id)){
  flow[[j]]$Diff.time<-difftime(flow[[j]]$Date.Time[i],flow[[j]]$Date.Time[i-1],units="secs")
}

# Volume of water flowing through at the given interval (L)
for (i in 1:length(Y.flume$id)){
  flow[[i]]$Volume<-flow[[i]]$flowrate*flow[[i]]$Diff.time
} 

# Volume to be 0 or positive, no negative values
for(i in 1:length(names(flow))){
  flow[[i]]$Volume[flow[[i]]$Volume<0]<-0
}

# Cumulative increase in volume - cumsum()
for (i in 1:length(Y.flume$id)){
  flow[[i]]$CumVolume<-cumsum(flow[[i]]$Volume)
}

# Trim individual flow data to the start and end times given by Guna's flumes for each irrigation
# IRR 1
I1<-names(flow)[grep(c("1_"), names(flow))]
IR1<-flow[I1]
for(i in 1:length(names(IR1))){
  flow[[I1[i]]]<-flow[[I1[i]]][guna.flume$Start[guna.flume$Irrigation=="1"]<flow[[I1[i]]]$Date.Time&flow[[I1[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="1"],]
}
# IRR 2
I2<-names(flow)[grep(c("2_"), names(flow))]
IR2<-flow[I2]
for(i in 1:length(names(IR2))){
  flow[[I2[i]]]<-flow[[I2[i]]][guna.flume$Start[guna.flume$Irrigation=="2"]<flow[[I2[i]]]$Date.Time&flow[[I2[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="2"],]
}
# IRR 3
I3<-names(flow)[grep(c("3_"), names(flow))]
IR3<-flow[I3]
for(i in 1:length(names(IR3))){
  flow[[I3[i]]]<-flow[[I3[i]]][guna.flume$Start[guna.flume$Irrigation=="3"]<flow[[I3[i]]]$Date.Time&flow[[I3[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="3"],]
}
# IRR 4
I4<-names(flow)[grep(c("4_"), names(flow))]
IR4<-flow[I4]
for(i in 1:length(names(IR4))){
  flow[[I4[i]]]<-flow[[I4[i]]][guna.flume$Start[guna.flume$Irrigation=="4"]<flow[[I4[i]]]$Date.Time&flow[[I4[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="4"],]
}
# IRR 5
I5<-names(flow)[grep(c("5_"), names(flow))]
IR5<-flow[I5]
for(i in 1:length(names(IR5))){
  flow[[I5[i]]]<-flow[[I5[i]]][guna.flume$Start[guna.flume$Irrigation=="5"]<flow[[I5[i]]]$Date.Time&flow[[I5[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="5"],]
}
# IRR 6
I6<-names(flow)[grep(c("6_"), names(flow))]
IR6<-flow[I6]
for(i in 1:length(names(IR6))){
  flow[[I6[i]]]<-flow[[I6[i]]][guna.flume$Start[guna.flume$Irrigation=="6"]<flow[[I6[i]]]$Date.Time&flow[[I6[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="6"],]
}
# IRR 7
I7<-names(flow)[grep(c("7_"), names(flow))]
IR7<-flow[I7]
for(i in 1:length(names(IR7))){
  flow[[I7[i]]]<-flow[[I7[i]]][guna.flume$Start[guna.flume$Irrigation=="7"]<flow[[I7[i]]]$Date.Time&flow[[I7[i]]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="7"],]
}


##### #1d VISUAL CHECK OF FLOW ##########################
# Visual Check of Flume Data: Checking plots
# Raw Counts
for (i in 1:length(Y.flume$id)){
  jpeg(paste("Flumes/","Plots/","Counts/",Y.flume$id[i],".jpg",sep="")) #plots of raw counts vs time
  par(mar=c(5.5,5.5,1,1),mgp=c(4,1,0),family="serif")
  plot(Counts~Date.Time,data=flow[[i]],ylab="Counts",xlab="Date and Time",las=2)
  
  dev.off()
}

# Water Height (mm)
for (i in 1:length(Y.flume$id)){
  jpeg(paste("Flumes/","Plots/","Hts/",Y.flume$id[i],".jpg",sep=""))
  par(mar=c(5.5,5.5,1,1),mgp=c(4,1,0),family="serif")
  plot(Ht.mm~Date.Time,data=flow[[i]],ylab="Water Height in Flumes (mm)",xlab="Date and Time",las=2)
  
  dev.off()
}

# Cumulative Volume (L)
for (i in 1:length(Y.flume$id)){
  jpeg(paste("Flumes/","Plots/","CumVol/",Y.flume$id[i],".jpg",sep=""))
  par(mar=c(5.5,5.5,1,1),mgp=c(4,1,0),family="serif")
  plot(CumVolume~Date.Time,data=flow[[i]],ylab="Cumulative Volume (L)",xlab="Date and Time",las=2)
  
  dev.off()
}


##### #1e REMOVE FUNNY FLOW DATA ########################

#### A) Based on Funny Graphs
### Remove more flume data - visiual check FAIL
funnygraph<-
  c("4_skip D 30190", # straight line from start to end, didn't quite measure the plateauing out of the flow
    "7_water D 30069", # patchy logged data so logger data removed ????
    "5_water U 20059",#again another diagonal line - not characteristic cumulative volume diagrams
    "7_skip D 20062"
    #"7_water U 30173" # no clear end point to cumulative volume curve
  )

flow[which(names(flow) %in% funnygraph)] <- NULL

#### B) Based on Volume
### There is quite a large difference between each of the flume logged data in terms of calculating total run-off water
### Rod Jackson's comments: flumes are pretty unreliable, it could be possible to have quite different amounts of water flowing through
# the different furrows but check with Guna's flow rates (from head ditch)
### Guna's flow data: I3 ~ 33000L, I4 ~ 32000L, I6 ~ 45000L for each of the irrigations for 4 furrows/2rows

lgevol<-names(flow)

for(i in 1:length(names(flow))){
  lgevol[i]<-
    ifelse(max(flow[[i]]$CumVolume)>25000,
           yes=names(flow[i]),
           no=NA)
}

lgevol<-lgevol[!is.na(lgevol)]

flow[which(names(flow) %in% lgevol)] <- NULL


##### #1e PROXY FLOW DATA SETS ##########################
# Proxy Data Sets for Flumes
# IRRIGATION 1 - SKIP (only one set of data - 1_30089)
I1_S<-names(flow)[grep(c("1_skip"), names(flow))] #subsetting flume data based on irrigation number and furrow type
IRR1_S<-flow[I1_S] #new list of the different tables based on Irrigation 1, Skip furrows
names(IRR1_S)
for(i in 1:length(names(IRR1_S))){
  IRR1_S[[i]]<-IRR1_S[[i]][guna.flume$Start[guna.flume$Irrigation=="1"]<IRR1_S[[i]]$Date.Time&IRR1_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="1"],]
}
# assigned data to each of the Irrigation 1 Skip furrow data to relevant list names
AveIRR1_S<-(do.call("rbind",IRR1_S)) #
AveI1_S<-aggregate(.~Date.Time,AveIRR1_S,FUN=mean)

# IRRIGATION 1 - WATER
I1_W<-names(flow)[grep(c("1_water"), names(flow))]
IRR1_W<-flow[I1_W]
names(IRR1_W)
for(i in 1:length(names(IRR1_W))){
  IRR1_W[[i]]<-IRR1_W[[i]][guna.flume$Start[guna.flume$Irrigation=="1"]<IRR1_W[[i]]$Date.Time&IRR1_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="1"],]
}
AveIRR1_W<-(do.call("rbind",IRR1_W)) #all the tables in this list are joined together using the function "rbind"
AveI1_W<-aggregate(.~Date.Time,AveIRR1_W,FUN=mean) #aggregate giant table based on Date.Time, so should give us an average cumulative volume for each of the furrows

AveI1_S<-AveI1_W # (no skip furrow data at all...)

# IRRIGATION 2 - SKIP
I2_S<-names(flow)[grep("2_skip", names(flow))]
IRR2_S<-flow[I2_S]
names(IRR2_S)
for(i in 1:length(names(IRR2_S))){
  IRR2_S[[i]]<-IRR2_S[[i]][guna.flume$Start[guna.flume$Irrigation=="2"]<IRR2_S[[i]]$Date.Time&IRR2_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="2"],]
}
AveIRR2_S<-(do.call("rbind",IRR2_S))
AveI2_S<-aggregate(.~Date.Time,AveIRR2_S,FUN=mean)

# IRRIGATION 2 - WATER
I2_W<-names(flow)[grep("2_water", names(flow))]
IRR2_W<-flow[I2_W]
names(IRR2_W)
for(i in 1:length(names(IRR2_W))){
  IRR2_W[[i]]<-IRR2_W[[i]][guna.flume$Start[guna.flume$Irrigation=="2"]<IRR2_W[[i]]$Date.Time&IRR2_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="2"],]
}
AveIRR2_W<-(do.call("rbind",IRR2_W))
AveI2_W<-aggregate(.~Date.Time,AveIRR2_W,FUN=mean)

# IRRIGATION 3 - SKIP
I3_S<-names(flow)[grep("3_skip", names(flow))]
IRR3_S<-flow[I3_S]
names(IRR3_S)
for(i in 1:length(names(IRR3_S))){
  IRR3_S[[i]]<-IRR3_S[[i]][guna.flume$Start[guna.flume$Irrigation=="3"]<IRR3_S[[i]]$Date.Time&IRR3_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="3"],]
}
AveIRR3_S<-(do.call("rbind",IRR3_S))
AveI3_S<-aggregate(.~Date.Time,AveIRR3_S,FUN=mean)

# IRRIGATION 3 - WATER
I3_W<-names(flow)[grep("3_water", names(flow))]
IRR3_W<-flow[I3_W]
names(IRR3_W)
for(i in 1:length(names(IRR3_W))){
  IRR3_W[[i]]<-IRR3_W[[i]][guna.flume$Start[guna.flume$Irrigation=="3"]<IRR3_W[[i]]$Date.Time&IRR3_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="3"],]
}
AveIRR3_W<-(do.call("rbind",IRR3_W))
AveI3_W<-aggregate(.~Date.Time,AveIRR3_W,FUN=mean)

# IRRIGATION 4 - SKIP
I4_S<-names(flow)[grep("4_skip", names(flow))]
IRR4_S<-flow[I4_S]
names(IRR4_S)
for(i in 1:length(names(IRR4_S))){
  IRR4_S[[i]]<-IRR4_S[[i]][guna.flume$Start[guna.flume$Irrigation=="4"]<IRR4_S[[i]]$Date.Time&IRR4_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="4"],]
}
AveIRR4_S<-(do.call("rbind",IRR4_S))
AveI4_S<-aggregate(.~Date.Time,AveIRR4_S,FUN=mean)

# IRRIGATION 4 - WATER
I4_W<-names(flow)[grep("4_water", names(flow))]
IRR4_W<-flow[I4_W]
names(IRR4_W)
for(i in 1:length(names(IRR4_W))){
  IRR4_W[[i]]<-IRR4_W[[i]][guna.flume$Start[guna.flume$Irrigation=="4"]<IRR4_W[[i]]$Date.Time&IRR4_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="4"],]
}
AveIRR4_W<-(do.call("rbind",IRR4_W))
AveI4_W<-aggregate(.~Date.Time,AveIRR4_W,FUN=mean)

# IRRIGATION 5 - SKIP
I5_S<-names(flow)[grep("5_skip", names(flow))]
IRR5_S<-flow[I5_S]
names(IRR5_S)
for(i in 1:length(names(IRR5_S))){
  IRR5_S[[i]]<-IRR5_S[[i]][guna.flume$Start[guna.flume$Irrigation=="5"]<IRR5_S[[i]]$Date.Time&IRR5_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="5"],]
}
AveIRR5_S<-(do.call("rbind",IRR5_S))
AveI5_S<-aggregate(.~Date.Time,AveIRR5_S,FUN=mean)

# IRRIGATION 5 - WATER
I5_W<-names(flow)[grep("5_water", names(flow))]
IRR5_W<-flow[I5_W]
names(IRR5_W)
for(i in 1:length(names(IRR5_W))){
  IRR5_W[[i]]<-IRR5_W[[i]][guna.flume$Start[guna.flume$Irrigation=="5"]<IRR5_W[[i]]$Date.Time&IRR5_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="5"],]
}
AveIRR5_W<-(do.call("rbind",IRR5_W))
AveI5_W<-aggregate(.~Date.Time,AveIRR5_W,FUN=mean)

# IRRIGATION 6 - SKIP
I6_S<-names(flow)[grep("6_skip", names(flow))]
IRR6_S<-flow[I6_S]
names(IRR6_S)
for(i in 1:length(names(IRR6_S))){
  IRR6_S[[i]]<-IRR6_S[[i]][guna.flume$Start[guna.flume$Irrigation=="6"]<IRR6_S[[i]]$Date.Time&IRR6_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="6"],]
}
AveIRR6_S<-(do.call("rbind",IRR6_S))
AveI6_S<-aggregate(.~Date.Time,AveIRR6_S,FUN=mean)

# IRRIGATION 6 - WATER
I6_W<-names(flow)[grep("6_water", names(flow))]
IRR6_W<-flow[I6_W]
names(IRR6_W)
for(i in 1:length(names(IRR6_W))){
  IRR6_W[[i]]<-IRR6_W[[i]][guna.flume$Start[guna.flume$Irrigation=="6"]<IRR6_W[[i]]$Date.Time&IRR6_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="6"],]
}
AveIRR6_W<-(do.call("rbind",IRR6_W))
AveI6_W<-aggregate(.~Date.Time,AveIRR6_W,FUN=mean)

# IRRIGATION 7 - SKIP
I7_S<-names(flow)[grep("7_skip", names(flow))]
IRR7_S<-flow[I7_S]
names(IRR7_S)
for(i in 1:length(names(IRR7_S))){
  IRR7_S[[i]]<-IRR7_S[[i]][guna.flume$Start[guna.flume$Irrigation=="7"]<IRR7_S[[i]]$Date.Time&IRR7_S[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="7"],]
}
AveIRR7_S<-(do.call("rbind",IRR7_S))
AveI7_S<-aggregate(.~Date.Time,AveIRR7_S,FUN=mean)

# IRRIGATION 7 - WATER
I7_W<-names(flow)[grep("7_water", names(flow))]
IRR7_W<-flow[I7_W]
names(IRR7_W)
for(i in 1:length(names(IRR7_W))){
  IRR7_W[[i]]<-IRR7_W[[i]][guna.flume$Start[guna.flume$Irrigation=="7"]<IRR7_W[[i]]$Date.Time&IRR7_W[[i]]$Date.Time<guna.flume$End[guna.flume$Irrigation=="7"],]
}
AveIRR7_W<-(do.call("rbind",IRR7_W))
AveI7_W<-aggregate(.~Date.Time,AveIRR7_W,FUN=mean)


# Dataset: flow2 (new list of flume data to be used for the analyses)
FlowAve<-list(AveI1_W,AveI1_S,AveI2_W,AveI2_S,AveI3_W,AveI3_S,AveI4_W,AveI4_S,AveI5_W,AveI5_S,AveI6_W,AveI6_S,AveI7_W,AveI7_S)
#make a new list containing all the flow average data for each irrigation and furrow type
names(FlowAve)<-c("AveI1_W","AveI1_S","AveI2_W","AveI2_S","AveI3_W","AveI3_S","AveI4_W","AveI4_S","AveI5_W","AveI5_S","AveI6_W","AveI6_S"
                  ,"AveI7_W","AveI7_S")
# two lists - flow and FlowAve
names(flow) # for where specific flume data exists
names(FlowAve) # for where specific flume data does not exist, a proxy is used


#########################################################
######## SURFACE WATER CHEM DATA ########################
#########################################################

##### #2a IMPORT & CLEAN DATA ###########################

alkalinity<-read.csv("Data/alkalinity.csv")
alkalinity<-alkalinity[!alkalinity$abs==99,]
alkalinity$alk<-(4.299*alkalinity$abs+1.016)*alkalinity$dilution
alkalinity<-alkalinity[!is.na(alkalinity$Labcode),]
alkalinity<-alkalinity[!is.na(alkalinity$Labcode),]
names(alkalinity)
alkalinity$Alt.Lab<-NULL

anions<-read.csv("Data/anions.csv")
anions<-anions[!is.na(anions$Labcode),]
anions$Alt.Lab<-NULL
anions$NO3N<-anions$NO3*14.0067/(16.000*3+14.0067)

carbon<-read.csv("Data/carbon.csv")
carbon<-carbon[!is.na(carbon$Labcode),]
carbon$Alt.Lab<-NULL

cations<-read.csv("Data/cations.csv")
cations<-cations[!is.na(cations$Labcode),]
cations$Alt.Lab<-NULL

field<-read.csv("Data/field.csv")
field<-field[!is.na(field$Labcode),]
field$RQEasy_Nitrate[field$Irrigation==1&field$RQEasy_Nitrate==99]<-NA
field$RQEasy_NO3<-field$RQEasy_Nitrate
field$RQEasy_Nitrate<-field$RQEasy_NO3/(14.0067+15.9994*3)*14.0067
head(field)
field$Alt.Lab<-NULL

GC<-read.csv("Data/GC.csv")
GC<-GC[!is.na(GC$Labcode),]
GC$Alt.Lab<-NULL

nutrients<-read.csv("Data/nutrients.csv")
nutrients<-nutrients[!is.na(nutrients$Labcode),]
nutrients$Alt.Lab<-NULL

urea<-read.csv("Data/urea.csv")
urea<-urea[!is.na(urea$Labcode),]
urea$Alt.Lab<-NULL
names(urea)<-c("Labcode","ID","Urea","Dilutionfactor","Urea.ugNml","Date.analysed")
urea<-subset(urea, select=-c(ID))

weather<-read.csv("Data/weather_station.csv")

surfaceN15<-read.csv("Data/SurfaceN15.csv")
N15<-aggregate(.~Labcode,data=surfaceN15,FUN=mean,na.action=na.pass)
N15$N15.TN.mgL<-N15$TN.ug/30 # TN ug divided by 30mL sample volume used for N15 analysis (mg/L)

##### #2b FORM MEGA CHEM DATASET ########################
# Dataset: chemistry (all water chemistry data)
AA<-merge(alkalinity,anions,all=TRUE,by="Labcode")
BB<-merge(AA,carbon,all=TRUE,by="Labcode")
CC<-merge(BB,cations,all=TRUE,by="Labcode")
DD<-merge(CC,field,all=TRUE,by="Labcode")
EE<-merge(DD,GC,all=TRUE,by="Labcode")
FF<-merge(EE,nutrients,all=TRUE,by="Labcode")
GG<-merge(FF,N15,all=TRUE,by="Labcode")
chemistry<-merge(GG,urea,all=TRUE,by=c("Labcode"))
chemistry<-chemistry[!is.na(chemistry$Labcode),]
chemistry$Date.Time<-as.POSIXct(paste(as.character(chemistry$Date.field),chemistry$Time.field,sep=" "),format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")

rm(AA,BB,CC,DD,EE,FF,GG)

head(chemistry)

##### #2c N2ON formulas #################################
##### N2ON functions

library(marelac)

### Convert EC (uS) readings to practical salinity (PS): Fofonoff & Millard
    # Equations are valid within the temperature range between -2C and 35C, pressure
    # range between 0-10000 decibars and a PS between 2-42 for the respective EC and C ratio.
    # Although PS values less than 2 are not defined, 
    # the equations deliver valid non-zero results down to thresholds of conductivity 
    # ratios >0.0005 and PS>0.02.
    # Values in these outer limits are estimates congruent with the Fortran algorithms, 
    # below these threshold functions return to 0.

### ECRatio: the ratio of conductivity of sample/conductivity of seawater with S=35, T=15, P=0
ECRatio<-function(EC){  # EC = EC reading in uS
  (EC*10^(-3))/42.914
} # output EC ratio: mS.cm-1/mS.cm-1

### PS
PS<-function(t,P,R){ # t=temperature in C, P=pressure in atm, R=ECRatio
 
  p<-P*10.1325000000 # convert pressurein atm to dbar
  
  e1<-2.070*10^(-5)
  e2<--6.370*10^(-10)
  e3<-3.989*10^(-15)
  
  d1<-3.426*10^(-2)
  d2<-4.464*10^(-4)
  d3<-4.215*10^(-1)
  d4<--3.107*10^(-3)  
  
  Rp<-1+(e1*p+e2*p^2+e3*p^3)/(1+d1*t+d2*t^2+d3*R+d4*t*R)
  
  c0<-0.6766097
  c1<-2.00664*10^(-2)
  c2<-1.104259*10^(-4)
  c3<--6.9698*10^(-7)
  c4<-1.0031*10^(-9)
  
  rt<-c0+c1*t+c2*t^2+c3*t^3+c4*t^4
  
  Rt<-R/Rp*rt
  
  a0<-0.0080
  a1<--0.1692
  a2<-25.3851
  a3<-14.0941
  a4<--7.0261
  a5<-2.7081
  
  b0<-0.0005
  b1<--0.0056
  b2<--0.0066
  b3<--0.0375
  b4<-0.0636
  b5<--0.0144
  k<-0.0162
  
  dSdT<-((t-15)/(1+k*(t-15)))*(b0+b1*Rt^(0.5)+b2*Rt+b3*Rt^(1.5)+b4*Rt^(2)+b5*Rt^(2.5))
  
  PS<-a0+a1*Rt^(0.5)+a2*Rt+a3*Rt^(1.5)+a4*Rt^2+a5*Rt^(2.5)+dSdT
  
  return(PS)
}

### Convert practical salinity to absolute salinity 
    # install.package: library(marelac)
    # use function: convert_PStoAS
    # convert_PStoAS(convert_RtoS(R=moree$EC.Ratio,t=moree$Temp,p=moree$dbar*0.1))

### Concentration of dissolved N2O
    # based on Roper 2013 and Weiss and Price 1980
    # [N2O] in sample 

N2O.sample<-function (t, GC.ppm, AS, p){ 
  # t=temp in C, GC.ppm=GC reading in ppm, AS=salinity in ppt (g/kg) or AS, p=pressure in atm 
  # FOR LAB CONDITIONS
  
  # Step 1 (mols in headspace - Roper, 2013)
  
  V.empty<-0.012 # volume of empty evacuated container (L)
  V.liquid<-0.006 # volume of liquid in evacuated container (L)
  V.headspace<-(V.empty-V.liquid)
  
  Psam<-V.empty/V.headspace # pressure in headspace (atm)
  
  TT<-t+273.15 # convert temp in C to K
  
  molar.volume<-GC.ppm*10^(-6) # molar volume of N2O in head space (LN2O/Lair)
  
  gasHS<-(Psam*molar.volume*V.headspace)/(0.08205601*TT) # 0.08205601=ideal gas constant, R (atm.L.mol-1.K-1)
  
  # Step 2 (mols dissolved in water - Weiss & Price, 1980)
  
  termA<-24.4543-67.4509*(100/TT)-4.8489*log(TT/100)-0.000544*AS
  termB<--9.4563/TT+0.04739-6.427*10^(-5)*TT
  K0<-exp(-62.7062+97.3066*(100/TT)+24.1406*log(TT/100)
          +AS*(-0.058420+0.033193*(TT/100)-0.0051313*(TT/100)^2))
  
  v<-32.3 # partial modal volume of CO2, which we assume to be the same for N2O
  
  FF<-K0*(p-exp(termA))*exp(p*termB+v*((1-p)/(0.08205601*t)))
  
  gasDS<- molar.volume*FF*V.liquid # mol
  
  # Step 3 (total mols - dissolved + headspace)
  
  gasTOT<-gasDS+gasHS # mols
  
  # Step 4 (concentration in sample)
  
  gasCONC.molL<-gasTOT/V.liquid # mols.L-1
  gasCONC.gL<-gasCONC.molL*(14.0067*2) # N2O-N g.L-1
  
  return(gasCONC.gL)
}

##### N2O FLUX
### Wind Transfer velocity (k.wind)

k.wind<-function(wind, t){# wind=wind speed at 2m above ground surface (m.s-1), t=temperature (C)
  
  wind.10m<-wind*(log(10/0.001)/log(2/0.001))
  
  N2O.Sc<-gas_schmidt(t,species="N2O")
  N2O.Sc<-as.vector(N2O.Sc)
  
  k.wind<-0.31*wind.10m^2*(N2O.Sc/660)^(0.5)
  #use constant 0.39 in Wanninkhof, 1992
  
  return (k.wind)
}


### Water transfer velocity (k.water)
# Water velocity
water.velocity<-function(water.counts,water.interval,propeller){
  counts.per.s<-water.counts/water.interval
  
  velocity.97.13<- 
    ifelse((counts.per.s<1.76),(0.0775*counts.per.s+0.0107),
           ifelse((1.76<counts.per.s&counts.per.s<14.61),(0.0522*counts.per.s+0.0553),
                  #counts.per.s>14.61
                  (0.0513*counts.per.s+0.0686)
           )
    )
  
  velocity.98.02<- 
    ifelse((counts.per.s<0.49),(0.2428*counts.per.s+0.0108),
           ifelse((0.49<counts.per.s&counts.per.s<4.62),(0.2549*counts.per.s+0.0050),
                  #counts.per.s>4.62
                  ( 0.2608*counts.per.s-0.0226)
           )
    )
  
  velocity.97.07<- 
    ifelse((counts.per.s<1.90),(0.0591*counts.per.s+0.0213),
           ifelse((1.90<counts.per.s & counts.per.s<8.50),(0.0559*counts.per.s+0.0275),
                  #counts.per.s>8.50
                  (0.0523*counts.per.s-0.0578)
           )
    )
  
  
  velocity<-ifelse(   #in m/s
    propeller=="1-97.13",
    velocity.97.13,
    (ifelse(
      propeller=="3-98.02",
      velocity.98.02,
      #propeller=="5-97.07"
      velocity.97.07
    )
    )
  )
  
  return(velocity)
}

# Water Transfer Velocity (water.k)
water.k<- function(p,t,AS,velocity,depth){
  
  P<-p*10.1325000000
  
  DD<-diffcoeff(AS, t, P, species = "N2O")
  D<-as.vector(DD[,1])
  
  water.trans.velocity<-sqrt(D*velocity/depth)
  
  return (water.trans.velocity)
  
}

# Water Discharge
discharge<-function(velocity, CSarea){ # velocity=water velocity (m/s), CSarea=cross sectional area of water body (m2)
  velocity*CSarea*0.001 # L.s-1
}


### Air samples (Reference Values)
N2O.air<-function(GC.ppm,t) { # GC.ppm=GC reading, t=temperature in C
  TT<-t+273.15
  molar.volume<-GC.ppm*10^(-6) # molar volume of N2O in head space (LN2O/Lair)
  gasHS<-(molar.volume)/(0.08205601*TT) # mols in headspace (mol/L)
  N2ONair<-gasHS*(14.0067*2) # g of N2ON/L
  return(N2ONair)
}


### Calculated flux (Clough et al., 2007; Cole & Caraco, 2001)

N2ONflux<-function(k,N2O.conc,N2O.atmo){ 
  # k=total transfer velocity, N2O.conc=concentration N2O in sample (g.L-1), N2O.atmo=conc N2O in air (g.L-1)
  
  N2ONflux<-k*(N2O.conc-N2O.atmo)*0.001 # (m.g)/(L*0.001.s), times by 0.001 to convert L to m3 
  
  return(N2ONflux) # g/m2/s
  
}


##### #2d N2ON calculations #############################
# N2ON calculations with script
chemistry$ECRatio<-ECRatio(chemistry$EC)
chemistry$PS<-PS(t = chemistry$Temperature,P = 0.0102,R = chemistry$ECRatio)
chemistry$AS<-convert_PStoAS(S=chemistry$PS,P=1.0012*10.1325000000)
chemistry$dN2ON.gL<-N2O.sample(t = chemistry$LabT,GC.ppm = chemistry$N2Oppm,AS = chemistry$AS,p = 1.001727) #nb. air pressure changes a bit
    #nb. values calculated are similar but not exactly the same as those Ben calculated...

weather$Date.Time<-as.POSIXct(paste(as.character(weather$Date),weather$Time,sep=" "),format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")

CHEMISTRY<-chemistry[!is.na(chemistry$Date.Time),]

CHEMISTRY$wind.kph<-NA
for (i in 1:length(CHEMISTRY$Date.Time)){
  CHEMISTRY$wind.kph[i]<-weather$Wind.Speed..kph.[which.min(abs(CHEMISTRY$Date.Time[i]-weather$Date.Time))]
}
CHEMISTRY$wind.mps<-CHEMISTRY$wind.kph*1000/60/60

CHEMISTRY$AirTemp.C<-NA
for (i in 1:length(CHEMISTRY$Date.Time)){
  CHEMISTRY$AirTemp.C[i]<-weather$Air.Temp...C.[which.min(abs(CHEMISTRY$Date.Time[i]-weather$Date.Time))]
}

CHEMISTRY$AirTemp.C<-NA
for (i in 1:length(CHEMISTRY$Date.Time)){
  CHEMISTRY$AirTemp.C[i]<-weather$Air.Temp...C.[which.min(abs(CHEMISTRY$Date.Time[i]-weather$Date.Time))]
}

CHEMISTRY$kwind<-NA
CHEMISTRY$kwind<-k.wind(wind=CHEMISTRY$wind.mps,t = CHEMISTRY$AirTemp.C)

N2O.amb<-N2O.air(GC.ppm=0.29811, t=20.6)
CHEMISTRY$n2oflux<-N2ONflux(k = CHEMISTRY$kwind,N2O.conc = CHEMISTRY$dN2ON.gL,N2O.atmo = N2O.amb)
CHEMISTRY$N2Oflux.ugmmin<-CHEMISTRY$n2oflux*10^6*60

write.csv(CHEMISTRY,"I:/Summer15_16/MetaAnalysis2013-16/Data/N15Flumes201415.csv")

##### #2e Correction: Head Water Concs ##################
HEAD<-chemistry[grep("H",chemistry$Field.location),]
HEAD$Labcode<-as.factor(HEAD$Labcode)

HEAD$NOx<-droplevels(HEAD$NOx) #### CHECK RAW DATA TO SEE WHY IT IS IMPORTANTING THESE VECTORS AS A FUNCTION AND NOT NUMERIC
class(HEAD$NOx)
HEAD$NOx<-as.numeric(levels(HEAD$NOx))[HEAD$NOx] 

HEAD$Ammonium<-droplevels(HEAD$Ammonium) #### CHECK RAW DATA TO SEE WHY IT IS IMPORTANTING THESE VECTORS AS A FUNCTION AND NOT NUMERIC
HEAD$Ammonium<-as.numeric(levels(HEAD$Ammonium))[HEAD$Ammonium]

# mt function: mt function to aggregate a table which contains numeric, character and factor vectors
mt <- function(x) {
  if(is.numeric(x)) { # if x is numeric
    return(mean(x)) # compute the mean
  } else {
    if (is.factor(x)) {
      return(unique(x))
    } else {
      tab <- table(x) # tabulate x
      return(paste(paste(names(tab), # and format it for display
                         tab, sep=": "),
                   collapse=", "))
    }
  }
}

HEADCONCS<-aggregate(.~Date.Time+Irrigation,data=HEAD,FUN=mt,na.action=na.pass) # average head water chemistry collected at the same time/from guna's flumes
HEADCONC<-HEADCONCS[,c("Date.Time","Irrigation","alk","NO3","NO3N","Total.Carbon",
                      "pH","EC","Temperature","RQEasy_Nitrate","Ammonium","NOx","NPOC","TN",
                      "Urea","Urea.ugNml","dN2ON.gL","N15.TN.mgL","N15.Atom.")]

FLUMECHEM<-chemistry[grep("E|W",chemistry$Field.location),]

N.FLUMECHEM<-FLUMECHEM[,c("Labcode","Field.location","alk","NO3","NO3N","Total.Carbon","Field.location",
                          "Date.field","Time.field","Time1.field","Time2.field","SampleNo",
                          "pH","EC","Temperature","RQEasy_Nitrate","Irrigation",
                          "CH4ppm","CO2ppm","N2Oppm",
                          "Ammonium","NOx","NPOC","TN","Urea","Urea.ugNml",
                          "Date.Time","dN2ON.gL","N15.TN.mgL","N15.Atom.")]

N.FLUMECHEM$NOx<-droplevels(N.FLUMECHEM$NOx)
N.FLUMECHEM$NOx<-as.numeric(levels(N.FLUMECHEM$NOx))[N.FLUMECHEM$NOx]

N.FLUMECHEM$Ammonium<-droplevels(N.FLUMECHEM$Ammonium)
N.FLUMECHEM$Ammonium<-as.numeric(levels(N.FLUMECHEM$Ammonium))[N.FLUMECHEM$Ammonium]


### Surface water concentration of N components corrected for head ditch concentrations ("run-off")
# RQ NO3
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrRQEasy_Nitrate[i]<-N.FLUMECHEM$RQEasy_Nitrate[i]-HEADCONC$RQEasy_Nitrate[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# NO3-N
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrNO3N[i]<-N.FLUMECHEM$NO3N[i]-HEADCONC$NO3N[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# N2O-N
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrdN2ON.gL[i]<-N.FLUMECHEM$dN2ON.gL[i]-HEADCONC$dN2ON.gL[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# Ammonium
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrAmmonium[i]<-N.FLUMECHEM$Ammonium[i]-HEADCONC$Ammonium[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# NOx
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrNOx[i]<-N.FLUMECHEM$NOx[i]-HEADCONC$NOx[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# TN
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$Corr.TN[i]<-N.FLUMECHEM$TN[i]-HEADCONC$TN[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# 15N TN
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$Corr15N.TN[i]<-N.FLUMECHEM$N15.TN.mgL[i]-HEADCONC$N15.TN.mgL[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
N.FLUMECHEM$CorrTN<-NA
N.FLUMECHEM$CorrTN<-N.FLUMECHEM$Corr15N.TN

for (i in 1:length(N.FLUMECHEM$Labcode)){
    ifelse(is.na(N.FLUMECHEM$CorrTN[i]),
         yes = N.FLUMECHEM$CorrTN[i]<-N.FLUMECHEM$Corr.TN[i],
         no = N.FLUMECHEM$CorrTN[i]<-N.FLUMECHEM$Corr15N.TN[i]
  )
}

# Urea
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrUrea[i]<-N.FLUMECHEM$Urea[i]-HEADCONC$Urea[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}
# NPOC
for (i in 1:length(N.FLUMECHEM$Labcode)){
  N.FLUMECHEM$CorrNPOC[i]<-N.FLUMECHEM$NPOC[i]-HEADCONC$NPOC[which.min(abs(N.FLUMECHEM$Date.Time[i]-HEADCONC$Date.Time))]
}

# DIN & DON
N.FLUMECHEM$CorrdN2O.mgL<-N.FLUMECHEM$CorrdN2ON.gL/1000
N.FLUMECHEM$CorrNOxNO3N<-rowMeans(N.FLUMECHEM[,c("CorrNO3N","CorrNOx")],na.rm=TRUE)
#N.FLUMECHEM[,c("CorrNOxNO3N","CorrNO3N","CorrNOx")]
N.FLUMECHEM$DIN<-rowSums(N.FLUMECHEM[,c("CorrAmmonium","CorrNOxNO3N","CorrUrea","CorrdN2O.mgL")],na.rm=TRUE) #TN based on DIN concentrations
N.FLUMECHEM$DON<-N.FLUMECHEM$CorrTN-N.FLUMECHEM$DIN
N.FLUMECHEM$DON<-ifelse(N.FLUMECHEM$DON<0,0,N.FLUMECHEM$DON)
N.FLUMECHEM$TotalN<-rowSums(N.FLUMECHEM[,c("DIN","DON")],na.rm=TRUE) # TN
#head(N.FLUMECHEM)

#########################################################
############## FLUME & CHEM DATA ########################
#########################################################

##### #4a Matching Flume & Chem Data ####################

#associated different chemistries with logger data
N.FLUMECHEM$Up.Down<-NA
N.FLUMECHEM$Up.Down[grep("E",x = N.FLUMECHEM$Field.location)]<-"U"
N.FLUMECHEM$Up.Down[grep("W",x = N.FLUMECHEM$Field.location)]<-"D"
N.FLUMECHEM$Furrow<-NA
N.FLUMECHEM$Furrow<-substring(N.FLUMECHEM$Field.location,1,1)

flumedets<-flume[c("Irrigation","Up.Down","Probe.SN","Ftype","Furrow.y")]
flumedets<-flumedets[!is.na(flumedets$Irrigation),]
names(flumedets)<-c("Irrigation","Up.Down","Probe.SN","Ftype","Furrow")

# Merge flume details with N chemistry
NFLUMECHEM<-merge(N.FLUMECHEM,flumedets,all.x=TRUE,by=c("Irrigation","Up.Down","Furrow"))

# Creation of new column to specify individual flume data for the given water sample taken
NFLUMECHEM$flow_datanames<-paste(NFLUMECHEM$Irrigation,"_",NFLUMECHEM$Ftype,"  ",NFLUMECHEM$Probe.SN,sep="")

# T/F statement to determine if individual flume data is available for the water sample taken
NFLUMECHEM$flowTF<-NFLUMECHEM$flow_datanames %in% names(flow)

# Creation of new column which specifies name of the file needed to select proxy flume data
NFLUMECHEM$Ftype[NFLUMECHEM$Furrow==4|NFLUMECHEM$Furrow==6|NFLUMECHEM$Furrow==8]<-"skip" #These stayed the same throughout the season
NFLUMECHEM$Ftype[NFLUMECHEM$Furrow==5|NFLUMECHEM$Furrow==7|NFLUMECHEM$Furrow==9]<-"water" #These stayed the same throughout the season
NFLUMECHEM$S.W<-NA
NFLUMECHEM$S.W[NFLUMECHEM$Ftype=="skip"]<-"S"
NFLUMECHEM$S.W[NFLUMECHEM$Ftype=="water"]<-"W"
NFLUMECHEM$FlowAveData<-paste("Ave","I",NFLUMECHEM$Irrigation,"_",NFLUMECHEM$S.W,sep="")

# Convert Date.Times either side of sample to as.POSIXct format
NFLUMECHEM$Date.Time1<-paste(as.character(NFLUMECHEM$Date.field),as.character(NFLUMECHEM$Time1.field),sep=" ")
NFLUMECHEM$Date.Time1<-as.POSIXct(NFLUMECHEM$Date.Time1,format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")
NFLUMECHEM$Date.Time2<-paste(as.character(NFLUMECHEM$Date.field),as.character(NFLUMECHEM$Time2.field),sep=" ")
NFLUMECHEM$Date.Time2<-as.POSIXct(NFLUMECHEM$Date.Time2,format="%d/%m/%Y %H:%M",tz="Australia/Brisbane")

### Associate cumulative volumes at given times either side of the water sample (match to closest time)
#CumVolume1
for(i in 1:length(NFLUMECHEM$Irrigation)){
  NFLUMECHEM$CumVol1[i]<-
    ifelse(NFLUMECHEM$flow_datanames[i] %in% names(flow),
           yes=flow[[NFLUMECHEM$flow_datanames[i]]]$
             CumVolume[which.min(abs(flow[[NFLUMECHEM$flow_datanames[i]]]$Date.Time-NFLUMECHEM$Date.Time1[i]))], ## NEED TO REDEFINE SO IT IS CLOSEST TIME MATCH
           no=FlowAve[[NFLUMECHEM$FlowAveData[i]]]$
             CumVolume[which.min(abs(FlowAve[[NFLUMECHEM$FlowAveData[i]]]$Date.Time-NFLUMECHEM$Date.Time1[i]))]  ## NEED TO REDEFINE SO IT IS CLOSEST TIME MATCH
    )
}

#CumVolume2
for(i in 1:length(NFLUMECHEM$Irrigation)){
  NFLUMECHEM$CumVol2[i]<-
    ifelse(NFLUMECHEM$flow_datanames[i] %in% names(flow),
           yes=flow[[NFLUMECHEM$flow_datanames[i]]]$
             CumVolume[which.min(abs(flow[[NFLUMECHEM$flow_datanames[i]]]$Date.Time-NFLUMECHEM$Date.Time2[i]))],  
           no=FlowAve[[NFLUMECHEM$FlowAveData[i]]]$
             CumVolume[which.min(abs(FlowAve[[NFLUMECHEM$FlowAveData[i]]]$Date.Time-NFLUMECHEM$Date.Time2[i]))]
    )
}


##### #4b Calculating total N losses ####################

NFLUMECHEM$Vol.L<-NFLUMECHEM$CumVol2-NFLUMECHEM$CumVol1
NFLUMECHEM$RQ_NO3Nloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrRQEasy_Nitrate/1000 #g
NFLUMECHEM$N2ONloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrdN2ON.gL #g
NFLUMECHEM$NH4loss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrAmmonium/1000 #g
NFLUMECHEM$NOxloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrNOx/1000 #g
#NFLUMECHEM$TNloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrTN/1000 #g
NFLUMECHEM$TotalNloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$TotalN/1000 #g
NFLUMECHEM$Urealoss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrUrea/1000 #g
NFLUMECHEM$NPOCloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$NPOC/1000 #g
NFLUMECHEM$NO3Nloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrNO3N/1000 #g

NFLUMECHEM$DONloss.g<-NFLUMECHEM$Vol.L*NFLUMECHEM$DON/1000 #g
NFLUMECHEM$NOx3Nloss.g <-NFLUMECHEM$Vol.L*NFLUMECHEM$CorrNOxNO3N/1000 #g

tail(NFLUMECHEM)

NFLUMECHEM<-unique(NFLUMECHEM)

write.csv(NFLUMECHEM,"Outputs/NFlumeChemistry.csv")

Nloss<-aggregate(cbind(N2ONloss.g,NH4loss.g,NOx3Nloss.g,Urealoss.g,RQ_NO3Nloss.g,DONloss.g,TotalNloss.g)~Irrigation+Up.Down, data=NFLUMECHEM,
          FUN=sum,na.action=na.pass,na.rm=TRUE)

###

Nloss.kgha.U<-Nloss[Nloss$Up.Down=="U",c("N2ONloss.g","NH4loss.g","NOx3Nloss.g","Urealoss.g","RQ_NO3Nloss.g","DONloss.g","TotalNloss.g")]*0.001/(80*6/10000)
Nloss.kgha.D<-Nloss[Nloss$Up.Down=="D",c("N2ONloss.g","NH4loss.g","NOx3Nloss.g","Urealoss.g","RQ_NO3Nloss.g","DONloss.g","TotalNloss.g")]*0.001/(87*6/10000)
Nloss.kgha<-rbind(Nloss.kgha.U,Nloss.kgha.D)
names(Nloss.kgha)<-c("N2ONloss.kgha","NH4loss.kgha","NOxloss.kgha","Urealoss.kgha","RQ_NO3Nloss.kgha","DON.kgha","TotalNloss.kgha")
Nloss.kgha<-cbind(Nloss[,c("Irrigation","Up.Down")],Nloss.kgha)
Nloss.kgha$Totals<-rowSums(Nloss.kgha[,c("N2ONloss.kgha","NH4loss.kgha","NOxloss.kgha","Urealoss.kgha","DON.kgha")])

Nloss.kgha$TotalNloss.kgha

mean(aggregate(TotalNloss.kgha~Up.Down,data=Nloss.kgha,FUN=sum)$TotalNloss.kgha)

write.csv(Nloss.kgha,"Outputs/C1_Nloss.kgha.csv")

#Nloss.kgha$OtherN<-Nloss.kgha$TNloss.kgha-Nloss.kgha$Totals
#Nloss.kgha$OtherN<-ifelse(Nloss.kgha$OtherN<0,0,Nloss.kgha$OtherN)



##### #4d Plotting proportion of different N losses #####

Nloss.kgha.ave<-aggregate(cbind(N2ONloss.kgha,NH4loss.kgha,NOxloss.kgha,Urealoss.kgha,RQ_NO3Nloss.kgha,DON.kgha)~Irrigation,
                          data=Nloss.kgha,
                          FUN=mean)
Nloss.kgha.ave$Urealoss.kgha<-ifelse(Nloss.kgha.ave$Urealoss.kgha<0,0,Nloss.kgha.ave$Urealoss.kgha) # for purposes of plotting
#Nloss.kgha.ave$NO3Nloss.kgha<-ifelse(Nloss.kgha.ave$NO3Nloss.kgha<0,0,Nloss.kgha.ave$NO3Nloss.kgha) # for purposes of plotting
Nloss.kgha.ave$NOxloss.kgha<-ifelse(Nloss.kgha.ave$NOxloss.kgha<0,0,Nloss.kgha.ave$NOxloss.kgha) # for purposes of plotting
#Nloss.kgha.ave$NOxNO3Nloss.kgha<-mean(Nloss.kgha.ave$NO3Nloss.kgha,Nloss.kgha.ave$NOxloss.kgha) # for purposes of plotting
Nloss.kgha.ave$Totals<-rowSums(Nloss.kgha.ave[,c("N2ONloss.kgha","NH4loss.kgha","NOxloss.kgha","Urealoss.kgha","DON.kgha" #,"NO3Nloss.kgha",
                                                 )])

colSums(Nloss.kgha.ave)



#COLOUR
library("RColorBrewer")
sequential<-brewer.pal(6,"RdYlBu")

png(file="Outputs/Nlossesedit.png",
    width=160,height=80,units="mm",res=300,bg="white")
#png(file="Outputs/Nlosses.png",
#    width=160,height=80,units="mm",res=1200,bg="white")
#jpeg(file="Outputs/Nlosses.jpg",
#    width=160,height=80,units="mm",res=1200,bg="white")
par(mfrow=c(1,1),mar=c(3,3,0.7,0.5),mgp=c(1.7,0.3,0),family="serif",bg=NA)
barplot(t(Nloss.kgha.ave[,c("N2ONloss.kgha","NH4loss.kgha","NOxloss.kgha","Urealoss.kgha","DON.kgha")]),
        names.arg=Nloss.kgha.ave$Irrigation,
        cex.names=0.9,
        col=sequential[c(1:3,5:6)],
        xlab="Irrigation",
        ylab="N loss (kg/ha)",yaxt="n",
        ylim=c(0,50))
axis(2,at = seq(from=0,to=50,by=10),labels = c("0","10","20","30","40","50"),line=0,
     lwd = 0.6,tick=FALSE,cex.axis=0.8)
axis(2,at = seq(from=0,to=50,by=10),labels = NA,
     lwd = 0.6,tck=-0.01)
        legend("topright",inset = c(0.01,0.02), legend=c("DON","Urea-N","NOx-N",expression(paste("NH"[4])),expression(paste("N"[2],"O-N"))),
               fill=sequential[c(6:5,3:1)],cex=0.9,
               title="N components")
box(lwd=0.6)
dev.off()


##### #4c Plotting total N losses #######################
Nloss.kgha

sequential<-brewer.pal(3,"RdYlBu")

png(file="Outputs/TN_lossedit.png",
    width=160,height=80,units="mm",res=300,bg="white")
#png(file="Outputs/TN_loss.png",
#    width=160,height=80,units="mm",res=1200,bg="white")
#jpeg(file="Outputs/TN_loss.jpg",
#    width=160,height=80,units="mm",res=1200,bg="white")
#width=1700,height=1800,res=1200)
par(mfrow=c(1,1),mar=c(2.5,2,0.5,0.5),mgp=c(0.26,0.005,0),family="serif",bg=NA)
plot(Totals~Irrigation,data=Nloss.kgha,ylim=c(-1,50),xlim=c(0.5,7.5),yaxs="i",xaxs="i",
     xaxt="n",yaxt="n",type="n",
     ylab="",
     xlab="",
     cex.lab=0.5,bty="n")
points(TotalNloss.kgha~Irrigation,data=Nloss.kgha[Nloss.kgha$Up.Down=="U",],cex=0.8,lwd=0.6, pch=19,col=sequential[1])#rgb(red=1,green=0,blue=0,alpha=0.5))
points(TotalNloss.kgha~Irrigation,data=Nloss.kgha[Nloss.kgha$Up.Down=="D",],cex=0.8,lwd=0.6, pch=19,col=sequential[3])#rgb(red=0,green=0.7,blue=0.5,alpha=0.5))
box(lwd=0.6)
axis(1,at = seq(from=1,to=7,by=1),labels = c("1","2","3","4","5","6","7"),line=0.1,
     lwd =0.6,tick=FALSE,cex.axis=0.8)
axis(1,at = seq(from=1,to=7,by=1),labels = NA,
     lwd = 0.6,tck=-0.01)
mtext(text="Irrigation Number",
      side=1,cex=0.9,outer = FALSE,line=1.3)
axis(2,at = seq(from=0,to=50,by=10),labels = c("0","10","20","30","40","50"),line=0,
     lwd = 0.6,tick=FALSE,cex.axis=0.8)
axis(2,at = seq(from=0,to=50,by=10),labels = NA,
     lwd = 0.6,tck=-0.01)
mtext(text=expression(paste("Total N lost from Lysimeter Plot (kg/ha)")),side=2,
      cex=0.9,outer = FALSE,line=1)

legend(x=6,y=48,legend = c("Upstream N","Downstream N"),cex=0.8,
       pch=19,col = c(sequential[3],sequential[1]))
dev.off()
#arrows(TAILS$dN2O.DecCor-TAILS$dN2O.DecCor.se,TAILS$IPCC.DecCor,TAILS$dN2O.DecCor+TAILS$dN2O.DecCor.se,TAILS$IPCC.DecCor,
#       length=0.05,angle=90,code=3,lwd=0.6)#dissolved N2ON arrows

#arrows(TAILS$dN2O.DecCor,TAILS$IPCC.DecCor-TAILS$IPCC.DecCor.se,TAILS$dN2O.DecCor,TAILS$IPCC.DecCor+TAILS$IPCC.DecCor.se,
#       length=0.05,angle=90,code=3,lwd=0.6)#IPCC arrows

#text(expression(paste("R"^2," = 0.9948")),x = 0.005,y=0.75,pos = 4,cex=0.7)
#text(expression(paste("p = 0.0326")),x = 0.005,y=0.7,pos = 4,cex=0.7)


##### #4b Calculating total N losses ####################
SeasonNloss<-aggregate(cbind(N2ONloss.kgha,NH4loss.kgha,NOxloss.kgha,Urealoss.kgha,RQ_NO3Nloss.kgha,Totals)~Up.Down, data=Nloss.kgha,
                 FUN=sum,na.action=na.pass,na.rm=TRUE)
colMeans(SeasonNloss[,c("N2ONloss.kgha","NH4loss.kgha","NOxloss.kgha","Urealoss.kgha","RQ_NO3Nloss.kgha","Totals")])


#########################################################
############## N15 LOSSES ###############################
#########################################################

##### #4a Check background N15 values (upstream) ########

Ao<-0.3663 #(% N15 natural abundance)
NFLUMECHEM$Fert<-NA
NFLUMECHEM$Fert[NFLUMECHEM$Irrigation=="1"|NFLUMECHEM$Irrigation=="2"|NFLUMECHEM$Irrigation=="3"]<-35.16-Ao # 35.06% N15 label over lysimeter plot
NFLUMECHEM$Fert[NFLUMECHEM$Irrigation=="4"|NFLUMECHEM$Irrigation=="5"|NFLUMECHEM$Irrigation=="6"|NFLUMECHEM$Irrigation=="7"]<-27.28-Ao # 35.06% N15 label over lysimeter plot
NFLUMECHEM$Ndff<-100*(NFLUMECHEM$N15.Atom.-Ao)/NFLUMECHEM$Fert # %N derived from fertiliser
NFLUMECHEM$N15Fert<-(NFLUMECHEM$TotalN/1000)*(NFLUMECHEM$Ndff/100) # Amount of TN derived from fertiliser g/L converted to kg
#NFLUMECHEM$N15Fert<-(NFLUMECHEM$CorrTN/1000)*(NFLUMECHEM$Ndff/100) # Amount of TN derived from fertiliser g/L converted to kg; based on a combination of N15 and other TN analyses
NFLUMECHEM$NFert_Water<-NFLUMECHEM$N15Fert*NFLUMECHEM$Vol.L*0.001 #(kg total lost)
NFLUMECHEM$TotalNloss.kg<-NFLUMECHEM$TotalNloss.g/1000
NFLUMECHEM$NFert_proxy<-NFLUMECHEM$Ndff/100*NFLUMECHEM$TotalNloss.kg

#NFLUMECHEM$Ndfs<-100-NFLUMECHEM$Ndff
#NFLUMECHEM$N15Soil<-(NFLUMECHEM$TotalN/1000)*(NFLUMECHEM$Ndfs/100)
#NFLUMECHEM$NSoil_Water<-NFLUMECHEM$N15Soil*NFLUMECHEM$Vol.L*0.001 #(kg total lost)

sum.N15FLUMECHEM<-aggregate(cbind(NFert_Water,NFert_proxy,TotalNloss.kg)~Field.location+Irrigation+S.W+Up.Down+Furrow,data=NFLUMECHEM,FUN=sum,na.action=na.pass,na.rm=TRUE)
U15<-sum.N15FLUMECHEM[sum.N15FLUMECHEM$Up.Down=="U",]
names(U15)<-c("U.Field.location","Irrigation","S.W","U.Up.Down","Furrow","U.N15Fert","U.NFert_proxy.kg","U.TNkg")
D15<-sum.N15FLUMECHEM[sum.N15FLUMECHEM$Up.Down=="D",]
names(D15)<-c("D.Field.location","Irrigation","S.W","D.Up.Down","Furrow","D.N15Fert","D.NFert_proxy.kg","D.TNkg")

N15Fert.loss<-merge(U15,D15,by=c("Irrigation","Furrow"),all.x=TRUE)
N15Fert.loss$Fertloss<-N15Fert.loss$D.N15Fert-N15Fert.loss$U.N15Fert # kg from furrow length
N15Fert.loss$TNloss<-N15Fert.loss$D.TNkg-N15Fert.loss$U.TNkg
N15Fert.loss$FertNproxy<-N15Fert.loss$D.NFert_proxy.kg-N15Fert.loss$U.NFert_proxy.kg

PlotArea<-42/10000 #(N15 applied over 3 by 3m area; divided by 10000 to convert m2 to ha)

N15Fert.loss$Fertloss.ha<-N15Fert.loss$Fertloss/(9/10000)
N15Fert.loss$TNloss.ha<-N15Fert.loss$TNloss/PlotArea
N15Fert.loss$FertNproxy.ha<-N15Fert.loss$FertNproxy/(9/10000)
N15Fert.plot<-aggregate(cbind(Fertloss.ha,FertNproxy.ha,TNloss.ha)~Irrigation,data=N15Fert.loss,FUN=sum,na.action=na.pass,na.rm=TRUE) #kg from whole of LYS plot

write.csv(N15Fert.loss,"Outputs/N15SurfaceLoss.csv")
write.csv(N15Fert.plot,"Outputs/Nloss_Runoff.csv")

sum(N15Fert.plot$Fertloss.ha)
##### #4b Method 2 - N15 values (upstream) ##############

DN15<-aggregate(cbind(D.N15Fert,D.NFert_proxy.kg, D.TNkg)~Irrigation,data=D15,FUN=sum,na.action=na.pass,na.rm=TRUE) #kg from whole of LYS plot
sum(DN15$D.NFert_proxy.kg/(9/10000))
sum(DN15$D.TNkg/(87*6/10000))

DN15


###########

