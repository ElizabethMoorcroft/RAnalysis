setwd("/Users/student/Documents/Bats/Simulations")

densityestimationfunction<-function(DATA){DATA2=(DATA+1);return(DATA2)}

FILENAME<-"Run23Oct2013Perch0,Density=70,Speed=0.00012,Iterations=Basic counts,StepLength=300,CorrWalkMaxAngleChange=0"
SENSORNAME<-"Run23Oct2013Perch0,Density=70,Speed=0.00012,Iterations=1-101,StepLength=300,CorrWalkMaxAngleChange=0,Sensors.csv"
SETTINGNAME<-"Run23Oct2013Perch0,Density=70,Speed=0.00012,Iterations=1-101,StepLength=300,CorrWalkMaxAngleChange=0,Settings.csv"

file<-as.matrix(read.csv(paste(FILENAME,".csv",sep=""),header=F))
Sensor<-read.csv(SENSORNAME)
Setting<-read.csv(SETTINGNAME,rownames=T)
length<-(dim(file)[1])-1
file<-as.matrix(file)[,1:length]

header<-file[1:4,]
iterations<-file[5:dim(file)[2],]

itnumber<-iterations[,1]
itcounts<-iterations[,2:dim(file)[2]]
itcounts<-matrix(as.numeric(itcounts),nrow=dim(file)[2]-1,byrow=T)
itdensitycounts<-densityestimationfunction(itcounts)

headervalues<-matrix(as.numeric(header[,-1]),nrow=dim(file)[2]-1,byrow=T)
headervalues<-headervalues[,4]/(7.5^2)

calcProfileWidth*(Speed*Time/Captures)


###T0rt0153