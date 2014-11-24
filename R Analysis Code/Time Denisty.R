#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Bat Project					                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: Who knows?!                                         	#
#                                                                  	 	#
# Edited by: -                                                     		#
# Edited on: -			                                            	#
#                                                                   	#
# Script title: Density calculation	         	                    	#
# Script purpose:Calculate the density form the simulation using		#
#                  - Tim's Function	(Not implemented)			        #
#                  - Marcus' Function                    				#
#                                                                   	#
#########################################################################
rm(list=ls(all=TRUE)) 

#####################
# Libraries 		#
#####################
library("RColorBrewer")
library("plotrix")


#####################
# Directory			#
#####################
DIR_DATA<-"/Users/student/Documents/Bats/Simulations"
DIR_SAVE<-"/Users/student/Documents/Bats/Simulations"
DIR_CODE<-"/Users/student/Documents/Bats/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")
source("calculateProfileWidth.R")


#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)

#################################
# Load in Settings				#
#################################
setwd(DIR_DATA)
Settings<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,Settings.csv")
Cameras<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,Sensors.csv")

#################################
# Setting variables				#
#################################
NoOfIterations	<-100 #Settings[which(Settings[,1] %in% "NoOfIterations"),2]
Seed			<-Settings[which(Settings[,1] %in% "Seed"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
StepLength		<-Settings[which(Settings[,1] %in% "StepLength"),2]
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
Speed			<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]
Time			<-NoOfSteps*StepLength
Density			<-NoofAnimals/(Area)

#################################
# Load Data						#
#################################


denistylist<-c()
for(i in 0:10){denistylist<-c(denistylist,floor(NoofAnimals-NoofAnimals*i/11)/Area*(1000^2))}

loadindata<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,density.csv",header=FALSE)


x<-loadindata[,-dim(loadindata)[2]]

Data<-x[-c(1:4),-1]
header<-x[c(1:4),-1]

timerow<-which(x[,1]=="Time")
sensorrow<-which(x[,1]=="Sensor")
callrow<-which(x[,1]=="Call")
densityrow<-which(x[,1]=="Density")

ModelSelection<-matrix(nrow=4,ncol=3)
ModelSelection[1,]<-c(2,3.14159, "p141") # REM
ModelSelection[2,]<-c(2,1.428 , "p343") # 
ModelSelection[3,]<-c(4,3.14159, "p221") # 
ModelSelection[4,]<-c(4,1.428 , "p322") # 

TimeMax<-NoOfSteps*StepLength
DensityMax<-NoofAnimals/(Area)

pdf("DensityPlots.pdf")
plot(0,0,ylim=c(-1,1),xlim=c(0,70),type="n",
	axes=FALSE,ylab="",xlab="Time (weeks)")

x<-c(0,DensityMax/7,2*DensityMax/7,3*DensityMax/7,4*DensityMax/7,5*DensityMax/7,6*DensityMax/7,7*DensityMax/7)
x<-round(x*(1000^2))

axis(side=1,at=x,labels=(x),las=2)
axis(side=2,at=c(-1,-0.5,0,0.5,1),labels=c(-1,-0.5,0,0.5,1),las=0)
mtext(text=expression(paste("Bias (Animals/",km^2,")",sep="")),side=2,line=2)
legend(x=0,y=1,col=1:4,pch=rep(1,4), 
		legend=c(paste("Model",ModelSelection[1,3]),
				paste("Model",ModelSelection[2,3]),
				paste("Model",ModelSelection[3,3]),
				paste("Model",ModelSelection[4,3])
				)
	)


for(i in 1:4){
	print(paste("I:",i))
	CamerarowI	<-which(ModelSelection[i,1]==Cameras[,1])	
	CameraWidthI<-Cameras[CamerarowI,5]*2	
	CameraRadiI	<-Cameras[CamerarowI,6]
	CallAngleI	<-as.numeric(ModelSelection[i,2])*2
	
	profilewidth<-calcProfileWidth(CallAngleI, CameraWidthI, CameraRadiI )
	selectedcols<-which(header[sensorrow,]==ModelSelection[i,1] & header[callrow,]== ModelSelection[i,2])
	
	allList<-vector(mode="list",length=4)
	
	for(time in 1:length(selectedcols)){
		Time<-selectedcols[time]
		TimeVal<-header[timerow,Time]*StepLength
		DensityVal<-header[densityrow,Time]/Area
		Timevector<-c()
		Numbvector<-c()
		Estvector<-c()
		allList[[time]]<-Time
		for(iteration in 1:dim(Data)[1]){
			number<- Data[iteration,Time]
			#print(paste("iteration",number))
			estimate<-(1/profilewidth[[1]])*number/(Speed*TimeVal)
			Numbvector<-c(Numbvector,number)
			Timevector<-c(Timevector,(estimate-DensityVal)*(1000^2))
			Estvector<-c(Estvector,estimate*(1000^2))
			Percentvector<-c(Timevector,100*(estimate-DensityVal)*(1000^2)/(DensityVal*(1000^2)))

		}
		#print(paste(DensityVal*(1000^2),mean(Numbvector), profilewidth[[1]], Speed  ))
		#print(table(Estvector))
		meanest<-mean(Timevector)
		#print(meanest)
		sdest<-sd(Timevector)	
		print(paste(DensityVal,sdest))
		print(mean(Percentvector))
		seest<-sdest/10
		plotCI(DensityVal*(1000^2),meanest,uiw=seest*1.96,col=i,add=T)
		testvalue<-wilcox.test(meanest, mu=0)$p.value
		if(testvalue<0.05){plotCI(err="n",x=DensityVal*(1000^2),y=meanest+seest*3,pch="*",col=i)
							print("SIGNIFICANT FOUND")}
	}
}

abline(h=0,lty=2,col="grey")
box()
dev.off()