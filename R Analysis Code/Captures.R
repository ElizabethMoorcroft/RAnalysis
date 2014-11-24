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
DIR_IMG<-"/Users/student/Documents/Bats/lucasMoorcroftManuscript/imgs"
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



loadindata<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,distance.csv",header=FALSE)


x<-loadindata[,-dim(loadindata)[2]]

Data<-x[-c(1:4),-1]
header<-x[c(1:4),-1]

timerow<-which(x[,1]=="Time")
sensorrow<-which(x[,1]=="Sensor")
callrow<-which(x[,1]=="Call")
densityrow<-which(x[,1]=="Density")

ModelSelection<-matrix(nrow=4,ncol=3)
ModelSelection[1,]<-c(0,3.14159, "p141") # REM
ModelSelection[2,]<-c(0,1.428 , "p343") # 
ModelSelection[3,]<-c(1,3.14159, "p221") # 
ModelSelection[4,]<-c(1,1.428 , "p322") # 

TimeMax<-25*60*60*24
DensityMax<-NoofAnimals/(Area)

#setwd(DIR_IMG)
#pdf("ResultsNumberCaptures.pdf")
plot(0,0,ylim=c(-70,70),xlim=c(0,2000),
	type="n",
	axes=FALSE,ylab="",xlab="Average number of captures")

x<-seq(0,10000,by=2000)


axis(side=1,at=(round(x)),labels=(round(x)),las=2)
axis(2)
mtext(text=expression(paste("Absolute error",sep="")),side=2,line=2)
legend(x=0,y=70,col=1:4,pch=rep(1,4), 
		legend=c(paste("Model",ModelSelection[1,3]),
				paste("Model",ModelSelection[2,3]),
				paste("Model",ModelSelection[3,3]),
				paste("Model",ModelSelection[4,3])
				)
	)

matrixcaps<-matrix(ncol=4,nrow=1000)
for(i in 1:4){
	CamerarowI	<-which(ModelSelection[i,1]==Cameras[,1])	
	CameraWidthI<-Cameras[CamerarowI,5]*2	
	CameraRadiI	<-Cameras[CamerarowI,6]
	CallAngleI	<-as.numeric(ModelSelection[i,2])*2
	print(paste("I:",i,ModelSelection[i,3]))

	
	profilewidth<-calcProfileWidth(CallAngleI, CameraWidthI, CameraRadiI )
	selectedcols<-which(header[sensorrow,]==ModelSelection[i,1] & header[callrow,]== ModelSelection[i,2])
	
	allList<-vector(mode="list",length=4)
	
	Var<-c()
	
	for(time in 1:length(selectedcols)){
		Column<-selectedcols[time]
		TimeVal<-header[timerow,Column]*StepLength
		DensityVal<-header[densityrow,Column]/Area
		Biasvector<-c()
		Numbvector<-c()
		Estvector<-c()
		allList[[time]]<-Time
		for(iteration in 1:dim(Data)[1]){
			number<- Data[iteration,Column]
			#print(paste("iteration",number))
			estimate<-(1/profilewidth[[1]])*number/(Speed*TimeVal)
			Numbvector<-c(Numbvector,number)
			Biasvector<-c(Biasvector,(estimate-DensityVal)*(1000^2))
			Estvector<-c(Estvector,estimate*(1000^2))
			Percentvector<-c(Biasvector,100*(estimate-DensityVal)*(1000^2)/(DensityVal*(1000^2)))
			#matrixcaps[i, number]<-sum(matrixcaps[i, number],Biasvector)
			if(number<2000){
			#plotCI(x=meanCaps,meanest,uiw=seest*1.96,col=i,add=T)
			points(x=number,y=(estimate-DensityVal)*(1000^2),col=i)
			
		}
		}
		meanest<-mean(Percentvector)
		sdest<-sd(Percentvector,na.rm=T)	
		seest<-sdest/10
			print(paste("Mean No.of Caps: ", mean(Numbvector), " Standard Dievation: ", sdest))
			
		#caps<-round(meanCaps/100)
		#matrixcaps[time,caps]<-
		#meanCaps<-mean(Numbvector)
		#if(meanCaps<1000){
			#plotCI(x=meanCaps,meanest,uiw=seest*1.96,col=i,add=T)
			#points(x=)
			
		#}
		testvalue<-wilcox.test(meanest, mu=0)$p.value
		if(testvalue<0.05){plotCI(err="n",x=TimeVal,y=meanest+seest*3,pch="*",col=i)
							print("SIGNIFICANT FOUND")}
		if(sd(Percentvector,na.rm=T)>5){ print(paste("STANDARD ERROR GREATER THAN 5",
													"Distance",round(TimeVal*Speed/(1000)),
													"Standard error",sd(Percentvector,na.rm=T)))}
		#if(time==1){Var=Estvector}
		#else{print(leveneTest(c(Estvector, Var)~c(rep("1",length(Estvector)),rep("0",length(Var)))))
         #}
	}
}

abline(h=0,lty=2,col="grey")
box()
#dev.off()