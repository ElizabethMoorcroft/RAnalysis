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
source("LoadIn.R")
setwd(DIR_CODE)
source("Output.R")


#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)


#################################
# Functions						#
#################################

PlotAddpoint<-function(x, Sensor,ModelSelect, NoOfSteps, Speed, Xval){
	


	#print(Xval*5)
	#print(Sensor[which(Sensor[,6] %in% (Xval*5)),])
	Radii	<-Sensor[which(Sensor[,6] %in% (Xval)),]
	ModelSelection<-matrix(nrow=4,ncol=3)
	
	ModelSelection[1,]<-c(Radii[1,1],3.14159, "p141") # REM
	ModelSelection[2,]<-c(Radii[1,1],1.428 , "p343") # 
	ModelSelection[3,]<-c(Radii[2,1],3.14159, "p221") # 
	ModelSelection[4,]<-c(Radii[2,1],1.428 , "p322") # 
	print(Radii)
	Output	<-CalBias(ModelSelection, Radii ,x, Speed)
	
	TimeOfInterest<-NoOfSteps

	for(ModelNumber in 1:4){
		model<-Output[[ModelNumber]]
		for(number in 1:length(model)){
			
			if(model[[number]]$Time==TimeOfInterest*(900)){
				
				meanbias<-mean(model[[number]]$Percent,na.rm=T)
				sdbias<-sd(model[[number]]$Percent,na.rm=T)	
				sebias<-sdbias/10
				print(paste("Inside plot",number, "ModelNumber",ModelNumber, "meanbias",meanbias,"sdbias",sdbias))
				plotCI(x=Xval,meanbias,uiw=sebias*1.96,col=ModelNumber,add=T)
			
			}	
		
		}
	}
	#return(sdbias)
}


#################################
# Plot				#
#################################
setwd(DIR_SAVE)
pdf("ResultsRadii.pdf")

	ModelSelection<-matrix(nrow=4,ncol=3)
	ModelSelection[1,]<-c(0,3.14159, "p141") # REM
	ModelSelection[2,]<-c(0,1.428 , "p343") # 
	ModelSelection[3,]<-c(1,3.14159, "p221") # 
	ModelSelection[4,]<-c(1,1.428 , "p322") # 

	plot(0,0,type="n",
		ylim=c(-5,5),xlim=c(0,25),
		ylab="",xlab="Radius of detector (meters)")
	mtext(text=expression(paste("Percentage error")),side=2,line=2)
	legend(x=0,y=5,col=1:4,pch=rep(1,4), 
		legend=c(paste("Model",ModelSelection[1,3]),
				paste("Model",ModelSelection[2,3]),
				paste("Model",ModelSelection[3,3]),
				paste("Model",ModelSelection[4,3])
				)
	)
	
	abline(h=0,lty=2,col="grey")
	box()
	
	
	Name<-"Run23Oct2013Radii,Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
	setwd(DIR_DATA)
	Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
	Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
	NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
	Speed<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
	loadindata<-read.csv(paste(Name,",time.csv",sep=""),header=FALSE)
	x<-loadindata[,-dim(loadindata)[2]]
	
	PrtOtpt<-vector(mode="list")
	for(i in 1:5){
		print(paste("i=",i))
		PlotAddpoint(x, Cameras, ModelSelect=0, NoOfSteps, Speed, i*5)
	}

dev.off()



