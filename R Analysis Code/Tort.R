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

COLmodel<-matrix(ncol=2,nrow=4)
COLmodel[1,]<-c("NW1", COLset3[3])
COLmodel[2,]<-c("SW1", COLset3[11])
COLmodel[3,]<-c("NE1", COLset3[1])
COLmodel[4,]<-c("SE3", COLset3[11])


#################################
# Functions						#
#################################

PlotAddpoint<-function(x, Cameras,ModelSelect, NoOfSteps, Speed, Xval){
	datafortim<-matrix(ncol=102,nrow=0);
	print("PlotAddpoint")
	if(ModelSelect ==1){
		ModelSelection<-matrix(nrow=4,ncol=3)
		ModelSelection[1,]<-c(0,3.14159, "p141") # REM
		ModelSelection[2,]<-c(0,1.428 , "p343") # 
		ModelSelection[3,]<-c(1,3.14159, "p221") # 
		ModelSelection[4,]<-c(1,1.428 , "p322") # 
	} else if(ModelSelect == 0){
		ModelSelection<-matrix(nrow=4,ncol=3)
		ModelSelection[1,]<-c(2,3.14159, "p141") # REM
		ModelSelection[2,]<-c(2,1.428 , "p343") # 
		ModelSelection[3,]<-c(4,3.14159, "p221") # 
		ModelSelection[4,]<-c(4,1.428 , "p322") # 
	}

	Output<-CalBias(ModelSelection, Cameras,x, Speed)
	print("Finished Output")
	TimeOfInterest<-14400
	#TimeOfInterest<-NoOfSteps/2

	for(ModelNumber in 1:4){
		print(paste("ModelNumber:", ModelNumber))
		model<-Output[[ModelNumber]]
		for(number in 1:length(model)){
			print(paste("number:", number,"model[[number]]$Time",model[[number]]$Time))
			if(model[[number]]$Time==TimeOfInterest*(900)){
				
				meanbias<-mean(model[[number]]$Percent,na.rm=T)
				sdbias<-sd(model[[number]]$Percent,na.rm=T)	
				sebias<-sdbias/10
				print(paste("Inside plot",number, "ModelNumber",ModelNumber, "meanbias",meanbias,"sdbias",sdbias))
				temp<-matrix(ncol=102,nrow=1,c(ModelNumber,Xval,model[[number]]$Percent));
				datafortim<-rbind(datafortim,temp)
				w.test<-wilcox.test(model[[number]]$Bias,mu=0)$p.value
				print(paste("test value: ", w.test))
				#plotCI(x=Xval,meanbias,uiw=sebias*1.96,col=ModelNumber,add=T)
				boxplot(model[[number]]$Percent,add=T,at=Xval)
			}	
		}
	}
	return(datafortim)
}


#################################
# Plot				#
#################################
setwd(DIR_IMG)
#pdf("ResultsTort.pdf")
	datafortim<-matrix(ncol=102,nrow=0);
	
	ModelSelection<-matrix(nrow=4,ncol=3)
	ModelSelection[1,]<-c(0,3.14159, "REM") # REM
	ModelSelection[2,]<-c(0,1.428 , "") # 
	ModelSelection[3,]<-c(1,3.14159, "p221") # 
	ModelSelection[4,]<-c(1,1.428 , "p322") # 
	
	par(mfrow=c(4,1))
	plot(0,0,type="n",
		ylim=c(-5,5),
		xlim=c(-0.5,pi),
		ylab="",xlab="Angle of correlated walk")
	mtext(text=expression(paste("Percentage error")),side=2,line=2)
	#legend(x=0,y=5,col=1:4,pch=rep(1,4), 
	#	legend=c(paste("Model",ModelSelection[1,3]),
	#			paste("Model",ModelSelection[2,3]),
	#			paste("Model",ModelSelection[3,3]),
	#			paste("Model",ModelSelection[4,3])
	#			)
	#)
	
	abline(h=0,lty=2,col="grey")
	box()

	Name<-"Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=3.14159"
	setwd(DIR_DATA)
	Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
	Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
	NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
	Speed<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
	Tort	 <-Settings[which(Settings[,1] %in% "CorrWalkMaxAngleChange"),2]
	loadindata<-read.csv(paste(Name,",timenosubs14400.csv",sep=""),header=FALSE)
	x<-loadindata[,-dim(loadindata)[2]]
	temp<-PlotAddpoint(x, Cameras,ModelSelect=1, NoOfSteps, Speed, Tort)
	datafortim<-rbind(datafortim,temp)
	
	Name<-"Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=1.0472"
	setwd(DIR_DATA)
	Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
	Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
	NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
	Speed<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
	Tort	 <-Settings[which(Settings[,1] %in% "CorrWalkMaxAngleChange"),2]
	loadindata<-read.csv(paste(Name,",timenosubs14400.csv",sep=""),header=FALSE)
	x<-loadindata[,-dim(loadindata)[2]]
	temp<-PlotAddpoint(x, Cameras,ModelSelect=1, NoOfSteps, Speed, Tort)
	datafortim<-rbind(datafortim,temp)

	Name<-"Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=2.0944"
	setwd(DIR_DATA)
	Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
	Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
	NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
	Tort	 <-Settings[which(Settings[,1] %in% "CorrWalkMaxAngleChange"),2]	
	Speed<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
	loadindata<-read.csv(paste(Name,",timenosubs14400.csv",sep=""),header=FALSE)
	x<-loadindata[,-dim(loadindata)[2]]
	temp<-PlotAddpoint(x, Cameras,ModelSelect=1, NoOfSteps, Speed, Tort)
	datafortim<-rbind(datafortim,temp)
	
	Name<-"Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
	setwd(DIR_DATA)
	Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
	Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
	NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
	Tort	 <-Settings[which(Settings[,1] %in% "CorrWalkMaxAngleChange"),2]	
	Speed<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
	loadindata<-read.csv(paste(Name,",timenosubs14400.csv",sep=""),header=FALSE)
	x<-loadindata[,-dim(loadindata)[2]]
	temp<-PlotAddpoint(x, Cameras, ModelSelect=1, NoOfSteps, Speed, Tort)
	datafortim<-rbind(datafortim,temp)

#dev.off()

model<-c("NW1","SW1","SE3","NE1")
#xlab="Maximum change in direction at each step (radians)"

setwd(DIR_IMG)
pdf("ResultsTort.pdf")
par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
for(i in 1:4){
	COL<-COLmodel[which(COLmodel[,1]==model[i]),2]
	plot(0,0,type="n",
		ylim=c(-6,6),
		xlim=c(-0.2,pi+0.2),
		axes=FALSE,
		ylab="",xlab=""
		,main="")
	box()
	axis(1,labels=FALSE,at=c(0,pi/3,2*pi/3,pi) )
	axis(2,labels=FALSE,at=c(-4,0,4) )

	#mtext(side=1,at=c(0,pi/3,2*pi/3,pi),text=expression(0,pi/3,2*pi/3,pi),line=1)
	mtext(side=2,at=c(-4,0,4),text=expression(-4,0,4),line=1)
	mtext(text=model[i],side=3,line=0)
	v<-vector(length=4)
	temp<-datafortim[which(datafortim[,1]==i),]
	tort<-sort(unique(temp[,2]))
	for(j in 1:length(tort)){
		d<-temp[which(temp[,2]==tort[j]),2:102]
		d<-as.matrix(d)
		boxplot(d[,1],at=tort[j],add=T,col=COL,axes=FALSE)
		v[j]<-var(d[,1])
		
	}
	print(v)
	#print(var.test(temp[which(temp[,2]==tort[4]),2:102],temp[which(temp[,2]==tort[1]),2:102],alternative="greater"))
	abline(h=0,col="grey",lty=2)
}
mtext(side=1,at=c(0,pi/3,2*pi/3,pi),text=expression(0,pi/3,2*pi/3,pi),line=1)
mtext(side=1,text="Maximum change in direction at each step (radians)",line=1.5,outer=TRUE)
mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
dev.off()