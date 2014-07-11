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


#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)

#################################
# Load data						#
#################################

setwd(DIR_DATA)
files<-list.files()

#Name<-"Run23Oct2013,Density=70,Speed=0.46,Iterations=1-51,StepLength=300,CorrWalkMaxAngleChange=3.14159"
fileno.runname<-grep("Run23Oct2013",files)
fileno.speed<-grep("Speed=0.46",files)
fileno.corr<-grep("CorrWalkMaxAngleChange=0",files)
fileno.all<- fileno.runname[which(fileno.runname %in% fileno.corr[which(fileno.corr %in% fileno.speed)])]
fileno<- fileno.all[grep("Settings",files[fileno.all])]

names<-unique(unlist(strsplit(files[fileno],",Settings.csv")))
for(i in 1:length(fileno)){
	print(paste("Loop: ",i,"/",length(fileno),sep=""))
	Name<-names[i]
	#Captures.temp<-read.csv(paste(Name,",Captures.csv",sep=""))
	Cameras.temp <-read.csv(paste(Name,",Sensors.csv",sep=""))
	Settings.temp<-read.csv(paste(Name,",Settings.csv",sep=""))
	if(i==1){
		#Captures<-Captures.temp
		Cameras<-Cameras.temp
		Settings<-Settings.temp
		}
	else{
		#Captures<-rbind(Captures,Captures.temp)
		Cameras<-merge(Cameras,Cameras.temp, by=c("X.location", "Y.location", "CentreAngle", "HalfWidthAngle","Radius"))
		Settings<-rbind(Settings,Settings.temp)
	}
}


Cameras<-unique(Cameras)
Settings<-unique(Settings)

Camera.ID<-104
for(i in 1:length(fileno)){
	print(paste("Loop: ",i,"/",length(fileno),sep=""))
	Name<-names[i]
	Captures.temp<-read.csv(paste(Name,",Captures.csv",sep=""))
	if(i==1){
		Captures<-Captures.temp[which(Captures.temp$SensorID == Camera.ID ), -which(names(Captures.temp) %in% "call")]
		}
	else{
		Captures<-rbind(Captures,Captures.temp[which(Captures.temp$SensorID == Camera.ID),-which(names(Captures.temp) %in% "call")])
	}
}


#################################
# Setting variables				#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
StepLength		<-Settings[which(Settings[,1] %in% "StepLength"),2]
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
#BatSpeed		<-4.689488
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]

Captures.save<-Captures
Camera.ID<-120

Captures.save<-Captures.save[which(Captures.save$SensorID == Camera.ID ),]
CameraWidth	<- Cameras$HalfWidthAngle[which(Cameras$ID == Camera.ID)]*2
Radius<-Cameras$Radius[which(Cameras$ID == Camera.ID)]

#################################
# Vectors for recording density	#
#################################
noofcaptures	 <-vector(length=NoOfIterations)
D.estimate		 <-vector(length=NoOfIterations)
D.estimate.marcus<-vector(length=NoOfIterations)
all.results		 <-list()

#################################
# Calculate density				#
#################################
# For each iteration 
#for(j in 1:NoOfSteps){
	
Max.no.steps<-ceiling(2*Radius/(BatSpeed*StepLength))

for(j in 1:20){
	J=j*200
	#J=j
	print(j)
	for(i in (1:50)){
		Captures.temp<-Captures.save[which(Captures.save$Time_step<=J),]
		#print(dim(Captures.temp))
		# -----------------------------------------	
		# Loads in the Captures for the each iteration
		# then counts them
		CapturesInI<-(Captures.temp[which(Captures.temp$Iteration.number==(i+50)),])
		noofcaptures[i]<-dim(CapturesInI)[1]
		if(noofcaptures[i]>1){
			remove<-c()
			for(k in 2:dim(CapturesInI)[1]){
				if(CapturesInI$AnimalNumber[k-1] == CapturesInI$AnimalNumber[k] & # Same animal
					(CapturesInI$Time_step[k] - CapturesInI$Time_step[k-1] <= Max.no.steps ||  # One time step difference
					 CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k]	# Same time step
					 ) ){remove<-c(remove,k)}	
			}
			if(length(remove)>0){
				#print(paste("remove:",remove))
				temp.cap<-CapturesInI[-remove,]
				#print(paste("dim1:",dim(temp.cap)[1]))
				noofcaptures[i]<-dim(temp.cap)[1]
			}
			#print(paste("noofcaptures[i]:",noofcaptures[i]))
		}	
		# Marcus' density estimation:
		# Density = Rate.Of.Photos *  (pi / speed.of.animal*radius*(2+camera width) )
		# Rate.Of.Photos = No.Photos/Time			
		D.estimate.marcus[i]<-(noofcaptures[i]/(J*StepLength))*(pi/(BatSpeed*Radius*(2+CameraWidth)))

	}	

	#################################
	# Printing density est.			#
	#################################
	sub.result.list<-list()

	# -------------------------------------------
	# Table of the number of captures
	# Tim's estimate
	# Marcus' estimate
	sub.result.list$Numberofsteps<-J
	sub.result.list$No.of.captures<-table(noofcaptures)
	sub.result.list$Marcus.Estimate<-mean(D.estimate.marcus)/10^-6
	sub.result.list$Marcus.Estimates<-D.estimate.marcus/10^-6
	sub.result.list$Marcus.Estimate.sd<-sd(D.estimate.marcus/10^-6)
	

	# -----------------------------------------
	# Actual density
	sub.result.list$True.Density<-(NoofAnimals/Area)/10^-6

	sub.result.list
	
	all.results[[j]]<- sub.result.list
} ### END OF LOOP

setwd(DIR_SAVE)
Nameplot<-paste(Name,",TimePlot,Camera=",Camera.ID,".pdf",sep="")
pdf(Nameplot)
par(oma=c(3,3,3,3))
plot(0,0, type="n", 
	xlim= c(0,(NoOfSteps*StepLength)/(60*60*24)),
	 ylim=c(60,80),
	xlab= "Number of days sampling",
	ylab= "Average estimate of density",
	main= "Change in accuracy of REM given monitoring effort",
	)
	
mtext(side = 1, 
	line=5, 
	text = paste("Number of iterations = ",NoOfIterations,
				", Speed = " , BatSpeed,
				", Sensor Radius = ", Radius,
				", Sensor Width = ", CameraWidth,
				#", Animal signal width = ", 
				sep=""),
	cex=0.8)
#for(j in  2:20){
#	for(i in 1:NoOfIterations){
#		points(type="l",col=COLsets[i],
#		x = c((all.results[[j-1]]$Numberofsteps*StepLength)/(60*60*24),(all.results[[j]]$Numberofsteps*StepLength)/(60*60*24)),
#		y = c(all.results[[j-1]]$Marcus.Estimates[i],all.results[[j]]$Marcus.Estimates[i])
#		)
#	}
#}
for(j in 1:20){
#for(j in  1:NoOfSteps){
	X = (all.results[[j]]$Numberofsteps*StepLength)/(60*60*24)
	Y = all.results[[j]]$Marcus.Estimate
	#print(paste("X: ", X, " Y: ",Y,sep=""))
		plotCI(
			x = (all.results[[j]]$Numberofsteps*StepLength)/(60*60*24),
			y = all.results[[j]]$Marcus.Estimate,
			uiw = (all.results[[j]]$Marcus.Estimate.sd),#*1.96)/sqrt(NoOfIterations),
			add = TRUE,
			col=COLset3[3]
		)
}
abline(h=all.results[[20]]$True.Density)
dev.off()