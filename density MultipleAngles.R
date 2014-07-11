#####################################################################
# Project title: Calculating denisty of Bat 						#
# Project: Bat Project					                            #
#                                                                   #
# Author: Elizabeth Moorcroft                                       #
# Date created: Who knows?!                                         #
#                                                                   #
# Edited by: -                                                      #
# Edited on: -			                                            #
#                                                                   #
# Script title: Density calculation	         	                    #
# Script purpose:Calculate the density form the simulation using	#
#                  - Tim's Function				                    #
#                  - Marcus' Function                    			#
#                                                                   #
#####################################################################


#####################
# Libraries 		#
#####################
library("RColorBrewer")


#####################
# Directory			#
#####################
DIR_DATA<-"/Users/student/Documents/Bats/Simulations"
DIR_CODE<-"/Users/student/Documents/Bats/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")


#####################
# Colours 			#
#####################
COL=brewer.pal(9,"Set1")


#################################
# Load data						#
#################################


Name<-"Bats,Density=5,Speed=10,Iterations=1-11,StepLength=3.5,DetectorRadius=11,CallHalfwidth=3.14159,CameraHalfwidth=3.14159,CorrWalkMaxAngleChange=0.349066,ProbChangeMoveState=0"

setwd(DIR_DATA)
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
Cameras	<-read.csv(paste(Name,",Cameras.csv",sep=""))
Settings<-read.csv(paste(Name,"Settings.csv",sep=""))



#################################
# Setting variables				#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
if(length(NoOfSteps)==0){NoOfSteps<-Settings[which(Settings[,1] %in% " NoSteps"),2]}
CallWidth		<-Settings[which(Settings[,1] %in% "Call_halfwidth"),2]*2
CameraWidth		<-Settings[which(Settings[,1] %in% "CameraWidth"),2]*2
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]

#################################
# Vectors for recording density	#
#################################
noofcaptures	 <-vector(length=NoOfIterations)
D.estimate		 <-vector(length=NoOfIterations)
D.estimate.marcus<-vector(length=NoOfIterations)


all.result.list<-list()

Captures$Angle.from.centre.of.camera.to.bat<-ceiling(abs(Captures$Angle.from.centre.of.camera.to.bat*180/pi))
Captures$Angle.from.bat.to.camera<-ceiling(abs(Captures$Angle.from.bat.to.camera*180/pi))

temp1<-c(1:180)
temp2<-c()
for(i in 1:180){temp2<-c(temp2,rep.int(i,180))}
angles<-cbind(temp1,temp2)

for(btc in 1:32400){

cameraangles<-angles[btc,1]
batangles<-angles[btc,2]


CapturesAngle<-Captures[which(Captures$Angle.from.centre.of.camera.to.bat<cameraangles & Captures$Angle.from.bat.to.camera<batangles ),]

for(i in (1:NoOfIterations)){

	# -----------------------------------------	
	# Loads in the Captures for the each iteration, then counts them
	CapturesInI<-(CapturesAngle[which(CapturesAngle$Iteration.number==(i)),])
	noofcaptures[i]<-dim(CapturesInI)[1]
	
	# -----------------------------------------	
	# Removes duclicates for the same collision	
	if(noofcaptures[i]>1){
		remove<-c()
		for(k in 2:dim(CapturesInI)[1]){
			if(CapturesInI$AnimalNumber[k-1] == CapturesInI$AnimalNumber[k] & # Same animal
				(CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k] -1 ||  # One time step difference
				 CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k]	# Same time step
				 ) ){remove<-c(remove,k)}	
		}
		if(length(remove)>0){noofcaptures[i]<-dim(CapturesInI[-remove,])[1]}
	}
	# -----------------------------------------	
	# Tim's function
	D.estimate[i]<-ImmobileDens(captures = noofcaptures[i]
							,time = LengthMonitoring
							,cam.angle = cameraangles
							,call.angle = batangles
							,r = CameraCallRadius
							,v = BatSpeed
							)$Density

}

#################################
# Printing density est.			#
#################################
sub.result.list<-list()

# -------------------------------------------
# Table of the number of captures
# Tim's estimate
# Marcus' estimate
sub.result.list$No.of.captures<-table(noofcaptures)
sub.result.list$Mean.No.of.captures<-mean(noofcaptures,na.rm=T)
sub.result.list$Tims.Estimate<-mean(D.estimate)/10^-6
sub.result.list$Tims.Estimate.sd<-sd(D.estimate/10^-6)


# -----------------------------------------
# Actual density
sub.result.list$True.Density<-(NoofAnimals/Area)/10^-6
sub.result.list$Bat.Angle<-batangles
sub.result.list$Camera.Angle<-cameraangles



all.result.list[[btc]]<-sub.result.list
}
