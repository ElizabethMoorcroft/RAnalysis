#####################################################################
# Project title: Plots of Bat flights								#
# Project: Bat Project					                            #
#                                                                   #
# Author: Elizabeth Moorcroft                                       #
# Date created: Who knows?!                                         #
#                                                                   #
# Edited by: -                                                      #
# Edited on: -			                                            #
#                                                                   #
# Script title: Plot movement 	         	                        #
# Script purpose:Check that the code works by a providing a visable #
#				 check that capturing and movement is correct		#
#                  - 							                    #
#                  -                       							#
#                                                                   #
#####################################################################


#####################
# Libraries 		#
#####################
library("RColorBrewer")
library("jpeg")


#####################
# Change Directory	#
#####################
Name<-"Bats,Density=1,Speed=10,Iterations=1-11,StepLength=3.5,DetectorRadius=11,CallHalfwidth=3.14159,CameraHalfwidth=3.14159,CorrWalkMaxAngleChange=0,ProbChangeMoveState=0"

setwd("/Users/student/Documents/Bats/Simulations")
Camera<-read.csv(paste(Name,",Cameras.csv",sep=""))
Movement<-read.csv(paste(Name,",Movement.csv",sep=""))
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))


img.bat <- readJPEG("bat.jpg")
img.cam <- readJPEG("camera.jpg")


#####################
# Colours 			#
#####################
COL=brewer.pal(9,"Set1")

#########################################
# Values for plot						#
#########################################
Time			<-Settings[which(Settings[,1] %in% "NoSteps"),2]
Callhalfwidth	<-Settings[which(Settings[,1] %in% "Call_halfwidth"),2]*2
Camerahalfwidth	<-Settings[which(Settings[,1] %in% "CameraWidth"),2]*2
CallRadius		<-Settings[which(Settings[,1] %in% "CallRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
MinX			<-Settings[which(Settings[,1] %in% "Sq_MinX"),2]
MaxX			<-Settings[which(Settings[,1] %in% "Sq_MaxX"),2]
MinY			<-Settings[which(Settings[,1] %in% "Sq_MinY"),2]
MaxY			<-Settings[which(Settings[,1] %in% "Sq_MaxY"),2]
NoOfAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]


#########################################
# Plot one with Max captures			#
#########################################

# Identifies the iteration with the most captures
MaxCap<-as.numeric(names(table(Captures[,4])))[which(table(Captures[,4])==max(table(Captures[,4])))[1]]
if(is.na(MaxCap)){MaxCap<-1}

setwd("/Users/student/Documents/Bats/Temp")

for(i in 1){
for(step in 1:100){
	#--------------------------------
	# the movement for the iteration with max captures
	Move<-Movement[which(Movement[,9]==i & Movement$StepNumber ==step),]
	Capt<-Captures[which(Captures[,4]==i  ),]
	
	name<-paste('myplot',step/1000,'.png',sep="")
	png(name)
	
	#--------------------------------
	# The limits of the plot
	plot(
		0,0
		,xlim=c(MinX,MaxX)
		,ylim=c(MinY,MaxY)
		,type="n"
		)
	
	# ----------------------------
	# PLots the cameras
	for(j in 1:dim(Camera)[1]){
		cam<-Camera[j,]
		rasterImage(img.cam, cam$X.location-100, cam$Y.location-100, cam$X.location+100, cam$Y.location+100)
	} # END OF J LOOP
	
	for(j in 1:NoOfAnimals){
		temp<-Move[which(Move$AnimalNumber==j),]
		rasterImage(img.bat, temp$Xlocation-100, temp$Ylocation-100, temp$Xlocation+100, temp$Ylocation+100)
	}

	dev.off() # END OF SAVE PLOT
		
}	
} # END OF PLOT LOOP

