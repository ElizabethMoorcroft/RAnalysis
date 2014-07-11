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


rm(list=ls(all=TRUE)) 

#####################
# Libraries 		#
#####################
library("RColorBrewer")


#####################
# Change Directory	#
#####################
Name<-"TestCaps,Density=70,Speed=4.6,Iterations=54-55,StepLength=300,CorrWalkMaxAngleChange=0"


setwd("/Users/student/Documents/Bats/Simulations")
Camera<-read.csv(paste(Name,",Sensors.csv",sep=""))
Movement<-read.csv(paste(Name,",Movement.csv",sep=""))
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))


Captures<-Captures[which(Captures$SensorID==0),]
hist(Movement$Distance.to.HR.centre, breaks=seq(0,7000,by=10),xlim=c(0,2000))

#####################
# Colours 			#
#####################
COL=brewer.pal(9,"Set1")

#########################################
# Values for plot						#
#########################################
Time			<-Settings[which(Settings[,1] %in% " NoSteps"),2]*0.35
Callhalfwidth	<-Settings[which(Settings[,1] %in% "Call_halfwidth"),2]*2
Camerahalfwidth	<-Settings[which(Settings[,1] %in% "CameraWidth"),2]*2
CallRadius		<-Settings[which(Settings[,1] %in% "CallRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
MinX			<-Settings[which(Settings[,1] %in% "Sq_MinX"),2]
MaxX			<-Settings[which(Settings[,1] %in% "Sq_MaxX"),2]
MinY			<-Settings[which(Settings[,1] %in% "Sq_MinY"),2]
MaxY			<-Settings[which(Settings[,1] %in% "Sq_MaxY"),2]

LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]
NoOfAnimals<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]


#########################################
# Plot one set of movements				#
#########################################


for(i in 54){
	#--------------------------------
	# the movement for the iteration with max captures
	Move<-Movement[which(Movement[,9]==i  & Movement$AnimalNumber <10 ),]
	Capt<-Captures[which(Captures[,4]==i  & Captures$SensorID ==3 & Captures$AnimalNumber <10 ),]

	#--------------------------------
	# For saving
	#name<-paste("MovementPlot",i,".pdf",sep="")
	#pdf(name)
	
	Move<-Move[which(Move$Xlocation >1000 & Move$Ylocation >1000),]
	Move<-Move[which(Move$Xlocation <6000 & Move$Ylocation <6000),]
	
	#--------------------------------
	# The limits of the plot
	plot(
		0,0
		#,xlim=c(MinX,MaxX)
		#,ylim=c(MinY,MaxY)
		,type="n"
		,xlim=c(3745,3775)
		,ylim=c(3745,3775)
		)
	
	# ----------------------------
	# PLots the cameras
	for(j in 1){
	#:dim(Camera)[1]){
		cam<-Camera[j,]
		points(  x=c(cam$X.location,cam$X.location+CallRadius*sin(cam$CentreAngle+cam$HalfWidthAngle))
				,y=c(cam$Y.location,cam$Y.location+CallRadius*cos(cam$CentreAngle+cam$HalfWidthAngle))
				,col="red"
				,type="l")
		points(  x=c(cam$X.location,cam$X.location+CallRadius*sin(cam$CentreAngle-cam$HalfWidthAngle))
				,y=c(cam$Y.location,cam$Y.location+CallRadius*cos(cam$CentreAngle-cam$HalfWidthAngle))
				,col="red"
				,type="l")
	} # END OF J LOOP
	
	# -----------------------------
	# Plots the location of any animal when captured
	# Plots the cameras where the capture took place as black
	for(k in 1:dim(Capt)[1]){
		points(Capt$X.location[k],Capt$Ylocation[k],pch="*",col="blue")
	}
	##print(i)
	# Plots the movement
	for(j in 2:dim(Move)[1]){
		# The movement is selected so that the leaving and re-entering is not drawn on the plot
		# and there isn't a line been animal n and n+1
		if( Move$Re.enterWorld[j]== 0 &
			Move$AnimalNumber[j] == Move$AnimalNumber[j-1] &
			Move$StepNumber[j] -1 == Move$StepNumber[j-1]
			){
				print(j)
			# Plot lines 
			points(
				 x=Move$Xlocation[c(j-1,j)]
				,y=Move$Ylocation[c(j-1,j)]
				,col=Move$AnimalNumber[j]+1
				,type="l"
				)		
		} #END oF IF STATEMENT
	} #END OF J FOR LOOP STATEMENT
	
	#dev.off() # END OF SAVE PLOT
} # END OF PLOT LOOP


	#:dim(Camera)[1]){
		cam<-Camera[j,]
		points(  x=c(cam$X.location,cam$X.location+CallRadius*sin(cam$CentreAngle+cam$HalfWidthAngle))
				,y=c(cam$Y.location,cam$Y.location+CallRadius*cos(cam$CentreAngle+cam$HalfWidthAngle))
				,col="red"
				,type="l")
		points(  x=c(cam$X.location,cam$X.location+CallRadius*sin(cam$CentreAngle-cam$HalfWidthAngle))
				,y=c(cam$Y.location,cam$Y.location+CallRadius*cos(cam$CentreAngle-cam$HalfWidthAngle))
				,col="red"
				,type="l")
	} # END OF J LOOP
	
	# -----------------------------
	# Plots the location of any animal when captured
	# Plots the cameras where the capture took place as black
	for(k in 1:dim(Capt)[1]){
		points(Capt$X.location[k],Capt$Ylocation[k],pch="*",col="blue")
		
		# Prints the distance from the bat to the camera when captured:
	#	temp<-sqrt((Capt$X.location[k]-cam$X.location)^2+(Capt$Ylocation[k]-cam$Y.location)^2)
	#	print(paste("The distance from bat to camera when captured",temp))
	}
		
	#dev.off() # END OF SAVE PLOT
#} # END OF PLOT LOOP

