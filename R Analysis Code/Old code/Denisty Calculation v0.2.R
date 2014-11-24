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
COLset3=brewer.pal(12,"Set3")


#################################
# Load data						#
#################################

Name<-"Test,Density=10,Speed=4.6,Iterations=1-201,StepLength=300,CorrWalkMaxAngleChange=0"


setwd(DIR_DATA)
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
Cameras	<-read.csv(paste(Name,",Sensors.csv",sep=""))
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))




#################################
# Setting variables				#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
StepLength		<-Settings[which(Settings[,1] %in% "StepLength"),2]
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]

Camera.ID<-7
Captures.save<-Captures
Captures<-Captures[which(Captures$SensorID == Camera.ID ),]
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
for(j in 1:NoSteps){
	for(i in (1:NoOfIterations)){
		Captures.temp<-Captures[which(Captures$Time_step<j),]
		# -----------------------------------------	
		# Loads in the Captures for the each iteration
		# then counts them
		CapturesInI<-(Captures.temp[which(Captures.temp$Iteration.number==(i)),])
		noofcaptures[i]<-dim(CapturesInI)[1]
		if(noofcaptures[i]>1){
			remove<-c()
			for(k in 2:dim(CapturesInI)[1]){
				if(CapturesInI$AnimalNumber[k-1] == CapturesInI$AnimalNumber[k] & # Same animal
					(CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k] -1 ||  # One time step difference
					 CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k]	# Same time step
					 ) ){remove<-c(remove,k)}	
			}
			if(length(remove)>0){
				temp.cap<-CapturesInI[-remove,]
				noofcaptures[i]<-dim(temp.cap)[1]
			}
		}	
		# Marcus' density estimation:
		# Density = Rate.Of.Photos *  (pi / speed.of.animal*radius*(2+camera width) )
		# Rate.Of.Photos = No.Photos/Time			
		D.estimate.marcus[i]<-(noofcaptures[i]/(j*sizej))*(pi/(BatSpeed*Radius*(2+CameraWidth)))

	}	

	#################################
	# Printing density est.			#
	#################################
	sub.result.list<-list()

	# -------------------------------------------
	# Table of the number of captures
	# Tim's estimate
	# Marcus' estimate
	sub.result.list$Numberofsteps<-j
	sub.result.list$No.of.captures<-table(noofcaptures)
	sub.result.list$Marcus.Estimate<-mean(D.estimate.marcus)/10^-6
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

StepLength<-5*60
for(j in 1:NoSteps){
	plotCI(
		x = (all.results[[j]]$Numberofsteps*StepLength)/(60*60*24),
		y = all.results[[j]]$Marcus.Estimate,
		uiw = (all.results[[j]]$Marcus.Estimate.sd*1.96)/sqrt(NoOfIterations),
		add = TRUE,
		col=COLset3[1]
	)
}
abline(h=all.results[j]]$True.Density)
dev.off()