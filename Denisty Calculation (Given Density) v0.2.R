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

Name<-"Test,Density=70,Speed=4.6,Iterations=1-101,StepLength=300,CorrWalkMaxAngleChange=0"


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
BatSpeed		<-4.689488
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]

Camera.ID<-3
Captures.save<-Captures
Captures<-Captures[which(Captures$SensorID == Camera.ID ),]
CameraWidth	<- Cameras$HalfWidthAngle[which(Cameras$ID == Camera.ID)]*2
Radius<-Cameras$Radius[which(Cameras$ID == Camera.ID)]

#################################
# Vectors for recording density	#
#################################
EndOfSimResults	 <-list()


#################################
# Calculate density				#
#################################
# For each iteration 

#for(cameraid in 0:max(Cameras$ID)){
for(cameraid in 0:110){
#for(cameraid in 0:1){
	print(paste("cameraid: ", cameraid,"/",max(Cameras$ID),sep=""))
	Camera.ID<-cameraid
	#Captures.save<-Captures
	Captures.save<-Captures[which(Captures$SensorID == Camera.ID ),]
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
	
	Max.no.steps<-ceiling(2*Radius/(BatSpeed*StepLength))
	set.seed(1) # Seed is set here, becuase if set inside the loop then would always get the same individuals
	
	for(j in 1:floor(NoofAnimals/100)){
		J=j*100
		print(j)
		sam_animalnumbers<-sample(1:NoofAnimals,J)
		Captures.temp<-Captures.save[which(Captures.save$AnimalNumber %in% sam_animalnumbers),]
		for(i in (1:NoOfIterations)){
			# -----------------------------------------	
			# Loads in the Captures for the each iteration
			# then counts them
			CapturesInI<-(Captures.temp[which(Captures.temp$Iteration.number==(i+50)),])
			#print(paste("Captures.temp", dim(Captures.temp)))

			noofcaptures[i]<-dim(CapturesInI)[1]
			if(noofcaptures[i]>1){
				#print(paste("noofcaptures[i]", noofcaptures[i]))
				remove<-c()
				for(k in 2:dim(CapturesInI)[1]){
					if(CapturesInI$AnimalNumber[k-1] == CapturesInI$AnimalNumber[k] & # Same animal
						(CapturesInI$Time_step[k] - CapturesInI$Time_step[k-1] <= Max.no.steps ||
						 CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k]	# Same time step
						 ) ){remove<-c(remove,k)}	
				}
				if(length(remove)>0){
					temp.cap<-CapturesInI[-remove,]
					noofcaptures[i]<-dim(temp.cap)[1]
					#print(paste("tempcap", dim(temp.cap)))
				}
			}	
			# Marcus' density estimation:
			# Density = Rate.Of.Photos *  (pi / speed.of.animal*radius*(2+camera width) )
			# Rate.Of.Photos = No.Photos/Time
			if(CameraWidth<=pi){			
				D.estimate.marcus[i]<-(noofcaptures[i]/LengthMonitoring)*(pi/(BatSpeed*Radius*(2+CameraWidth)))
				}
			else if(CameraWidth==2*pi){
				D.estimate.marcus[i]<- noofcaptures[i]/(2*BatSpeed*Radius*LengthMonitoring)
				}
			else {
				D.estimate.marcus[i]<-NA
				}

		}	

		#################################
		# Printing density est.			#
		#################################
		sub.result.list<-list()

		# -------------------------------------------
		# Table of the number of captures
		# Tim's estimate
		# Marcus' estimate
		sub.result.list$Radius<-Radius
		sub.result.list$SensorWidth<-CameraWidth
		sub.result.list$MonitoringLength <- LengthMonitoring
		sub.result.list$No.of.captures<-table(noofcaptures)
		sub.result.list$Marcus.Estimate<-mean(D.estimate.marcus)/10^-6
		sub.result.list$Marcus.Estimate.sd<-sd(D.estimate.marcus/10^-6)
		sub.result.list$Marcus.Estimates<-D.estimate.marcus/10^-6

		# -----------------------------------------
		# Actual density
		sub.result.list$True.Density<-(J/Area)/10^-6
	
		all.results[[j]]<- sub.result.list
	} ### END OF LOOP

	setwd(DIR_SAVE)
	Nameplot<-paste(Name,",DensityPlot,Camera=",Camera.ID,".pdf",sep="")
	pdf(Nameplot)
	par(oma=c(3,3,3,3))
	plot(0,0, type="n", 
		xlim= c(0,all.results[[floor(NoofAnimals/100)]]$True.Density),
		ylim=c(-10,10),
		xlab= "Density of animals",
		ylab= "Percentage error",
		main= "Change in accuracy of REM given the density of animals",
		)
	
	mtext(side = 1, 
		line=5, 
		text = paste("Number of iterations = ",NoOfIterations,
					", Speed = " , BatSpeed,
					", Sensor Radius = ", Radius,
					", Sensor Width = ", CameraWidth,
					", Length of monitoring (days) = ", LengthMonitoring/(60*60*24),
					#", Animal signal width = ", 
					sep=""),
		cex=0.8)

	for(j in 1:floor(NoofAnimals/100)){
		x<-c()
		for(i in 1:NoOfIterations){x<-c(all.results[[j]]$Marcus.Estimates[i]-all.results[[j]]$Marcus.Estimate,x)}
		SD<-sd(x)
		#print(paste(SD,min(x),max(x)))
		plotCI(
			x = all.results[[j]]$True.Density,
			y = (all.results[[j]]$Marcus.Estimate- all.results[[j]]$True.Density)/all.results[[j]]$True.Density*100,
			uiw = (SD),#*1.96)/sqrt(NoOfIterations),
			add = TRUE,
			col=COLset3[1]
		)
	}	
	abline(h=0)
	dev.off()
	
	EndOfSimResults[[cameraid+1]]<-all.results[[j]]


}