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
fileno.runname<-grep("Run23Oct2013Perch",files)
fileno.speed<-grep("Speed=0.46",files)
fileno.corr<-grep("CorrWalkMaxAngleChange=0",files)
fileno.all<- fileno.runname[which(fileno.runname %in% fileno.corr[which(fileno.corr %in% fileno.speed)])]
fileno<- fileno.all[grep("Settings",files[fileno.all])]

names<-unique(unlist(strsplit(files[fileno],",Settings.csv")))
for(i in 1:length(fileno)){
	print(paste("Loop: ",i,"/",length(fileno),sep=""))
	Name<-names[i]
	Cameras.temp <-read.csv(paste(Name,",Sensors.csv",sep=""))
	Settings.temp<-read.csv(paste(Name,",Settings.csv",sep=""))
	if(i==1){
		Cameras<-Cameras.temp
		Settings<-Settings.temp
		}
	else{
		Cameras<-merge(Cameras,Cameras.temp, by=c("X.location", "Y.location", "CentreAngle", "HalfWidthAngle","Radius"))
		Settings<-rbind(Settings,Settings.temp)
	}
}
Cameras<-unique(Cameras)
Settings<-unique(Settings)


#################################
# Setting variables				#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
Seed			<-Settings[which(Settings[,1] %in% "Seed"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
StepLength		<-Settings[which(Settings[,1] %in% "StepLength"),2]
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]


EndOfSimResults<-list()

####################################
# Loading captures
#	- time consuming 
#	- Can max out memory 
####################################

# to avoid the memory maxing out some unnecessary columns are removed, this is the list
includedcolumns<-c(	"AnimalNumber",
					"Time_step",
					"SensorID",
					"Iteration.number",
					"Angle.from.animal.to.sensor"
					)
# Each capture file is read in and added to the full list 
for(i in 1:length(fileno)){
	print(paste("Loop: ",i,"/",length(fileno),sep="")) # Printsthe iteration number to the screen
	Name<-names[i]

	if(i==1){
		Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
		Captures<-Captures[,which(names(Captures) %in% includedcolumns)]
	} 
	else{
		Captures.temp<-read.csv(paste(Name,",Captures.csv",sep=""))
		Captures.temp<-Captures.temp[,which(names(Captures.temp) %in% includedcolumns)]
		Captures<-rbind(Captures,Captures.temp)
		rm(Captures.temp)
	}
}

nameswithit<-strsplit(names[1],"Iterations=1-51,")
for( i in 0:max(Cameras$ID.x)){
	mergedname<-paste(nameswithit[[1]][1],nameswithit[[1]][2],",SensorID=",i,".csv",sep="")
	caps<-Captures[which(Captures$SensorID==i),]
	write.csv(caps,mergedname, row.names =FALSE)
}

nameswithit<-strsplit(names[1],"Iterations=1-51,")
#for(cameraid in 0:max(Cameras$ID)){
for(cameraid in 230:max(Cameras$ID.x)){
	print(paste("cameraid: ", cameraid,"/",max(Cameras$ID.x),sep=""))
	Camera.ID<-cameraid
	
	Captures.save<-read.csv(paste(nameswithit[[1]][1],nameswithit[[1]][2],",SensorID=",i,".csv",sep=""))
	CameraWidth	<- Cameras$HalfWidthAngle[which(Cameras$ID.x == Camera.ID)]*2
	Radius<-Cameras$Radius[which(Cameras$ID.x == Camera.ID)]

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

	for(j in 1:10){
		#print(paste("J: ", j,"/",10,sep=""))

		J=j*400
		Captures.temp<-Captures.save[which(Captures.save$Time_step<=J),]
		for(i in (1:50)){
			#print(paste("I: ", i,"/",50,sep=""))

			# -----------------------------------------	
			# Loads in the Captures for the each iteration
			# then counts them
			CapturesInI<-(Captures.temp[which(Captures.temp$Iteration.number==(i)),])
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
			if(CameraWidth<=pi){			
				D.estimate.marcus[i]<-(noofcaptures[i]/(J*StepLength))*(pi/(BatSpeed*Radius*(2+CameraWidth)))
				}
			else if(CameraWidth==2*pi){
				D.estimate.marcus[i]<- noofcaptures[i]/(2*BatSpeed*Radius*J*StepLength)
				}
			else {
				D.estimate.marcus[i]<-(noofcaptures[i]/(J*StepLength))*(pi/(BatSpeed*Radius*(2*sin(CameraWidth/2)+CameraWidth)))
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
		sub.result.list$Numberofsteps<-J
		sub.result.list$Radius<-Radius
		sub.result.list$SensorWidth<-CameraWidth
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

	EndOfSimResults[[1]]<-all.results[[10]]
}