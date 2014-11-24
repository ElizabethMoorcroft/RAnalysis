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


Name<-"Paper,Density=70,Speed=0.46,Iterations=1-26,StepLength=120,DetectorRadius=11,CallHalfwidth=3.14159,CameraHalfwidth=3.14159,CorrWalkMaxAngleChange=0"
setwd(DIR_DATA)

setwd(DIR_DATA)
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
Cameras	<-read.csv(paste(Name,",Cameras.csv",sep=""))
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))




#################################
# Setting variables				#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
if(length(NoOfSteps)==0){NoOfSteps<-Settings[which(Settings[,1] %in% " NoSteps"),2]}
CallWidth		<-Settings[which(Settings[,1] %in% "Call_halfwidth"),2]*2
CameraWidth		<- 2*(45/180)*pi #<-Settings[which(Settings[,1] %in% "CameraWidth"),2]*2
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]


Captures.save<-Captures
Captures<-Captures[which(abs(Captures$Angle.from.centre.of.camera.to.bat) < =0.7853982 ),]

#NoSteps <- 1.54300*10^3
#LengthMonitoring<- 1.5*60*60
#################################
# Vectors for recording density	#
#################################
noofcaptures	 <-vector(length=NoOfIterations)
D.estimate		 <-vector(length=NoOfIterations)
D.estimate.marcus<-vector(length=NoOfIterations)

#plot(0,0,type="n",xlim=c(1,NoOfIterations),ylim=c(0,2))
plot(0,0,
	type="n",
	xlim=c(1,NoOfIterations),ylim=c(40,60),
	xlab="Simulation number",ylab="Average estimated density (per km^2)",
	main="True density = 5(per km^2), Density estimated using REM",
	sub="Blue = Calls in range; Red - Time in range"
	)


Captures.temp<-Captures
Time.temp  <-LengthMonitoring


#################################
# Calculate density				#
#################################
# For each iteration 
for(i in (1:NoOfIterations)){
	#print(i)
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
		if(length(remove)>0){noofcaptures[i]<-dim(CapturesInI[-remove,])[1]}
	}

	# Marcus' density estimation:
	# Density = Rate.Of.Photos *  (pi / speed.of.animal*radius*(2+camera width) )
	# Rate.Of.Photos = No.Photos/Time			
	D.estimate.marcus[i]<-(noofcaptures[i]/Time.temp)*(pi/(BatSpeed*CameraCallRadius*(2+CameraWidth)))
	points(x=i,y=mean(D.estimate.marcus[1:i]/10^-6),col="red")	
		
} ### END OF LOOP


#################################
# Printing density est.			#
#################################
sub.result.list<-list()

# -------------------------------------------
# Table of the number of captures
# Tim's estimate
# Marcus' estimate
sub.result.list$No.of.captures<-table(noofcaptures)
sub.result.list$Marcus.Estimate<-mean(D.estimate.marcus)/10^-6
sub.result.list$Marcus.Estimate.sd<-sd(D.estimate.marcus/10^-6)

# -----------------------------------------
# Actual density
sub.result.list$True.Density<-(NoofAnimals/Area)/10^-6

sub.result.list
abline(h=sub.result.list$True.Density,lty=2)
