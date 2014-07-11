#---------------------------------------------------------------------
# Project title: Creating movie of simulated movement				 		
# Project: Detector Project					                        		
#                                                                   
# Author: Elizabeth Moorcroft                                       
# Date created: 27thSept	                                        	
#                                                                   
# Edited by: -                                                      
# Edited on: -			                                            	
#                                                                   
# Script title: Movie Script	         	                        
# Script purpose:Creates a series of png files that can be turned   
#				  into a movie										
#                  							                    
#---------------------------------------------------------------------

# R libraries
library("RColorBrewer")
library("jpeg")
library("png")

# Directories
Data<-"/Users/student/Documents/Bats/Simulations"
Save<-"/Users/student/Documents/Bats/Movie"
Pictures<-"/Users/student/Desktop"

# Name of simulation
Name<-"Run23Oct2013RTESTcompare,Perch0,Density=0.5,Speed=0.46,Iterations=1-2,StepLength=300,CorrWalkMaxAngleChange=0.436332"

# Load in Movement and Setting files (if it's too long then in may not load)
setwd(Data)
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
Movement<-read.csv(paste(Name,",Movement.csv",sep=""))
Sensor<-read.csv(paste(Name,",Sensors.csv",sep=""))
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))

# Load images of animal and detector
setwd(Pictures)
img.animal <- readPNG("bat.png")
img.sensor <- readPNG("Mic.png")
img.size<- 15

##
blue.col=brewer.pal(9,"Blues")[3]
red.col=brewer.pal(9,"Reds")[3]


# Values for plot
MinX<-Settings[which(Settings[,1] %in% "Sq_MinX"),2]
MaxX<-Settings[which(Settings[,1] %in% "Sq_MaxX"),2]
MinY<-Settings[which(Settings[,1] %in% "Sq_MinY"),2]
MaxY<-Settings[which(Settings[,1] %in% "Sq_MaxY"),2]

#
NoOfAnimals<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]

#polygon
polygonlist<-vector(mode="list",length=dim(Sensor)[1])
for(j in 1:dim(Sensor)[1]){
	sen<-Sensor[j,]
	sublist<-vector(mode="list",length=2)
	x<-c(sen$X.location)
	y<-c(sen$Y.location)
	MaxAngle<-round(sen$HalfWidthAngle*180/pi)
	Radii<-sen$Radius
	for(angle in -MaxAngle:MaxAngle){
		x<-c(x,Radii*cos((angle+90)*pi/180)+sen$X.location)
		y<-c(y,Radii*sin((angle+90)*pi/180)+sen$Y.location)	
	}
	sublist[[1]]<-x;sublist[[2]]<-y;
	polygonlist[[j]]<-sublist
} # END OF J LOOP


# Selects the first iteration of the model
iteration<-1#as.numeric(names(table(Movement$Iternation.number))[1])

setwd(Save)
	
#Numbering system
NoSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
nodigits<-length(strsplit(as.character(NoSteps),"")[[1]])

Time<-10
	
# For each time step a frame is created
for(step in 1:Time){
		# the movement for the iteration with max captures
		Move<-Movement[which(Movement$StepNumber ==step),]
		Capt<-Captures
		
		# Creates a save name which is "myplot000XX" so that they are automatically in the correct order in folder
		nodigitsframe<-length(strsplit(as.character(step),"")[[1]])
		nozeros<-nodigits - nodigitsframe
		if(nozeros>0){for(i in 1:nozeros){number<-paste(paste(rep(0,nozeros), collapse = ''),step,sep="")}}
		name<-paste('myplot',number,'.png',sep="")
		
		# Opens plot
		png(name)
	
		#--------------------------------
		# The limits of the plot
		plot(
			0,0
			,xlab=""
			,ylab=""
			,main=NULL
			,axes=FALSE
			,xlim=c(MinX,MaxX)
			,ylim=c(MinY,MaxY)
			,type="n"
			)
		box()
		#boxed.labels(MaxX*2/3 ,MaxY*7/8, "Number of captures: 1",ypad=5)
		# ----------------------------
		# Plots the sensors
		for(j in 1:dim(Sensor)[1]){
			polygon(polygonlist[[j]][[1]],polygonlist[[j]][[2]],col=blue.col,lty=0)
			sen<-Sensor[j,]
			rasterImage(img.sensor, # Image used
					sen$X.location-img.size, sen$Y.location-img.size*2, # Left bottom coordinates
					sen$X.location+img.size, sen$Y.location+img.size*2) # right top coordinates
		} # END OF J LOOP
		
		# ----------------------------
		# Plots animals
		for(j in 1:NoOfAnimals){
			
			temp<-Move[which(Move$AnimalNumber==(j-1)),]
			
			animallist<-vector(mode="list",length=dim(Sensor)[1])
			sublist<-vector(mode="list",length=2)
			x<-c(temp$Xlocation)
			y<-c(temp$Ylocation)
			MaxAngle<-round(sen$HalfWidthAngle*180/pi)
			Radii<-sen$Radius
			for(angle in -MaxAngle:MaxAngle){
				x<-c(x,Radii*cos((angle+90)*pi/180)+temp$Xlocation)
				y<-c(y,Radii*sin((angle+90)*pi/180)+temp$Ylocation)	
			}
			sublist[[1]]<-x;sublist[[2]]<-y;
			animallist[[j]]<-sublist
		} # END OF J LOOP
		polygon(animallist[[j]][[1]],animallist[[j]][[2]],col=red.col,lty=0)
			
		rasterImage(img.animal, # Name 
						temp$Xlocation-img.size, temp$Ylocation-img.size, # Left bottom coordinates
						temp$Xlocation+img.size*2, temp$Ylocation+img.size) # right top coordinates

		dev.off() # Closes Plot
		
}	# end of time step
	


