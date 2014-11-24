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
# Script title: Check animal movement 	         	                #
# Script purpose:													#
#																	#
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
Name<-"Test,Density=20,Speed=0.46,Iterations=1-11,StepLength=300,CorrWalkMaxAngleChange=0"


setwd("/Users/student/Documents/Bats/Simulations")
Camera<-read.csv(paste(Name,",Sensors.csv",sep=""))
Movement<-read.csv(paste(Name,",Movement.csv",sep=""))
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))


LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]
NoOfAnimals<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]


#########################################
# Plot one with Max captures			#
#########################################

# Identifies the iteration with the most captures
MaxCap<-as.numeric(names(table(Captures[,4])))[which(table(Captures[,4])==max(table(Captures[,4])))[1]]
if(is.na(MaxCap)){MaxCap<-1}

M<-Movement[which(Movement[,2]==max(Movement[,2])),]

matrix<-matrix(nrow=0,ncol=dim(M)[1])
for(i in (1:NoOfAnimals)-1){
	Mat<-matrix(nrow=0,ncol=dim(M)[1])
	values<-which(M[,1]==i)
	print(i)
	if(length(values)>1){
		V<-M[values,]
		rnames<-as.numeric(rownames(V))
		print(rnames)
		final<-max(rnames)
		Mat<-V[which(rownames(V)==final),]
	} else{Mat<-M[values,]}
	
	matrix<-rbind(matrix,Mat)
}

hist(matrix[,6])
mean(matrix[,6]/LengthMonitoring)
sd(matrix[,6]/LengthMonitoring)