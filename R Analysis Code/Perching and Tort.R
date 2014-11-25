#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Bat Project					                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: Who knows?!                                         	#
#                                                                  	 	#
# Edited by: E.Moorcroft                                                #
# Edited on: 25Nov14	                                            	#
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
#DIR_IMG<-"/Users/student/Documents/Bats/lucasMoorcroftManuscript/imgs"
DIR_IMG<-"/Users/student/Documents/Bats/Temp"
DIR_CODE<-"/Users/student/Documents/Bats/RAnalysis/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")
source("calculateProfileWidth.R")
source("LoadIn.R")
setwd(DIR_CODE)
source("Output.R")


#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)

COLmodel<-matrix(ncol=2,nrow=4)
COLmodel[1,]<-c("NW1", COLset3[3])
COLmodel[2,]<-c("SW1", COLset3[11])
COLmodel[3,]<-c("NE1", COLset3[1])
COLmodel[4,]<-c("SE3", COLset3[11])


#################################
# Model selection				#
#################################

r<-10 # radius
t<-14400 # time

# Camera angle; Call angle; Radius; Time; Name
ModelSelection<-matrix(nrow=4,ncol=5)
ModelSelection[1,]<-c(1.0472,3.14159 , r, t,"NW1") # p141
ModelSelection[2,]<-c(1.0472,1.428   , r, t,"SW1") # p343
ModelSelection[3,]<-c(1.74533,3.14159, r, t,"NE1") # p221
ModelSelection[4,]<-c(1.74533,1.428  , r, t,"SE3") # p322



#################################
# Functions						#
#################################



#################################
# Plot - perching               #
#################################

##Create data set
time.perch<-c(0,0.25,0.5,0.75)
names.base<-lapply(time.perch,function(x){paste("Run23Oct201317July", x, ",Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,", sep="")})

setwd(DIR_DATA)
biases<-calculate.all.biases(Names=paste(names.base,"timenosubs14400.csv",sep=""),
                            Names.Cameras=paste(names.base,"Sensors.csv",sep=""),
                            ModelSelection,
                            Speed=0.46,
                            Density=6.999111e-05,
                            StepLength=900)

setwd(DIR_IMG)
pdf("ResultsPerch.pdf")
    par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
    plot.biases.boxplot(all.data=biases,colour.list=COLmodel,model.name=ModelSelection[,5])
    mtext(side=1,at=c(0,1,2,3),text=expression(0,0.25,0.5,0.75),line=1)
    mtext(side=1,text="Proportion of time spent stationary",line=1.5,outer=TRUE)
    mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
dev.off()


#################################
# Plot - Tort                   #
#################################

##Create data set
tort<-c(0,1.0472,2.0944,3.14159)
names.base<-lapply(tort,function(x){paste("Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=", x,",",sep="")})

setwd(DIR_DATA)
biases<-calculate.all.biases(Names=paste(names.base,"timenosubs14400.csv",sep=""),
                            Names.Cameras=paste(names.base,"Sensors.csv",sep=""),
                            ModelSelection,
                            Speed=0.46,
                            Density=6.999111e-05,
                            StepLength=900)

setwd(DIR_IMG)
pdf("ResultsTort.pdf")
    par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
    plot.biases.boxplot(all.data=biases,colour.list=COLmodel,model.name=ModelSelection[,5])
    mtext(side=1,at=c(0,1,2,3),text=expression(0,pi/3,2*pi/3,pi),line=1)
    mtext(side=1,text="Maximum change in direction at each step (radians)",line=1.5,outer=TRUE)
    mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
dev.off()







