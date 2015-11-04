
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
library(fields)


#####################
# Directory			#
#####################
DIR_DATA<-"/Users/student/Documents/Bats/Simulations"
DIR_SAVE<-"/Users/student/Documents/Bats/Simulations"
#DIR_IMG<-"/Users/student/Documents/Bats/lucasMoorcroftManuscript/imgs"
DIR_IMG<-"/Users/student/Documents/Bats/Temp2"
DIR_CODE<-"/Users/student/Documents/Bats/RAnalysis/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")
source("calculateProfileWidth.R")


#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")
COLsets=rep(c(COLset1,COLset2,COLset3),4)

##reds for SD
COLred = brewer.pal(9,"Reds")



##Colours match Tims graph
COLMATCHGRAPH<- c( COLset3[1], COLset3[12], COLset3[6], COLset3[4], COLset3[5], COLset3[3], COLset3[8], COLset3[11])

ramp<-colorRamp(COLred)

#################################
# Percentage ERRORS             #
#################################
percentage.error<-c(-10,-1,0,1,10)
#Spectral
COLspec = brewer.pal(5,"Spectral")

#################################
# Load in Settings				#
#################################
setwd(DIR_DATA)
name<-"Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
Settings<-read.csv(paste(name,",Settings.csv",sep=""))
Cameras<-read.csv(paste(name,",Sensors.csv",sep=""))

#################################
# Setting variables				#
#################################
NoOfIterations	<-100 #Settings[which(Settings[,1] %in% "NoOfIterations"),2]
Seed			<-Settings[which(Settings[,1] %in% "Seed"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
StepLength		<-Settings[which(Settings[,1] %in% "StepLength"),2]
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
Speed			<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]
Time			<-NoOfSteps*StepLength
Density			<-NoofAnimals/(Area)

#################################
# Load Data						#
#################################
loadindata<-read.csv(paste(name,",Test.csv",sep=""),header=FALSE)

x<-loadindata[,-dim(loadindata)[2]]

#### Values
Data        <-x[-c(1:4),-1]
header      <-x[c(1:4),-1]
callwidth   <-names(table(as.numeric(header[2,])))
radii       <-names(table(as.numeric(Cameras[,6])))
camerawidth <-names(table(as.numeric(Cameras[,5])))
dimension   <-dim(Data)[2]


Matrix				<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(Matrix)	<-callwidth; rownames(Matrix)<-camerawidth
MatrixNum			<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixNum)	<-callwidth; rownames(MatrixNum)<-camerawidth
MatrixSD			<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixSD)	<-callwidth; rownames(MatrixSD)<-camerawidth
MatrixTest			<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixTest)<-callwidth; rownames(MatrixTest)<-camerawidth

for(i in 12:dim(Data)[2]){
#for(i in 1:84){

	
	#print(paste("I:",i))
	CamerarowI	<-which(header[1,i]==Cameras[,1])	
	CameraWidthI<-Cameras[CamerarowI,5]*2	
	CameraRadiI	<-Cameras[CamerarowI,6]
	CallAngleI	<-header[2,i]*2
	
	#print(paste("Callangle",CallAngleI,"Camerawidth",CameraWidthI))

	
	profilewidth<-calcProfileWidth(CallAngleI, CameraWidthI, CameraRadiI )
		
	
	#print(paste("profilewidth:", profilewidth[[1]]))
	tempest<-c()
	tempdata<-c()
	tempper<-c()
	for(j in 1:dim(Data)[1]){		
		if(Data[j,i]==0){tempest<-c(tempest,0)}
		else{tempest<-c(tempest,(1/profilewidth[[1]])*Data[j,i]/(Speed*Time))}
		tempdata<-c(tempdata,Data[j,i])
	}

	m<-mean(tempest,na.rm=T)
	sd1<-sd(tempest,na.rm=T)
	p<-100*(tempest-Density)/Density
	#print(summary(tempest))
	if(CameraRadiI==10){			
		rownum<-which(abs(as.numeric(camerawidth) - CameraWidthI/2)<0.1)
		colnum<-which(abs(as.numeric(callwidth) - CallAngleI/2)<0.1)
		#print(paste("HELLO", rownum, colnum))
		Matrix[rownum,colnum]<-m
		MatrixNum[rownum,colnum]<-mean(tempdata)
		MatrixSD[rownum,colnum]<-sd1#sd(p,na.rm=T)
		MatrixTest[rownum,colnum]<-wilcox.test(tempest[-c(1,length(tempest))],Density)$p.value

	}
	
}



###  - Models Selected for the analysis
ChoosenPts<-matrix(ncol=4,nrow=4)
ChoosenPts[1,]<-c(camerawidth[3],callwidth[11],"NW1",5) #"p131"
ChoosenPts[2,]<-c(camerawidth[3],callwidth[5], "SW1",8)#"p331"
ChoosenPts[3,]<-c(camerawidth[6],callwidth[9],"NE1",1)#"p221"
ChoosenPts[4,]<-c(camerawidth[5],callwidth[5],"SE3",8)#"p322"



#### Calcaultes the %error  mean and std and plots it
calculate.plot<-function(profilewidth, Data, Speed, Time, NoOfIterations, Density, xlocation ,error.counter){
    
    #Calucalte the estimated density
    tempest<-(1/profilewidth)*Data/(Speed*Time)
    #print("tempest");print(tempest)
    
    # Calcaulates the percentage error
    Values<-100*(tempest-Density)/Density
    #print("Values");print(Values)
    
    # mean/sd/sterr
    sdadj<-sd(Values,na.rm=T)
    meanadj<-mean(Values,na.rm=T)
    stardarderr<-sdadj/sqrt(NoOfIterations)
            
    #Plots
    print(paste("x-value: ", xlocation, ", y-value: ",meanadj))
    plotCI(x=xlocation,y=meanadj,uiw=(1.96*stardarderr),add=T,col=COLspec[error.counter])
}
        


### - Points with CI for each model and percentage error
setwd(DIR_IMG)
pdf("AverageModelBias - 4 models.pdf")

# Sets the outline of the plot
par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))

list.of.models<-c(1:4)

# model each column of the data (each inidvidual camera/call/radius combo)
for(Column in 1:dim(Data)[2]){
		      
    # Values for camera/call and radius
    CamerarowI	<-which(header[1,Column]==Cameras[,1])
    CameraWidthI<-Cameras[CamerarowI,5]*2
    CameraRadiI	<-Cameras[CamerarowI,6]
    CallAngleI	<-header[2,Column]*2
    
    # select point from matrix
    example.points.camera   <-which(as.numeric(ChoosenPts[,1])==CameraWidthI/2)
    example.points.call     <-which(as.numeric(ChoosenPts[,2])==CallAngleI/2)
    example.points.both     <-example.points.camera[which(example.points.camera %in% example.points.call)]
    example.point.location  <-which(list.of.models %in% example.points.both)
    
    
    if(length(example.point.location)==1 & CameraRadiI==10){
        if(length(list.of.models)==0){break();}
        list.of.models = list.of.models[-example.point.location]
        
        plot(type="n",0,0,xlab="",ylab="",xlim=c(1,4),ylim=c(-15,15),axes=FALSE,main="")
        
        # For Call
        error.counter<-1
        for(error in 1:length(percentage.error)){
            # Calculate profile width
            c.angle<-CallAngleI +CallAngleI *percentage.error[error]/100
            if(c.angle>(2*pi)){c.angle=2*pi} else if(c.angle<0){c.angle=0}
            profilewidth<-calcProfileWidth(c.angle ,CameraWidthI, CameraRadiI)[[1]]
            calculate.plot(profilewidth, Data=Data[,Column], Speed, Time, NoOfIterations, Density, xlocation=1, error.counter)
            error.counter=error.counter+1
        }# error loop
        
        # For Camera
        error.counter<-1
        for(error in 1:length(percentage.error)){
            # Calculate profile width
            c.angle<-CameraWidthI +CameraWidthI *percentage.error[error]/100
            if(c.angle>(2*pi)){c.angle=2*pi} else if(c.angle<0){c.angle=0}
            profilewidth<-calcProfileWidth(CallAngleI ,c.angle, CameraRadiI)[[1]]
            calculate.plot(profilewidth, Data=Data[,Column], Speed, Time, NoOfIterations, Density, xlocation=2, error.counter)
            error.counter=error.counter+1
        }# error loop
        
        # For Radius
        error.counter<-1
        for(error in 1:length(percentage.error)){
            # Calculate profile width
            c.radii<-CameraRadiI +CameraRadiI*percentage.error[error]/100
            profilewidth<-calcProfileWidth(CallAngleI ,CameraWidthI , c.radii)[[1]]
            calculate.plot(profilewidth, Data=Data[,Column], Speed, Time, NoOfIterations, Density, xlocation=3, error.counter)
            error.counter=error.counter+1
        }# error loop
        
        # For Speed
        error.counter<-1
        for(error in 1:length(percentage.error)){
            # Calculate profile width
            new.speed<-Speed +Speed*percentage.error[error]/100
            profilewidth<-calcProfileWidth(CallAngleI ,CameraWidthI , CameraRadiI)[[1]]
            calculate.plot(profilewidth, Data=Data[,Column], new.speed, Time, NoOfIterations, Density, xlocation=4, error.counter)
            error.counter=error.counter+1
        }# error loop
        
        abline(h=0,col="grey",lty=2)
        axis(1,labels=FALSE,at=c(1:4))
        axis(2,labels=FALSE,at=c(-10,0,10) )
        # labels y axis and the title
        mtext(side=2,at=c(-10,0,10),text=c(-10,0,10),line=1)
        mtext(text=ChoosenPts[example.points.both,3],side=3,line=0)
        box()
    } # else{print("not in table")} # end of if loop
        
} # End of column loop
    



mtext(side=1,at=c(1,2,3,3.9),text=c("Signal Width","Sensor Width","Sensor Radius","Animal Speed"),line=1,cex=0.8)
mtext(side=1,text="Number of days",line=1.5,outer=TRUE)
mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
legend(x=1.25,y=12,title="Percentage error",legend=percentage.error,col=COLspec ,lty=rep(1,5),bg="white")
dev.off()