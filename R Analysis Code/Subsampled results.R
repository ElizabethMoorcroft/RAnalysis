
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
#DIR_IMG<-"/Users/student/Documents/Bats/lucasMoorcroftManuscript/imgs"
DIR_IMG<-"/Users/student/Documents/Bats/Temp"
DIR_CODE<-"/Users/student/Documents/Bats/R analysis code"


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

COLset3=brewer.pal(12,"Set3")

#################################
# Load in Settings				#
#################################
setwd(DIR_DATA)
Settings<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,Settings.csv")
Cameras<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,Sensors.csv")

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
loadindata<-read.csv("Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0Save.csv",header=FALSE)

x<-loadindata[,-dim(loadindata)[2]]

Data<-x[-c(1:3),-1]
header<-x[c(1:3),-1]
callwidth<-names(table(as.numeric(header[2,])))
radii<-names(table(as.numeric(Cameras[,6])))
camerawidth<-names(table(as.numeric(Cameras[,5])))
dimension<-dim(Data)[2]
Matrix<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(Matrix)<-callwidth; rownames(Matrix)<-camerawidth
MatrixNum<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixNum)<-callwidth; rownames(MatrixNum)<-camerawidth
MatrixSD<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixSD)<-callwidth; rownames(MatrixSD)<-camerawidth
MatrixTest<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixTest)<-callwidth; rownames(MatrixTest)<-camerawidth

#for(i in 1:dim(Data)[2]){
for(i in 1:84){

	
	print(paste("I:",i))
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
		tempper<-
		tempdata<-c(tempdata,Data[j,i])
	}

	m<-mean(tempest,na.rm=T)
	sd1<-sd(tempest,na.rm=T)
	p<-100*(tempest-Density)/Density
	#print(summary(tempest))
	if(CameraRadiI==100){			
		rownum<-which(abs(as.numeric(camerawidth) - CameraWidthI/2)<0.1)
		colnum<-which(abs(as.numeric(callwidth) - CallAngleI/2)<0.1)
		#print(paste("HELLO", rownum, colnum))
		Matrix[rownum,colnum]<-m
		MatrixNum[rownum,colnum]<-mean(tempdata)
		MatrixSD[rownum,colnum]<-sd(p,na.rm=T)
		MatrixTest[rownum,colnum]<-wilcox.test(tempest[-c(1,length(tempest))],Density)$p.value
		#print(paste("P-value",MatrixTest[rownum,colnum]))
	}
	
}


cols<-brewer.pal(9,"YlOrRd")
ramp<-colorRampPalette(brewer.pal(11,"Spectral"))(500)


setwd(DIR_SAVE)
pdf("AverageBias.pdf")
filled.contour(z=Matrix -Density,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
			, color.palette=colorRampPalette(brewer.pal(11,"Spectral"),space="Lab")
			, xlab="Sensor width", ylab="Call width",main="Average bias")
dev.off()
pdf("StandardDeviationContour.pdf")
filled.contour(z=MatrixSD,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
			, color.palette=colorRampPalette(brewer.pal(11,"Spectral"),space="Lab")
			, xlab="Sensor width", ylab="Call width",main="Standard Deviation")
dev.off()

setwd(DIR_IMG)
pdf("ResultStandardDeviation.pdf")
library(fields)
par(mar=c(5,5,5,7))
image(z=MatrixSD,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
			, col=(brewer.pal(11,"Reds"))
			, xlab="Sensor width", ylab="Call width"
			, axes=FALSE
			,main="")
mtext(side=2,at=c(0,pi,2*pi),text=expression(0, pi, 2*pi))
mtext(side=1,at=c(0,pi,2*pi),text=expression(0, pi, 2*pi))
image.plot(MatrixSD, col=(brewer.pal(11,"Reds")), legend.only=TRUE)
box()
dev.off()

pdf("P-Values.pdf")
filled.contour(z=MatrixTest,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
			, color.palette=colorRampPalette(brewer.pal(11,"Spectral"),space="Lab")
			, xlab="Sensor width", ylab="Call width",main="P-Value, H0: Bias = 0")
dev.off()


pdf("Models.pdf")
image(z=Matrix,x=as.numeric(camerawidth),y=as.numeric(callwidth), col=ramp,ylim=c(0,pi))

points(type="l", x=c(0,pi),y=c(pi,pi))
points(type="l", x=c(0,pi),y=c(pi/2,pi/2))
points(type="l", x=c(pi/2,pi/2),y=c(0,pi))
points(type="l", x=c(pi,pi),y=c(0,pi))
points(type="l", x=c(0,0),y=c(0,pi))
points(type="l", x=c(pi/4,pi/4),y=c(0,pi))
points(type="l", x=c(0,pi),y=c(pi,0))
points(type="l", x=c(0,pi/4),y=c(pi,pi/2))
points(type="l", x=c(pi/4,pi/2),y=c(pi,pi/2))
points(type="l", x=c(pi/2,pi),y=c(pi,0))
points(type="l", x=c(pi/2,pi),y=c(pi,pi/2))
points(type="l", x=c(0,pi/4),y=c(pi/2,0))
points(type="l", x=c(0,pi/4),y=c(0,pi/2))
points(type="l", x=c(0,pi/2),y=c(0,pi/2))
points(type="l", x=c(pi/4,pi/2),y=c(0,pi/2))

grid<-expand.grid(as.numeric(camerawidth),as.numeric(callwidth))
points(grid)

COLMATCHGRAPH<-c(COLset3[c(1:6)],COLset2[c(7)],COLset3[c(8)])
ChoosenPts<-matrix(ncol=4,nrow=25)
ChoosenPts[1,]<-c(camerawidth[1],callwidth[2],"SW7", 8) #"p344"
ChoosenPts[2,]<-c(camerawidth[1],callwidth[3],"SW8", 8)#"p345"
ChoosenPts[3,]<-c(camerawidth[2],callwidth[2],"SW4", 8)#"p341"
ChoosenPts[4,]<-c(camerawidth[2],callwidth[4],"SW5", 8)#"p342"
ChoosenPts[5,]<-c(camerawidth[1],callwidth[4],"SW9", 8)#"p346"
ChoosenPts[6,]<-c(camerawidth[1],callwidth[6],"SW6", 8)#"p343"

ChoosenPts[7,]<-c(camerawidth[1],callwidth[7], "NW7" ,6)#"p243"
ChoosenPts[8,]<-c(camerawidth[1],callwidth[10],"NW6" ,6)#"p242"
ChoosenPts[9,]<-c(camerawidth[1],callwidth[11], "NW5" ,6)#"p241"

ChoosenPts[10,]<-c(camerawidth[1],callwidth[12],"REM",5) #"p141"

ChoosenPts[11,]<-c(camerawidth[3],callwidth[2], "SW3",8) #"p333"
ChoosenPts[12,]<-c(camerawidth[3],callwidth[4], "SW2",8)#"p332"
ChoosenPts[13,]<-c(camerawidth[3],callwidth[6], "SW1",8)#"p331"

ChoosenPts[14,]<-c(camerawidth[3],callwidth[7],"NW4",6) #"p233"
ChoosenPts[15,]<-c(camerawidth[3],callwidth[10],"NW3",6) #"p232"
ChoosenPts[16,]<-c(camerawidth[3],callwidth[11],"NW2",6) #"p231"

ChoosenPts[17,]<-c(camerawidth[3],callwidth[12],"NW1",5) #"p131"

ChoosenPts[18,]<-c(camerawidth[5],callwidth[2],"SE4",8)#"p323"
ChoosenPts[19,]<-c(camerawidth[5],callwidth[6],"SE3",8)#"p322"
ChoosenPts[21,]<-c(camerawidth[6],callwidth[6],"SE2",7)#"p321"

ChoosenPts[20,]<-c(camerawidth[5],callwidth[7],"NE3",6)#"p223"
ChoosenPts[22,]<-c(camerawidth[6],callwidth[7],"NE2",3)#"p222"
ChoosenPts[23,]<-c(camerawidth[6],callwidth[11],"NE1",1)#"p221"

ChoosenPts[24,]<-c(camerawidth[7],callwidth[2],"SE1",2)#"p311"

ChoosenPts[25,]<-c(camerawidth[7],callwidth[11],"gas",4)
points(ChoosenPts,pch="*")

dev.off()

pdf("AverageModelBias.pdf")
#par(mar=c(6,4,4,2))
plot(type="n",0,0,xlab="Model Number",ylab="",xlim=c(1,26),ylim=c(-10,10),axes=FALSE)
abline(h=0,col="grey",lty=2)
modelcount=1
for(model in 1:dim(ChoosenPts)[1]){
	print(paste("Model: ", model,"/",length(ChoosenPts),sep=""))
	for(i in 1:dim(Data)[2]){
		
		#print(paste("I:",i))
		CamerarowI	<-which(header[1,i]==Cameras[,1])	
		CameraWidthI<-Cameras[CamerarowI,5]*2	
		CameraRadiI	<-Cameras[CamerarowI,6]
		CallAngleI	<-header[2,i]*2
		
		examplept<-which(CameraWidthI/2 == as.numeric(ChoosenPts[model,1]) & CallAngleI/2 == as.numeric(ChoosenPts[model,2]))
		if(length(examplept)>0){
		
			profilewidth<-calcProfileWidth(CallAngleI, CameraWidthI, CameraRadiI )
		
			tempest<-c()
			tempdata<-c()
			for(j in 1:dim(Data)[1]){
				if(Data[j,i]==0){tempest<-c(tempest,0)}
				else{tempest<-c(tempest,(1/profilewidth[[1]])*Data[j,i]/(Speed*Time))}
				tempdata<-c(tempdata,Data[j,i])
			}

			if(CameraRadiI==10){
				rownum<-which(abs(as.numeric(camerawidth) - CameraWidthI/2)<0.1)
				colnum<-which(abs(as.numeric(callwidth) - CallAngleI/2)<0.1)
                print(Data[,i])
				Values<-tempest*(1000^2)- Density*(1000^2)
				Values<-100*Values/(Density*(1000^2))
				sdadj<-sd(Values,na.rm=T)
				meanadj<-mean(Values,na.rm=T) 
				stardarderr<-sdadj/sqrt(NoOfIterations)
				#plotCI(x=modelcount,y=meanadj,uiw=(1.96*stardarderr),add=T)
				boxplot(at=modelcount,Values,add=T,axes=FALSE,col=COLMATCHGRAPH[as.numeric(ChoosenPts[model,4])])
				modelcount=modelcount+1
				print(paste("InexamplePt",meanadj,stardarderr ))
			}
		}
	}
}
axis(side=1,at=c(1:dim(ChoosenPts)[1]),labels=ChoosenPts[,3],las=2)
axis(side=2,at=c(-10,-5,0,5,10),labels=c(10,-5,0,5,10),las=0)
mtext(text=expression(paste("Percentage error",sep="")),side=2,line=2)
box()
dev.off()