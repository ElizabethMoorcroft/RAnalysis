#plot(xlim=c(1,5),ylim=c(-100,100),0,type="n")
#for(i in 1:3){
#    for(j in 1:5){
#        boxplot(add=T,x=((analysis[[i]][[j]]$grem$density-2*10^-8)/(2*10^-8))*100,col=COLset3[i],at=j-i/3,width=0.3)
#    }
#}
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
DIR_IMG<-"/Users/student/Documents/Bats/Temp2"
DIR_CODE<-"/Users/student/Documents/Bats/RAnalysis/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")
source("calculateProfileWidth.R")
source("ModelSelect.R")
source("subfunctions.R")

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
# Load in Settings				#
#################################
setwd(DIR_DATA)
Settings<-read.csv("Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,Settings.csv")
Cameras<-read.csv("Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,Sensors.csv")

#################################
# Setting variables				#
#################################
NoOfIterations	<-100#100 #Settings[which(Settings[,1] %in% "NoOfIterations"),2]
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



loadindata<-read.csv("Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,TestCounts6Aug.csv",header=FALSE)


x<-loadindata[,-dim(loadindata)[2]]

Data<-x[-c(1:3),-1]
header<-x[c(1:3),-1]

capsrow<-which(x[,1]=="Captures")
sensorrow<-which(x[,1]=="Sensor")
callrow<-which(x[,1]=="Call")
     


estimatesmatrix<-matrix(nrow=dim(Data)[1]+5,ncol=dim(Data)[2])
iter.name<-paste("iter",1:dim(Data)[1],sep="")
rownames(estimatesmatrix)<-c("namemodel","signal_angle","camera_width","Radius","No_of_captures",iter.name)

#for(i in 1:dim(Data)[2]){
for(i in 1:836){
	print(paste(i,"/",dim(Data)[2]))
	profile<-cameraprofile(SensorNo=header[sensorrow,i], SensorInfo=Cameras, Callwidth=header[callrow,i]*2)[[1]]
	model<-pickmodel(sensor=Cameras[which(header[sensorrow,i]==Cameras$ID),]$HalfWidthAngle*2,call=header[callrow,i]*2)
	estimatesmatrix[1,i]<-model
	print("model select")
	if((header[callrow,i] -3.14159 <0.01 & Cameras[which(header[sensorrow,i]==Cameras$ID),]$HalfWidthAngle- 1.04720<0.01)|| # call pi, cam pi/3  - 
		(header[callrow,i] - 1.428<0.01 & Cameras[which(header[sensorrow,i]==Cameras$ID),]$HalfWidthAngle - 1.04720<0.01)|| # call pi/2, cam pi/3
		(header[callrow,i]- 3.14159<0.01 & Cameras[which(header[sensorrow,i]==Cameras$ID),]$HalfWidthAngle - 1.74533<0.01)||  # call pi, cam 5pi/9??
		(header[callrow,i] - 1.428 <0.01& Cameras[which(header[sensorrow,i]==Cameras$ID),]$HalfWidthAngle - 1.74533<0.01) # call pi/2, cam 5pi/9??
	){estimatesmatrix[2,i]<-"Y"}else{estimatesmatrix[2,i]<-"N"}
  estimatesmatrix[2,i]<-header[callrow,i]*2
  estimatesmatrix[3,i]<-Cameras[which(header[sensorrow,i]==Cameras$ID),]$HalfWidthAngle*2 
  estimatesmatrix[4,i]<-Cameras[which(header[sensorrow,i]==Cameras$ID),]$Radius
  estimatesmatrix[5,i]<-header[capsrow,i]
	print("time")
	Time<-Data[,i]
	print(mean(Time/(60*60)))
	Captures<-header[capsrow,i]
	estimatesmatrix[6:(dim(Data)[1]+5),i]<-(((1/profile)*Captures/(Speed*Time))*(1000^2)-70)/70*100
}

write.csv(file="Allmodels_fixedcaps_variabletime_percenterror.csv",estimatesmatrix)

testcases<-estimatesmatrix[,which(estimatesmatrix[2,]=="Y")]
NW1<-testcases[,which(testcases[1,]=="NW1" & as.numeric(testcases[3,])>0)]
SW1<-testcases[,which(testcases[1,]=="SW1" & as.numeric(testcases[3,])>0)]
NE1<-testcases[,which(testcases[1,]=="NE1" & as.numeric(testcases[3,])>0)]
SE3<-testcases[,which(testcases[1,]=="SE3" & as.numeric(testcases[3,])>0)]


PlusMinusPerError<-70

maxval<-(dim(Data)[1]+3)
yval<-10

CV<-function(data){round(sd(data)/mean(data),2)}

setwd(DIR_IMG)
pdf("ResultsNoCaptures.pdf")
par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
plot(0,0,type="n",xlim=c(1,yval),ylim=c(0-PlusMinusPerError,0+PlusMinusPerError),
	ylab="",xlab="", axes=FALSE
	,main="")
	box()
abline(h=0,lty=2,col="grey")
#mtext(side=1,at=c(2,4,6,8,10),text=expression(20,40,60,80,100),line=1)
mtext(side=2,at=c(-PlusMinusPerError,0,PlusMinusPerError),text=c(-PlusMinusPerError,0,PlusMinusPerError),line=1)
mtext(text="NW1",side=3,line=0)
for(i in 1:dim(NW1)[2]){
	model<-"NW1"
	COL<-COLmodel[which(COLmodel==model),2]
	data<-as.numeric(NW1[4:maxval,i])
	dataerr<-data - Density*1000^2
  
	cv<-CV(data)
	boxplot(dataerr,add=T, at=as.numeric(NW1[3,i])/10, axes=FALSE,col=COL)
	text(x=as.numeric(NW1[3,i])/10,y=min(dataerr)-10, label=paste(cv ),cex=1)
	w.test<-wilcox.test(data,mu=Density*1000^2)$p.value
	sig.value<-0.05/dim(SE3)[2]; if(w.test<sig.value){sig<-"yes"}else{sig<-"no"}
	print(paste("Number of caps:",as.numeric(NW1[3,i])/10 ,"CV",sd(data)/mean(data), " p-value: ", w.test, " significant ", sig))
	
}

plot(0,0,type="n",xlim=c(1,yval),ylim=c(0-PlusMinusPerError,0+PlusMinusPerError),
	ylab="",xlab="", axes=FALSE
	,main="")
box()
abline(h=0,lty=2,col="grey")
#mtext(side=1,at=c(2,4,6,8,10),text=expression(20,40,60,80,100),line=1)
mtext(side=2,at=c(-PlusMinusPerError,0,PlusMinusPerError),text=c(-PlusMinusPerError,0,PlusMinusPerError),line=1)
mtext(text="SW1",side=3,line=0)
for(i in 1:dim(SW1)[2]){
	model<-"SW1"
	COL<-COLmodel[which(COLmodel==model),2]
	data<-as.numeric(SW1[4:maxval,i])
	dataerr<-data - Density*1000^2
	cv<-CV(data)
	boxplot(dataerr,add=T, at=as.numeric(SW1[3,i])/10, axes=FALSE,col=COL)
	text(x=as.numeric(SW1[3,i])/10,y=min(dataerr)-10, label=paste(cv ),cex=1)
	w.test<-wilcox.test(data,mu=Density*1000^2)$p.value
	sig.value<-0.05/dim(SE3)[2]; if(w.test<sig.value){sig<-"yes"}else{sig<-"no"}

	print(paste("Number of caps:",as.numeric(NW1[3,i])/10 ,"CV",sd(data)/mean(data), " p-value: ", w.test, " significant ", sig))
	
}
plot(0,0,type="n",xlim=c(1,yval),ylim=c(0-PlusMinusPerError,0+PlusMinusPerError),
	ylab="",xlab="", axes=FALSE
	,main="")
box()
abline(h=0,lty=2,col="grey")
#mtext(side=1,at=c(2,4,6,8,10),text=expression(20,40,60,80,100),line=1)
mtext(side=2,at=c(-PlusMinusPerError,0,PlusMinusPerError),text=c(-PlusMinusPerError,0,PlusMinusPerError),line=1)
mtext(text="SE3",side=3,line=0)
for(i in 1:dim(SE3)[2]){
	model<-"SE3"
	COL<-COLmodel[which(COLmodel==model),2]
	data<-as.numeric(SE3[4:maxval,i])
	dataerr<-data - Density*1000^2
	cv<-CV(data)
	boxplot(dataerr,add=T, at=as.numeric(SE3[3,i])/10, axes=FALSE,col=COL)
	text(x=as.numeric(SE3[3,i])/10,y=min(dataerr)-10, label=paste(cv ),cex=1)
	w.test<-wilcox.test(data,mu=Density*1000^2)$p.value
    sig.value<-0.05/dim(SE3)[2]; if(w.test<sig.value){sig<-"yes"}else{sig<-"no"}
	print(paste("Number of caps:",as.numeric(NW1[3,i])/10 ,"CV",sd(data)/mean(data), " p-value: ", w.test, " significant ", sig))
	
}
plot(0,0,type="n",xlim=c(1,yval),ylim=c(0-PlusMinusPerError,0+PlusMinusPerError),
	ylab="",xlab="", axes=FALSE
	,main="")
box()
abline(h=0,lty=2,col="grey")
mtext(side=1,at=c(2,4,6,8,10),text=expression(20,40,60,80,100),line=1)
mtext(side=2,at=c(-PlusMinusPerError,0,PlusMinusPerError),text=c(-PlusMinusPerError,0,PlusMinusPerError),line=1)
mtext(text="NE1",side=3,line=0)
for(i in 1:dim(NE1)[2]){
	model<-"NE1"
	COL<-COLmodel[which(COLmodel==model),2]
	data<-as.numeric(NE1[4:maxval,i])
	dataerr<-data - Density*1000^2
	cv<-CV(data)
	boxplot(dataerr,add=T, at=as.numeric(NE1[3,i])/10, axes=FALSE,col=COL)
	text(x=as.numeric(NE1[3,i])/10,y=min(dataerr)-10, label=paste( cv ),cex=1)
	w.test<-wilcox.test(data,mu=Density*1000^2)$p.value
	sig.value<-0.05/dim(SE3)[2]; if(w.test<sig.value){sig<-"yes"}else{sig<-"no"}
	print(paste("Number of caps:",as.numeric(NW1[3,i])/10 ,"CV",sd(data)/mean(data), " p-value: ", w.test, " significant ", sig))
	
}

mtext(side=1,text="Number of captures",line=1.5,outer=TRUE)
mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
dev.off()