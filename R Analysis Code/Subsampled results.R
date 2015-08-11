
#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Bat Project					                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: Who knows?!                                         	#
#                                                                  	 	#
# Edited by: E.Moorcroft                                           		#
# Edited on: 24Nov		                                            	#
#                                                                   	#
# Script title: Density calculation	         	                    	#
# Script purpose:Calculate the density form the simulation using		#
#                  - Tim's Function                                     #
#                                                                   	#
#########################################################################

# Clear all the saved data
rm(list=ls(all=TRUE)) 


#####################
# Libraries 		#
#####################
library("RColorBrewer")
library("plotrix")
library("fields")


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

cols<-brewer.pal(9,"YlOrRd")
ramp<-colorRampPalette(brewer.pal(11,"Spectral"))(500)

COLMATCHGRAPH<-c(COLset3[c(1:6)],COLset2[c(7)],COLset3[c(8)])


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
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
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
loadindata  <-read.csv(paste(name,",Test.csv",sep=""),header=FALSE)
x           <-loadindata[,-dim(loadindata)[2]]

Data        <-x[-c(1:4),-1]
header      <-x[c(1:4),-1]
callwidth   <-c("0",names(table(as.numeric(header[2,]))))
radii       <-names(table(as.numeric(Cameras[,6])))
camerawidth <-names(table(as.numeric(Cameras[,5])))
dimension   <-dim(Data)[2]


#################################
# Combinations of call and      #
# camera to be used in the      #
# subsequent analysis           #
#################################
ChoosenPts<-matrix(ncol=4,nrow=25)
ChoosenPts[1,]<-c(camerawidth[1],callwidth[2],"SW7", 8)  #"p344"
ChoosenPts[2,]<-c(camerawidth[1],callwidth[3],"SW8", 8)  #"p345"
ChoosenPts[3,]<-c(camerawidth[2],callwidth[2],"SW4", 8)  #"p341"
ChoosenPts[4,]<-c(camerawidth[2],callwidth[4],"SW5", 8)  #"p342"
ChoosenPts[5,]<-c(camerawidth[1],callwidth[4],"SW9", 8)  #"p346"
ChoosenPts[6,]<-c(camerawidth[1],callwidth[6],"SW6", 8)  #"p343"
ChoosenPts[7,]<-c(camerawidth[1],callwidth[7], "NW7" ,6) #"p243"
ChoosenPts[8,]<-c(camerawidth[1],callwidth[10],"NW6" ,6) #"p242"
ChoosenPts[9,]<-c(camerawidth[1],callwidth[11], "NW5" ,6)#"p241"
ChoosenPts[10,]<-c(camerawidth[1],callwidth[12],"REM",5) #"p141"
ChoosenPts[11,]<-c(camerawidth[3],callwidth[2], "SW3",8) #"p333"
ChoosenPts[12,]<-c(camerawidth[3],callwidth[4], "SW2",8) #"p332"
ChoosenPts[13,]<-c(camerawidth[3],callwidth[6], "SW1",8) #"p331"
ChoosenPts[14,]<-c(camerawidth[3],callwidth[7],"NW4",6)  #"p233"
ChoosenPts[15,]<-c(camerawidth[3],callwidth[10],"NW3",6) #"p232"
ChoosenPts[16,]<-c(camerawidth[3],callwidth[11],"NW2",6) #"p231"
ChoosenPts[17,]<-c(camerawidth[3],callwidth[12],"NW1",5) #"p131"
ChoosenPts[18,]<-c(camerawidth[5],callwidth[2],"SE4",8)  #"p323"
ChoosenPts[19,]<-c(camerawidth[5],callwidth[6],"SE3",8)  #"p322"
ChoosenPts[21,]<-c(camerawidth[6],callwidth[6],"SE2",7)  #"p321"
ChoosenPts[20,]<-c(camerawidth[5],callwidth[7],"NE3",6)  #"p223"
ChoosenPts[22,]<-c(camerawidth[6],callwidth[7],"NE2",3)  #"p222"
ChoosenPts[23,]<-c(camerawidth[6],callwidth[11],"NE1",1) #"p221"
ChoosenPts[24,]<-c(camerawidth[7],callwidth[2],"SE1",2)  #"p311"
ChoosenPts[25,]<-c(camerawidth[7],callwidth[11],"gas",4)



#################################
# Functions                     #
#################################
#### Calcaultes the %error  mean and std and plots it
add.boxplot<-function(profilewidth, Data, Speed, Time, Density, xlocation, colour){
    
    #Calucalte the estimated density
    tempest<-(1/profilewidth)*Data/(Speed*Time)
    
    # Calcaulates the percentage error
    Values<-100*(tempest-Density)/Density
    
    # mean/sd/sterr
    sdadj<-sd(Values,na.rm=T)
    meanadj<-mean(Values,na.rm=T)
    stardarderr<-sdadj/sqrt(length(Data))
    
    #Plots
    print(paste("x-value: ", xlocation, ", y-value: ",meanadj))
    boxplot(at=xlocation,Values,add=T,axes=FALSE,col=colour)
    
    return(Values)
}


#################################
# Matrix for data input         #
#################################
Matrix<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(Matrix)<-callwidth; rownames(Matrix)<-camerawidth
MatrixNum<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixNum)<-callwidth; rownames(MatrixNum)<-camerawidth
MatrixSD<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixSD)<-callwidth; rownames(MatrixSD)<-camerawidth
MatrixTest<-matrix(nrow=length(camerawidth),ncol=length(callwidth))
colnames(MatrixTest)<-callwidth; rownames(MatrixTest)<-camerawidth



#################################
# Update matrices with values	#
#################################


for(column in 1:dim(Data)[2]){

    print(paste("I:",column))
    
    # For each of the columns calculate the profile
	CamerarowI	<-which(header[1,column]==Cameras[,1])
	CameraWidthI<-Cameras[CamerarowI,5]*2	
	CameraRadiI	<-Cameras[CamerarowI,6]
	CallAngleI	<-header[2,column]*2
    profilewidth<-calcProfileWidth(CallAngleI, CameraWidthI, CameraRadiI )
		
	
    # Calculates the estimates
    tempdata<-Data[,column]
    tempest<-1/profilewidth[[1]]*tempdata/(Speed*Time)
		
	m<-mean(tempest,na.rm=T)
	sd1<-sd(tempest,na.rm=T)
	p<-100*(tempest-Density)/Density
	
    # in the correct call and camera width save the mean, sd and percentage
	if(CameraRadiI==10){
		rownum<-which(abs(as.numeric(camerawidth) - CameraWidthI/2)<0.1)
		colnum<-which(abs(as.numeric(callwidth) - CallAngleI/2)<0.1)
		#print(paste("HELLO", rownum, colnum))
		Matrix[rownum,colnum]<-m
		MatrixNum[rownum,colnum]<-mean(tempdata)
		MatrixSD[rownum,colnum]<-sd(p,na.rm=T)
		MatrixTest[rownum,colnum]<-wilcox.test(tempest,Density)$p.value
		#print(paste("P-value",MatrixTest[rownum,colnum]))
	}
	
}




#################################
# Plot the matrices             #
#################################
# plot of the average bais - as filled contour
setwd(DIR_IMG)
pdf("AverageBias.pdf")
filled.contour(z=Matrix -Density,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
			, color.palette=colorRampPalette(brewer.pal(11,"Spectral"),space="Lab")
			, xlab="Sensor width", ylab="Call width",main="Average bias")
dev.off()
# plot of the standard deviation - as filled contour
pdf("StandardDeviationContour.pdf")
filled.contour(z=MatrixSD,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
			, color.palette=colorRampPalette(brewer.pal(11,"Spectral"),space="Lab")
			, xlab="Sensor width", ylab="Call width",main="Standard Deviation")
dev.off()
# plot of the p-values - as filled contour
pdf("P-Values.pdf")
filled.contour(z=MatrixTest,x=as.numeric(camerawidth)*2,y=as.numeric(callwidth)*2
, color.palette=colorRampPalette(brewer.pal(11,"Spectral"),space="Lab")
, xlab="Sensor width", ylab="Call width",main="P-Value, H0: Bias = 0")
dev.off()


# plot of the stardard deviation  - as blocks
pdf("ResultStandardDeviation.pdf")

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

# plots the estimates - as coloured blocks
# Adds lines showing models
# Adds circles showing all collected data
# Adds stars showing all chosen points
pdf("Models.pdf")
image(z=Matrix,x=as.numeric(camerawidth),y=as.numeric(callwidth), col=ramp,ylim=c(0,pi))
# equations for lines
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
# All collected data
grid<-expand.grid(as.numeric(camerawidth),as.numeric(callwidth))
points(grid)
# All chosen  points
points(ChoosenPts,pch="*")
dev.off()






pdf("AverageModelBias.pdf")
#par(mar=c(6,4,4,2))
plot(type="n",0,0,xlab="Model Number",ylab="",xlim=c(1,26),ylim=c(-15,15),axes=FALSE)
abline(h=0,col="grey",lty=2)
list.of.models<-c(1:25)



save.data<-matrix(nrow=104,ncol=0)
iter.name<-paste("iter",1:100,sep="")
rownames(save.data)<-c("namemodel","signal_angle","camera_width","Radius",iter.name)

# model each column of the data (each inidvidual camera/call/radius combo)
for(Column in 1:dim(Data)[2]){
    
    # Values for camera/call and radius
    CamerarowI	<-which(header[1,Column]==Cameras[,1])
    CameraWidthI<-Cameras[CamerarowI,5]*2
    CameraRadiI	<-Cameras[CamerarowI,6]
    CallAngleI	<-header[2,Column]*2
    
    # select point from that is in the Chosen points matrix
    example.points.camera   <-which(abs(as.numeric(ChoosenPts[,1])-CameraWidthI/2)<0.1)
    example.points.call     <-which(abs(as.numeric(ChoosenPts[,2])-CallAngleI/2)<0.1)
    example.points.both     <-example.points.camera[which(example.points.camera %in% example.points.call)]
    example.point.location  <-which(list.of.models %in% example.points.both)
    
    if(length(example.point.location)==1 & CameraRadiI==10){
        if(length(list.of.models)==0){break();}
        list.of.models = list.of.models[-example.point.location]
        profilewidth<-calcProfileWidth(CallAngleI ,CameraWidthI, CameraRadiI)[[1]]
        values<-add.boxplot(profilewidth, Data=Data[,Column], Speed, Time, Density, xlocation=example.points.both, colour=COLMATCHGRAPH[as.numeric(ChoosenPts[example.points.both,4])])
        print(values)
        save.data<-save.data_valuesformat_function(data=values,call=CallAngleI,sensor=CameraWidthI,radius=CameraRadiI,name=ChoosenPts[example.points.both,3],save.data)
        } # else{print("not in table")} # end of if loop
    
} # End of column loop
axis(side=1,at=c(1:dim(ChoosenPts)[1]),labels=ChoosenPts[,3],las=2)
axis(side=2,at=c(-15,-10,-5,0,5,10,15),labels=c(-15,-10,-5,0,5,10,15),las=0)
mtext(text=expression(paste("Percentage error",sep="")),side=2,line=2)
box()

write.csv("AllModels_percenterror.csv",x=save.data)
dev.off()