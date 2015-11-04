
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
DIR_SAVE<-"/Users/student/Documents/Bats/Temp2"
#DIR_IMG<-"/Users/student/Documents/Bats/lucasMoorcroftManuscript/imgs"
DIR_IMG<-"/Users/student/Documents/Temp"
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



###  - Models Selected for the analysis
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



#### Calcaultes the %error  mean and std and plots it

calculate.error<-function(profilewidth, Data, Speed, Time, NoOfIterations, Density){
    
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
    print(paste("y-value: ",meanadj))
    #plotCI(x=xlocation,y=meanadj,uiw=(1.96*stardarderr),add=T,col=COLspec[error.counter])
    return(list(meanadj,stardarderr,Values))
}

plots.error<-function(meanadj, stardarderr, xlocation, error.counter){
    #Plots
    print(paste("x-value: ", xlocation, ", y-value: ",meanadj))
    plotCI(x=xlocation,y=meanadj,uiw=(1.96*stardarderr),add=T,col=COLspec[error.counter])
}

create.table.of.data<-function(noofiterations){
  results<-matrix(ncol=0,nrow=noofiterations+11)
  n_iter<-paste("iter",1:100,sep="")
  row.names(results)<-c("ModelName","callwidth", "camerawidth", "cameraradius", "speed","time",
                        "input_callwidth", "input_camerawidth", "input_cameraradius", "input_speed","input_time",
                        n_iter)
  return(results)
}

calculate.and.plots<-function(name,
                              callwidth, camerawidth, cameraradius, data, speed, time,
                              act_callwidth, act_camerawidth, act_cameraradius,act_speed, act_time,
                              noofiterations, density, xlocation, colour,results){
    profilewidth<-calcProfileWidth(callwidth ,camerawidth, cameraradius)[[1]]
    values<-calculate.error(profilewidth, Data=data, Speed=speed, Time=time, NoOfIterations=noofiterations, Density=density)
    plots.error(values[[1]],values[[2]],xlocation=xlocation, error.counter=colour)
    results<-cbind(results,
                c(name,
                  act_callwidth, act_camerawidth, act_cameraradius,act_speed, act_time,
                  callwidth, camerawidth, cameraradius, speed,time,
                  values[[3]]))
    return(list(values[[1]],values[[2]],results))
}

result.matrix<-matrix(nrow=length(percentage.error)*4, ncol=dim(ChoosenPts)[1])
x<-c()
for(error in 1:length(percentage.error)){x<-c(x,paste(c("callwidth","camerawidth","cameraradius","speed"),percentage.error[error],sep=""))}
rownames(result.matrix)<-c(x)
colnames(result.matrix)<-ChoosenPts[,3]

result.matrix.sd<-result.matrix


### - Points with CI for each model and percentage error
setwd(DIR_IMG)
pdf("AverageModelBias - 4 models Jan 7.pdf")
results<-create.table.of.data(NoOfIterations)
# Sets the outline of the plot
#par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))

list.of.models<-c(1:25)

plot(type="n",0,0,xlab="",ylab="",xlim=c(1,25),ylim=c(-15,15),axes=FALSE,main="")
model.number=0
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
    name<-ChoosenPts[example.points.both,3]    
    
    
    
    if(length(example.point.location)==1 & CameraRadiI==10){
        if(length(list.of.models)==0){break();}
        list.of.models = list.of.models[-example.point.location]
        model.number=example.points.both
        
        
        results.column<-example.points.both
        
        # For Call
        counter<-1
        for(error in 1:length(percentage.error)){
            # Calculate profile width
            c.angle<-CallAngleI +CallAngleI *percentage.error[error]/100; if(c.angle>(2*pi)){c.angle=2*pi} else if(c.angle<0){c.angle=0}
            r1<-calculate.and.plots(name,
                                    callwidth=c.angle, camerawidth=CameraWidthI, cameraradius=CameraRadiI, data=Data[,Column], speed=Speed, time=Time,
                                    act_callwidth=CallAngleI, act_camerawidth=CameraWidthI, act_cameraradius=CameraRadiI, act_speed=Speed, act_time=Time,
                                    noofiterations=NoOfIterations, density=Density, xlocation=model.number, colour=error,results)
            results<-r1[[3]]
            
            #cam.angle<-CameraWidthI +CameraWidthI *percentage.error[error]/100;if(cam.angle>(2*pi)){cam.angle=2*pi} else if(cam.angle<0){cam.angle=0}
            #r2<-calculate.and.plots(name,
            #                        callwidth=CallAngleI, camerawidth=cam.angle, cameraradius=CameraRadiI, data=Data[,Column], speed=Speed, time=Time,
            #                        act_callwidth=CallAngleI, act_camerawidth=CameraWidthI, act_cameraradius=CameraRadiI,act_speed=Speed, act_time=Time,
            #                        noofiterations=NoOfIterations, density=Density, xlocation=2, colour=error,results)
            #results<-r2[[3]]
            
            #c.radii<-CameraRadiI +CameraRadiI*percentage.error[error]/100
            #r3<-calculate.and.plots(name,
            #                        callwidth=CallAngleI, camerawidth=CameraWidthI, cameraradius=c.radii, data=Data[,Column], speed=Speed, time=Time, 
            #                        act_callwidth=CallAngleI, act_camerawidth=CameraWidthI, act_cameraradius=CameraRadiI,act_speed=Speed, act_time=Time,
            #                        noofiterations=NoOfIterations, density=Density, xlocation=3, colour=error,results)
            #results<-r3[[3]]
            #
            #new.speed<-Speed +Speed*percentage.error[error]/100
            #r4<-calculate.and.plots(name,
            #                        callwidth=CallAngleI, camerawidth=CameraWidthI, cameraradius=CameraRadiI, data=Data[,Column], speed=new.speed, time=Time,
            #                        act_callwidth=CallAngleI, act_camerawidth=CameraWidthI, act_cameraradius=CameraRadiI,act_speed=Speed, act_time=Time,
            #                        noofiterations=NoOfIterations, density=Density, xlocation=4, colour=error,results)
            #results<-r4[[3]]
        
            #print("results.column");print(results.column);print(counter:(counter+3));print(c(r1,r2,r3,r4))
            
            result.matrix[(counter),results.column]<-r1[[1]];
            #result.matrix[(counter+1),results.column]<-r2[[1]];
            #result.matrix[(counter+2),results.column]<-r3[[1]];
            #result.matrix[(counter+3),results.column]<-r4[[1]]
            
            result.matrix.sd[(counter),results.column]<-r1[[2]];
            #result.matrix.sd[(counter+1),results.column]<-r2[[2]];
            #result.matrix.sd[(counter+2),results.column]<-r3[[2]];
            #result.matrix.sd[(counter+3),results.column]<-r4[[2]]
            counter<-counter+4

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
    



#mtext(side=1,at=c(1,2,3,3.9),text=c("Signal Width","Sensor Width","Sensor Radius","Animal Speed"),line=1,cex=0.8)
#mtext(side=1,text="Number of days",line=1.5,outer=TRUE)
text(x=c(1:25), y=par("usr")[3] - 0.2, labels = ChoosenPts[,3], srt = 45, pos = 1, xpd = TRUE)
mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
legend(x=1.25,y=12,title="Percentage error",legend=percentage.error,col=COLspec ,lty=rep(1,5),bg="white")
write.csv(results,"Sensitivity_percentageerror.csv")
dev.off()


