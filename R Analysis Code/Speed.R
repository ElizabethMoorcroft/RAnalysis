PlotAddpoint<-function(x, Cameras,ModelSelect, NoOfSteps, Speed){
	
	if(ModelSelect ==1){
		ModelSelection<-matrix(nrow=4,ncol=3)
		ModelSelection[1,]<-c(0,3.14159, "p141") # REM
		ModelSelection[2,]<-c(0,1.428 , "p343") # 
		ModelSelection[3,]<-c(1,3.14159, "p221") # 
		ModelSelection[4,]<-c(1,1.428 , "p322") # 
	} else if(ModelSelect == 0){
		ModelSelection<-matrix(nrow=4,ncol=3)
		ModelSelection[1,]<-c(2,3.14159, "p141") # REM
		ModelSelection[2,]<-c(2,1.428 , "p343") # 
		ModelSelection[3,]<-c(4,3.14159, "p221") # 
		ModelSelection[4,]<-c(4,1.428 , "p322") # 
	}

	Output<-CalBias(ModelSelection, Cameras,x)

	TimeOfInterest<-NoOfSteps

	for(ModelNumber in 1:4){
		model<-Output[[ModelNumber]]
		for(number in 1:length(model)){
			
			if(model[[number]]$Time==TimeOfInterest*(900)){
				
				meanbias<-mean(model[[number]]$Bias,na.rm=T)
				sdbias<-sd(model[[number]]$Bias,na.rm=T)	
				sebias<-sdbias/10
				print(paste("Inside plot",number, "ModelNumber",ModelNumber, "meanbias",meanbias))
				plotCI(x=(Speed*(60*60*24))/1000,meanbias,uiw=sebias*1.96,col=ModelNumber,add=T)
			
			}	
		}
	}
}




pdf("ResultsSpeed.pdf")
plot(0,0,ylim=c(-7,7),xlim=c(0,40),type="n",
	axes=TRUE,ylab="",xlab="Speed (km/days)")
#axis(side=1,at=(x),labels=(x),las=2)
#axis(side=2,at=c(-1,-0.5,0,0.5,1),labels=c(-1,-0.5,0,0.5,1),las=0)
mtext(text=expression(paste("Bias (Animals/",km^2,")",sep="")),side=2,line=2)
legend(x=2,y=5,col=1:4,pch=rep(1,4), 
		legend=c(paste("Model",ModelSelection[1,3]),
				paste("Model",ModelSelection[2,3]),
				paste("Model",ModelSelection[3,3]),
				paste("Model",ModelSelection[4,3])
				)
	)


Name<-"Run23Oct2013Perch0,Density=70,Speed=0.15,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
setwd(DIR_DATA)
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
Speed	 <-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
loadindata<-read.csv(paste(Name,",time.csv",sep=""),header=FALSE)
x<-loadindata[,-dim(loadindata)[2]]
PlotAddpoint(x, Cameras,ModelSelect=1, NoOfSteps, Speed)

Name<-"Run23Oct2013Perch0,Density=70,Speed=0.31,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
setwd(DIR_DATA)
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
Speed	 <-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
loadindata<-read.csv(paste(Name,",time.csv",sep=""),header=FALSE)
x<-loadindata[,-dim(loadindata)[2]]
PlotAddpoint(x, Cameras,ModelSelect=1, NoOfSteps, Speed)

Name<-"Run23Oct2013Perch0,Density=70,Speed=0.00012,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
setwd(DIR_DATA)
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
Speed	 <-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
loadindata<-read.csv(paste(Name,",time.csv",sep=""),header=FALSE)
x<-loadindata[,-dim(loadindata)[2]]
PlotAddpoint(x, Cameras,ModelSelect=0, NoOfSteps, Speed)

Name<-"Run23Oct2013Perch0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0"
setwd(DIR_DATA)
Settings<-read.csv(paste(Name,",Settings.csv",sep=""))
Cameras<-read.csv(paste(Name,",Sensors.csv",sep=""))
NoOfSteps<-Settings[which(Settings[,1] %in% "NoSteps"),2]
Speed	 <-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
loadindata<-read.csv(paste(Name,",time.csv",sep=""),header=FALSE)
x<-loadindata[,-dim(loadindata)[2]]
PlotAddpoint(x, Cameras,ModelSelect=0, NoOfSteps, Speed)

abline(h=0,lty=2,col="grey")
box()

dev.off()





