
Name<-"Bats,Boundary=,Boundaries,Density=0.2,Speed=10,Iterations=1-2,StepLength=3.5,DetectorRadius=11,CallHalfwidth=3.14159,CameraHalfwidth=3.14159,CorrWalkMaxAngleChange=0,ProbChangeMoveState=0"

setwd("/Users/student/Documents/Bats/Simulations")
HR<-read.csv(paste(Name,",HomeRange.csv",sep=""))

par(mfrow=c(2,2))

for(i in 1:2){
	hr<-HR[which(HR$Iternation.number ==i),]

	plot(type="n",0,0,xlim=c(0,7500),ylim=c(0,7500))

	points(x=hr$XLocation, y=hr$YLocation)
	for(i in 1:dim(hr)[1]){
		points(x=c(hr$XLocation[i]-hr$Size[i],hr$XLocation[i]+hr$Size[i]), y=c(hr$YLocation[i],hr$YLocation[i]),type="l",col=as.numeric(hr$Sex)[i]+1)
		points(y=c(hr$YLocation[i]-hr$Size[i],hr$YLocation[i]+hr$Size[i]), x=c(hr$XLocation[i],hr$XLocation[i]),type="l",col=as.numeric(hr$Sex)[i]+1)
	}
}