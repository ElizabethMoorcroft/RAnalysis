# For each model output list

CalBias<-function(ModelSelection,Cameras, x, Speed ){
	Output<-vector(mode="list",length=4)
	
	Data<-x[-c(1:4),-1]
	header<-x[c(1:4),-1]
	
	timerow		<-which(x[,1]=="Time")
	sensorrow	<-which(x[,1]=="Sensor")
	callrow		<-which(x[,1]=="Call")
	densityrow	<-which(x[,1]=="Density")
	
	for(i in 1:4){
		print(paste("I:",i))
		CamerarowI	<-which(ModelSelection[i,1]==Cameras[,1])	
		CameraWidthI<-Cameras[CamerarowI,5]*2	
		CameraRadiI	<-Cameras[CamerarowI,6]
		CallAngleI	<-as.numeric(ModelSelection[i,2])*2
		
		print(paste("CamerarowI",CamerarowI,"CameraWidthI",CameraWidthI))
		
		#Calculation of the profile width
		profilewidth<-calcProfileWidth(CallAngleI, CameraWidthI, CameraRadiI )
		
		#print(profilewidth)
		selectedcols<-which(header[sensorrow,]==ModelSelection[i,1] & header[callrow,]== ModelSelection[i,2])
		print(paste("ModelSelection[i,1]",ModelSelection[i,1],"ModelSelection[i,2]",ModelSelection[i,2]))
		#print(paste("which(header[sensorrow,]==ModelSelection[i,1])",which(header[sensorrow,]==ModelSelection[i,1])))
		#print(paste("which(header[callrow,]== ModelSelection[i,2])",which(header[callrow,] == ModelSelection[i,2])))
		print(selectedcols)
		selectedcols<-selectedcols[1]
		TempOutput<-vector(mode="list",length=length(selectedcols))
		
		for(time in 1:length(selectedcols)){
			Column<-selectedcols[time]
			TimeVal<-header[timerow,Column]*StepLength
			DensityVal<-header[densityrow,Column]/Area

			Numbvector<-Data[,Column]
			Estvector<-(1/profilewidth[[1]])*Numbvector/(Speed*TimeVal)
			Biasvector<-(Estvector-DensityVal)*(1000^2)
			Percentvector<-100*Biasvector/(DensityVal*(1000^2))
			print(paste("Speed",Speed,"Time",TimeVal,"Number", mean(Numbvector),"Estimate",mean(Estvector)))
			print(paste("MinBias",min(Biasvector),"MaxBias",max(Biasvector)))
			TempOutput[[time]]<-list(Time=TimeVal,
									Numbers=Numbvector, 
									Bias = Biasvector, 
									Estimates = Estvector, 
									Percent = Percentvector)
		}
		Output[[i]] <-TempOutput
	}
	return(Output)
}

