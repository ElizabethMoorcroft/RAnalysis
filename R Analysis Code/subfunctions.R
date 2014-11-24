

cameraprofile<-function(SensorNo, SensorInfo, Callwidth){
	CameraWidth<-SensorInfo[which(SensorNo==SensorInfo$ID),]$HalfWidthAngle*2
	CameraRadi<-SensorInfo[which(SensorNo==SensorInfo$ID),]$Radius
	
	profile<-calcProfileWidth(Callwidth, CameraWidth, CameraRadi)
	
	return(profile)
}

Density<-function(profile,Speed,Time,Captures){
	
	Estimate<-(1/profile)*Captures/(Speed*Time)
	
	return(Estimate)
}
