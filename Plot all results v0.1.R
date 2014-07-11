l<-length(EndOfSimResults)
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)

maxsd<-max(unlist(lapply(EndOfSimResults,'[[',which(names(EndOfSimResults[[1]]=="Marcus.Estimate.sd")))))

plot(0,0,type="n",
	xlim=c(0,500),	
	ylim=c(0,maxsd),
	xlab="Radius (meters)",
	ylab="SD of estimates"
)

sensorwidth<-unlist(lapply(EndOfSimResults,'[[',which(names(EndOfSimResults[[1]]=="SensorWidth")))))
sw<-as.numeric(names(table(sensorwidth)))
lsw<-length(sw)
	
for(i in 1:lsw){
	sensorsi<-which[sensorwidth==sw[i]]
	lsensorsi<-length(sensori)
	for(j in 2:lsensori){
		valuem1<-sensorsi[j-1]
		value<-sensorsi[j]
		if(EndOfSimResults[[valuem1]]$SensorWidth==EndOfSimResults[[value]]$SensorWidth){
			points(	x=c(EndOfSimResults[[valuem1]]$Radius,EndOfSimResults[[value]]$Radius),
					y=c(EndOfSimResults[[valuem1]]$Marcus.Estimate.sd,EndOfSimResults[[value]]$Marcus.Estimate.sd),
					col=COLsets[i]
				)
		}
	
	
}

plot(0,0,type="n",
	xlim=c(0,2*pi),	
	ylim=c(0,maxsd),
	xlab="Detection angle (radians)",
	ylab="SD of estimates"
)
	
for(i in 2:l){
	
	points(	x=c(EndOfSimResults[[i-1]]$SensorWidth,EndOfSimResults[[i]]$SensorWidth),
			y=c(EndOfSimResults[[i-1]]$Marcus.Estimate.sd,EndOfSimResults[[i]]$Marcus.Estimate.sd),
			col="blue"
			)
	
}