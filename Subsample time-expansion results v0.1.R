#####################
# Libraries 		#
#####################
library("RColorBrewer")


#####################
# Directory			#
#####################
DIR_DATA<-"/Users/student/Documents/Bats/Simulations"
DIR_CODE<-"/Users/student/Documents/Bats/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")


#####################
# Colours 			#
#####################
COL=brewer.pal(9,"Set1")


setwd(DIR_DATA)
# Name of data 
Name<-"Temporary,NoBoundaries,StraightMove,Density=5,Speed=6.3,Iterations=1-501,StepLength=3.5,DetectorRadius=50,CallHalfwidth=3.14159,CameraHalfwidth=0.785398"

# Read in data
setwd(DIR_DATA)
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
Cameras	<-read.csv(paste(Name,",Cameras.csv",sep=""))
Settings<-read.csv(paste(Name,"Settings.csv",sep=""))

# Selects all non-calls
Captures<-Captures[which(Captures$Call==0),]
Captures<-Captures[order(Captures$Iteration.number,Captures$AnimalNumber,Captures$Time_step,Captures$Percentage.time.within.step),]
#################################
# Naming variables variables	#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
CallWidth		<-Settings[which(Settings[,1] %in% "Call_halfwidth"),2]*2
CameraWidth		<-Settings[which(Settings[,1] %in% "CameraWidth"),2]*2
CameraCallRadius<-Settings[which(Settings[,1] %in% "DetectorRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]
StepLength		<-Settings[which(Settings[,1] %in% "StepLength"),2]


Time.temp<-LengthMonitoring/10
intstartvec<-seq(0,0.99,by=0.01)
intendvec<-intstartvec+0.01

all.results<-list()
no<-1:floor(NoOfSteps/100)
x<-c()
for(i in 1:length(no)){x<-c(x,(100*no[i]-10):(100*no[i]))}

# -----------------------------------------	
# Start of loop
# -----------------------------------------	

#For each of the start and end location of the capture interval there is a 
# iteration. During this each of the NoOfIterations is calculated 
for(IT in 1:length(intstartvec)){
	# Creating vectors for results
	# These are created everytime so that they are wiped 
	# and cannot accidently get the same value as last iteration
	noofcaptures	 <-vector(length=NoOfIterations)
	noofcaptures.end <-vector(length=NoOfIterations)
	D.estimate.marcus.end<-vector(length=NoOfIterations)
	noofcaptures.limit <-vector(length=NoOfIterations)
	D.estimate.marcus.limit<-vector(length=NoOfIterations)
	noofcaptures.all <-vector(length=NoOfIterations)
	D.estimate.marcus.all<-vector(length=NoOfIterations)
	D.estimate		 <-vector(length=NoOfIterations)
	D.estimate.marcus<-vector(length=NoOfIterations)
	
	# Print the iteration number out
	print(paste("IT", IT))
	
	# Select the interval for the 
	intstart<-intstartvec[IT]
	intend<-intendvec[IT]

	# For each iteration
	for(i in 1:NoOfIterations){
		
		print("Limit")
		remove<-c()
		Captureslimit<-(Captures[which(Captures$Iteration.number==i),])
		Captureslimit<-(Captureslimit[which( Captureslimit$Time_step %in% x),])
		if(dim(Captureslimit)[1]>1){
		for(k in 2:dim(Captureslimit)[1]){
				if(Captureslimit$AnimalNumber[k-1] == Captureslimit$AnimalNumber[k] & # Same animal
					(Captureslimit$Time_step[k-1]==Captureslimit$Time_step[k] -1 ||  # One time step difference
					 Captureslimit$Time_step[k-1]==Captureslimit$Time_step[k]	# Same time step
					 ) ){remove<-c(remove,k)}	
		}
		if(length(remove)>0){Captureslimit<-Captureslimit[-remove,]}}
		noofcaptures.limit[i]<-dim(Captureslimit)[1]
		D.estimate.marcus.limit[i]<-(noofcaptures.limit[i]/length(no)*StepLength)*(pi/(BatSpeed*CameraCallRadius*(2+CameraWidth)))
		
		print("End")	
		Capturesatend<-(Captures[which(Captures$Iteration.number==i & Captures$Percentage.time.within.step==1),])
		if(dim(Capturesatend)[1]>1){
		for(k in 2:dim(Capturesatend)[1]){
				if(Capturesatend$AnimalNumber[k-1] == Capturesatend$AnimalNumber[k] & # Same animal
					(Capturesatend$Time_step[k-1]==Capturesatend$Time_step[k] -1 ||  # One time step difference
					 Capturesatend$Time_step[k-1]==Capturesatend$Time_step[k]	# Same time step
					 ) ){remove<-c(remove,k)}	
		}
		if(length(remove)>0){Capturesatend<-Capturesatend[-remove,]}}
		noofcaptures.end[i]<-dim(Capturesatend)[1]
		D.estimate.marcus.end[i]<-(noofcaptures.end[i]/NoOfSteps)*(pi/(BatSpeed*CameraCallRadius*(2+CameraWidth)))
		
		print("All")
	# For each iteration
		CapturesAll<-Captures[which(Captures$Iteration.number==i),]
		if(dim(CapturesAll)[1]>1){		
		for(k in 2:dim(CapturesAll)[1]){
				if(CapturesAll$AnimalNumber[k-1] == CapturesAll$AnimalNumber[k] & # Same animal
					(CapturesAll$Time_step[k-1]==CapturesAll$Time_step[k] -1 ||  # One time step difference
					 CapturesAll$Time_step[k-1]==CapturesAll$Time_step[k]	# Same time step
					 ) ){remove<-c(remove,k)}	
		}
		if(length(remove)>0){CapturesAll<-CapturesAll[-remove,]}}
		noofcaptures.all[i]<-dim(CapturesAll)[1]
		D.estimate.marcus.all[i]<-(noofcaptures.all[i]/LengthMonitoring)*(pi/(BatSpeed*CameraCallRadius*(2+CameraWidth)))

		print("Sub")
		#print(i)
		# -----------------------------------------	
		# Loads in the Captures for the each iteration
		# then counts them
		CapturesInI<-(Captures[which(Captures$Iteration.number==(i)),])
		vec<-c()
		# Remove all of the end of step counts becuase they are not needed
		if(any(CapturesInI$Percentage.time.within.step==1)){CapturesInI<-CapturesInI[-which(CapturesInI$Percentage.time.within.step==1),]}
			
			# If there are two or more captures then they are ordered  
			if(dim(CapturesInI)[1]>=2){
				Temp<-CapturesInI[order(CapturesInI$AnimalNumber,CapturesInI$Time_step,CapturesInI$Percentage.time.within.step),]
				# Label Start and End of each capture interval
				# They should be in pairs
				# Becuase they have been ordered then this step should work
				Tempwithse<-cbind(Temp,c("S","E"))
				names(Tempwithse)[dim(Tempwithse)[2]]<-"StartEnd"
				# Add a vector to the matrix called Step
				# this will be the number of the capture
				Tempwithse<-cbind(Tempwithse,vector(length = dim(Tempwithse)[1]))
				names(Tempwithse)[dim(Tempwithse)[2]]<-"Step"
				
				# Each capture event (start and end of the capture)
				# is given a unique number
				temp<-1
				for(j in 1:dim(Tempwithse)[1]){
					Tempwithse$Step[j] = temp
					if(Tempwithse$StartEnd[j] == "E"){temp <- temp+1}
				}
				
				# For each of the unique captures Check whether it would be in the "on interval"
				# Because: 
				# 	~ the start of the "in area movement" needs to be before the end point of the interval (or in the previous time step)
				#	~ the end of the "in area movement" needs to be after the start of the "on interval"
				
				for(k in 1:(temp-1)){
					endpoints<-which(Tempwithse$Step==k & Tempwithse$StartEnd=="E")
					startpoints<-which(Tempwithse$Step==k & Tempwithse$StartEnd=="S")
					#print(paste("END=",endpoints))
					#print(paste("START=",startpoints))
		
					if(Tempwithse$Percentage.time.within.step[endpoints]>= intstart & 
						(Tempwithse$Percentage.time.within.step[startpoints]<= intend | 
						Tempwithse$Time_step[startpoints]!=Tempwithse$Time_step[endpoints] )){ vec<-c(vec,k)}
	
				} # END of FOR 
				# Make these into the matrix
				CapturesInI<-Tempwithse[which(Tempwithse$Step %in% vec),]
			}# END OF IF	
		
		# Calculate the number of captures
		noofcaptures[i]<-length(vec)
		#noofcaptures[i]<-dim(CapturesInI)[1]
		#if(noofcaptures[i]>1){
		#	remove<-c()
		#	for(k in 2:dim(CapturesInI)[1]){
		#		if(CapturesInI$AnimalNumber[k-1] == CapturesInI$AnimalNumber[k] & # Same animal
		#			(CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k] -1 ||  # One time step difference
		#			 CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k]	# Same time step
		#			 ) ){remove<-c(remove,k)}	
		#	}
		#	if(length(remove)>0){noofcaptures[i]<-dim(CapturesInI[-remove,])[1]}
		#} # END OF IF(noofcaptures[i]>1)
		#print(paste("No.of captures 2:",noofcaptures[i]))


		# -----------------------------------------	
		# Tim's function
		#D.estimate[i]<-ImmobileDens(captures = noofcaptures[i]
		#						,time = Time.temp
		#						,cam.angle = CameraWidth
		#						,call.angle = CallWidth
		#						,r = CameraCallRadius
		#						,v = BatSpeed
								#,vd = CameraSpeed
		#						)$Density
		#points(x=i,y=mean(D.estimate[1:i]/10^-6),col="red")
		#print("M")
		# -----------------------------------------
		# Marcus' density estimation:
		# Density = Rate.Of.Photos *  (pi / speed.of.animal*radius*(2+camera width) )
		# Rate.Of.Photos = No.Photos/Time			
		#print(paste("i=",i, ", noofcaptures=",noofcaptures[i]))
		#print(paste("noofcaptures=",noofcaptures))
		D.estimate.marcus[i]<-(noofcaptures[i]/Time.temp)*(pi/(BatSpeed*CameraCallRadius*(2+CameraWidth)))
		#print(paste("noofcaptures[i]",noofcaptures[i]))

		#print(paste("D.estimate.marcus[i]",D.estimate.marcus[i]))
		#points(x=i,y=mean(D.estimate.marcus[1:i]/10^-6),col="red")		
	} ### END OF FOR I LOOP

	sub.result.list<-list()

	# -------------------------------------------
	# Saves the values to a list
	# 
	sub.result.list$Captures			<-noofcaptures
	sub.result.list$CapturesAtEnd		<-table(noofcaptures.end)
	sub.result.list$Marcus.Estimate.End	<-mean(D.estimate.marcus.end,na.rm=T)/10^-6
	sub.result.list$CapturesAtLimit		<-table(noofcaptures.limit)
	sub.result.list$Marcus.Estimate.Limit<-mean(D.estimate.marcus.limit,na.rm=T)/10^-6
	sub.result.list$CapturesAll			<-table(noofcaptures.all)
	sub.result.list$Marcus.Estimate.All	<-mean(D.estimate.marcus.all,na.rm=T)/10^-6
	sub.result.list$IntervalStart		<-intstart
	sub.result.list$IntervalEnd			<-intend
	sub.result.list$No.of.captures		<-table(noofcaptures)
	sub.result.list$Tims.Estimate		<-mean(D.estimate,na.rm=T)/10^-6
	sub.result.list$Tims.Estimate.sd	<-sd(D.estimate/10^-6,na.rm=T)
	sub.result.list$Marcus.Estimate		<-mean(D.estimate.marcus,na.rm=T)/10^-6
	sub.result.list$Marcus.Estimate.sd	<-sd(D.estimate.marcus/10^-6,na.rm=T)

	print(sub.result.list)
	# -----------------------------------------
		# Actual density
	sub.result.list$True.Density<-(NoofAnimals/Area)/10^-6

	all.results[[IT]]<-sub.result.list
}# END OF IT FOR LOOP


# -----------------------------------------	
# End of loop
# -----------------------------------------	


est<-c(1:100)
start<-c(1:100)
end<-c(1:100)
CapAtEnd<-c(1:100)

for(i in 1:100){est[i]<-all.results[[i]]$Marcus.Estimate}
for(i in 1:100){start[i]<-all.results[[i]]$IntervalStart}
for(i in 1:100){end[i]<-all.results[[i]]$IntervalEnd}
for(i in 1:100){CapAtEnd[i]<-all.results[[i]]$Marcus.Estimate.End}

par(mfrow=c(2,2))
plot(x=start,y=est)
hist(est)



	
	