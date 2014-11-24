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
Name<-"Temporary,NoBoundaries,StraightMove,Density=2.5,Speed=6.3,Iterations=1-501,StepLength=3.5,CallHalfwidth=3.14159,CameraHalfwidth=0.785398"

setwd(DIR_DATA)
Captures<-read.csv(paste(Name,",Captures.csv",sep=""))
Cameras	<-read.csv(paste(Name,",Cameras.csv",sep=""))
Settings<-read.csv(paste(Name,"Settings.csv",sep=""))

Captures<-Captures[which(Captures$Call==0),]
names(Captures)[which(names(Captures)%in% c("X..time"))]<- "Percentage.time.within.step"

#################################
# Setting variables				#
#################################
NoOfIterations	<-Settings[which(Settings[,1] %in% "NoOfIterations"),2]
NoOfSteps		<-Settings[which(Settings[,1] %in% "NoSteps"),2]
if(length(NoOfSteps)==0){NoOfSteps<-Settings[which(Settings[,1] %in% " NoSteps"),2]}
Time			<-NoOfSteps*0.35
CallWidth		<-Settings[which(Settings[,1] %in% "Call_halfwidth"),2]*2
CameraWidth		<-Settings[which(Settings[,1] %in% "CameraWidth"),2]*2
CameraCallRadius<-11#<-Settings[which(Settings[,1] %in% "CallRadius"),2]
CameraSpeed		<-Settings[which(Settings[,1] %in% "SpeedCamera"),2]
BatSpeed		<-Settings[which(Settings[,1] %in% "AnimalSpeed"),2]
NoofAnimals		<-Settings[which(Settings[,1] %in% "NoOfAnimals"),2]
Area			<-Settings[which(Settings[,1] %in% "Area"),2]
LengthMonitoring<-Settings[which(Settings[,1] %in% "LengthMonitoring"),2]


noofcaptures	 <-vector(length=NoOfIterations)
D.estimate		 <-vector(length=NoOfIterations)
D.estimate.marcus<-vector(length=NoOfIterations)

Time.temp<-LengthMonitoring/10

intstartvec<-seq(0,0.99,by=0.01)
intendvec<-seq(0.01,1,by=0.01)

all.results<-list()

for(IT in 1:100){

noofcaptures	 <-vector(length=NoOfIterations)
D.estimate		 <-vector(length=NoOfIterations)
D.estimate.marcus<-vector(length=NoOfIterations)

print(paste("IT", IT))


intstart<-intstartvec[IT]
intend<-intendvec[IT]


x<-1:501
#x<-x[-c(26,65,74,82,131,151,187,200,216,246,260,270,296,312,314,324,331,334,370,375,401,408,421,425,449,482)]
# For each iteration 
for(i in x){	
	
	print(i)
	# -----------------------------------------	
	# Loads in the Captures for the each iteration
	# then counts them
	CapturesInI<-(Captures[which(Captures$Iteration.number==(i)),])
		#print(CapturesInI)
		if(dim(CapturesInI)[1]>1){
			Temp<-CapturesInI[order(CapturesInI[,1],CapturesInI[,2],CapturesInI[,7]),]
			#print(Temp)
			if(any(Temp$Percentage.time.within.step==1)){Tempminus1<-Temp[-which(Temp$Percentage.time.within.step==1),]}else{Tempminus1<-Temp}
			print((Tempminus1))
			Tempwithse<-cbind(Tempminus1,c("S","E"))
			names(Tempwithse)[dim(Tempwithse)[2]]<-"StartEnd"
			#print(Tempwithse)
			Tempwithse<-cbind(Tempwithse,vector(length = dim(Tempwithse)[1]))
			names(Tempwithse)[dim(Tempwithse)[2]]<-"Step"
			#print(Tempwithse)
			
			vec<-c()
			
			temp<-1
			for(j in 1:dim(Tempwithse)[1]){
				Tempwithse$Step[j] = temp
				if(Tempwithse$StartEnd[j] == "E"){temp <- temp+1}
			}
			#print(Tempwithse)
	
			for(k in 1:(temp-1)){
				#print(k)
				#print((temp-1))
				endpoints<-which(Tempwithse$Step==k & Tempwithse$StartEnd=="E")
				startpoints<-which(Tempwithse$Step==k & Tempwithse$StartEnd=="S")
				#print(paste("END=",endpoints))
				#print(paste("START=",startpoints))
	
				if(Tempwithse$Percentage.time.within.step[endpoints]>= intstart & 
						(Tempwithse$Percentage.time.within.step[startpoints]<= intend | 
						Tempwithse[startpoints,2]!=Tempwithse[endpoints,2] )){ vec<-c(vec,k)}
	
			}

			Tempwithse<-Tempwithse[which(Tempwithse$Step %in% vec),]
			CapturesInI<-Tempwithse	
		}
		
	
	noofcaptures[i]<-dim(CapturesInI)[1]
	print(paste("No.of captures 1:",noofcaptures[i]))
	if(noofcaptures[i]>1){
		remove<-c()
		for(k in 2:dim(CapturesInI)[1]){
			if(CapturesInI$AnimalNumber[k-1] == CapturesInI$AnimalNumber[k] & # Same animal
				(CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k] -1 ||  # One time step difference
				 CapturesInI$Time_step[k-1]==CapturesInI$Time_step[k]	# Same time step
				 ) ){remove<-c(remove,k)}	
		}
		if(length(remove)>0){noofcaptures[i]<-dim(CapturesInI[-remove,])[1]}
		
	}
	print(paste("No.of captures 2:",noofcaptures[i]))


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
} ### END OF LOOP

sub.result.list<-list()

# -------------------------------------------
# Table of the number of captures
# Tim's estimate
# Marcus' estimate
sub.result.list$IntervalStart<-intstart
sub.result.list$IntervalEnd<-intend
sub.result.list$No.of.captures<-table(noofcaptures)
sub.result.list$Tims.Estimate<-mean(D.estimate,na.rm=T)/10^-6
sub.result.list$Tims.Estimate.sd<-sd(D.estimate/10^-6,na.rm=T)
sub.result.list$Marcus.Estimate<-mean(D.estimate.marcus,na.rm=T)/10^-6
sub.result.list$Marcus.Estimate.sd<-sd(D.estimate.marcus/10^-6,na.rm=T)

print(sub.result.list)
# -----------------------------------------
# Actual density
sub.result.list$True.Density<-(NoofAnimals/Area)/10^-6

all.results[[IT]]<-sub.result.list
}

x<-c(1:100)
for(i in 1:100){x[i]<-all.results[[i]]$Marcus.Estimate}

x1<-c(1:100)
for(i in 1:100){x1[i]<-all.results[[i]]$IntervalStart}

x2<-c(1:100)
for(i in 1:100){x2[i]<-all.results[[i]]$IntervalEnd}
	
	
y<-c(1:19)
for(i in 1:19){
	l<-length(all.results[[i]]$No.of.captures)
	temp<-0
	for(k in 1:7){
		temp=all.results[[i]]$No.of.captures[k]*as.numeric(labels(all.results[[i]]$No.of.captures)$noofcaptures)[k]
	}
	y[i]<-temp/475}
	
	
	
	