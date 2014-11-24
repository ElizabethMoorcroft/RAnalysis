
 move<-read.csv("Run23Oct2013testcorrwalkprint,Perch0,Density=0.02,Speed=0.46,Iterations=10-11,StepLength=900,CorrWalkMaxAngleChange=3.14159,Movement.csv") 
 
plot(type="n",0,0,xlim=c(0,7500),ylim=c(0,7500))
for(i in 1:149){
	if(move[i+1,8]==0){
	points(x=c(move[i,3],move[i+1,3]),y=c(move[i,4],move[i+1,4]),type="l")
	}
	}