library("RColorBrewer")

setwd("/Users/student/Documents/Bats")
Move<-read.csv("Move3.csv")

COL=brewer.pal(9,"Set1")
plot(
	0
	,0
	,xlim=c(0,100)
	,ylim=c(0,100)	
	,type="n")

for(i in 2:dim(Move)[1]){
	if( Move$Re.enterWorld[i]== 0 &
		Move$AnimalNumber[i] == Move$AnimalNumber[i-1]
		& Move$AnimalNumber[i]==0
		){

		points(
			 x=Move$Xlocation[c(i-1,i)]
			,y=Move$Ylocation[c(i-1,i)]
			,col=COL[Move$AnimalNumber[i]+1]
			,type="l"
			)
			
	}
}