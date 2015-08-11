#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Bat Project					                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: Who knows?!                                         	#
#                                                                  	 	#
# Edited by: E.Moorcroft                                                #
# Edited on: 25Nov14	                                            	#
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


#####################
# Directory			#
#####################
DIR_DATA<-"/Users/student/Documents/Bats/Simulations"
DIR_SAVE<-"/Users/student/Documents/Bats/Simulations"
#DIR_IMG<-"/Users/student/Documents/Bats/lucasMoorcroftManuscript/imgs"
DIR_IMG<-"/Users/student/Documents/Bats/Temp2"
DIR_CODE<-"/Users/student/Documents/Bats/RAnalysis/R analysis code"


#####################
# Source code		#
#####################
setwd(DIR_CODE)
source("Tim's original bat code.R")
source("calculateProfileWidth.R")
source("LoadIn.R")
setwd(DIR_CODE)
source("Output.R")


#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)

COLmodel<-matrix(ncol=2,nrow=4)
COLmodel[1,]<-c("NW1", COLset3[3])
COLmodel[2,]<-c("SW1", COLset3[11])
COLmodel[3,]<-c("NE1", COLset3[1])
COLmodel[4,]<-c("SE3", COLset3[11])


#################################
# Model selection				#
#################################

r<-10 # radius
t<-14400 # time
n<-3937

# Camera angle; Call angle; Radius; Time; Name
ModelSelection<-matrix(nrow=4,ncol=6)
ModelSelection[1,]<-c(1.0472,3.14159 , r, t, n, "NW1") # p141
ModelSelection[2,]<-c(1.0472,1.428   , r, t, n, "SW1") # p343
ModelSelection[3,]<-c(1.74533,3.14159, r, t, n, "NE1") # p221
ModelSelection[4,]<-c(1.74533,1.428  , r, t, n, "SE3") # p322



#################################
# Functions						#
#################################

model.name<-function(row.model.select){
    if(row.model.select[1]==1.0472 & row.model.select[2]==3.14159){name<-"NW1"}
    else if(row.model.select[1]==1.0472 & row.model.select[2]==1.428){name<-"SW1"}
    else if(row.model.select[1]==1.74533 & row.model.select[2]==3.14159){name<-"NE1"}
    else if(row.model.select[1]==1.74533 & row.model.select[2]==1.428){name<-"SE3"}
    else{name<-"NA"}
    return(name)
}

all.models<-c("NW1","SW1","NE1","SE3")


#################################
# Plot - perching               #
#################################

##Create data set
time.perch<-c(0,0.5,0.75)
names.base<-lapply(time.perch,function(x){paste("Run23Oct201317July", x, ",Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,", sep="")})
x.pos<-matrix(ncol=4,rep(1:4,each=4))

setwd(DIR_DATA)
biases<-calculate.all.biases(Names=paste(names.base,"timenosubs14400.csv",sep=""),
                            Names.Cameras=paste(names.base,"Sensors.csv",sep=""),
                            ModelSelection,
                            Speed=0.46,
                            Area=(7500^2),
                            StepLength=900,
                            x.pos)

save.data<-matrix(nrow=105,ncol=0)
iter.name<-paste("iter",1:100,sep="")
rownames(save.data)<-c("namemodel","signal_angle","camera_width","Radius","Prop_time_still",iter.name)
setwd(DIR_IMG)
save.data_function(biases,vector.character=time.perch,save.data=save.data,save.name="Prop_time_still_percentageerror.csv")

setwd(DIR_IMG)
pdf("ResultsPerch.pdf")
    par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
    plot.biases.boxplot(all.data=biases,colour.list=COLmodel,model.name=ModelSelection[,5],ylim=6,ylab=c(-4,0,4),type="boxplot")
    mtext(side=1,at=c(1,2,3,4),text=expression(0,0.25,0.5,0.75),line=1)
    mtext(side=1,text="Proportion of time spent stationary",line=1.5,outer=TRUE)
    mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
dev.off()


#################################
# Plot - Tort                   #
#################################

##Create data set
tort<-c(0,1.0472,2.0944,3.14159)
names.base<-lapply(tort,function(x){paste("Run23Oct201317July0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=", x,",",sep="")})
x.pos<-matrix(ncol=4,rep(1:4,each=4))

setwd(DIR_DATA)
biases<-calculate.all.biases(Names=paste(names.base,"timenosubs14400.csv",sep=""),
                            Names.Cameras=paste(names.base,"Sensors.csv",sep=""),
                            ModelSelection,
                            Speed=0.46,
                            Area=(7500^2),
                            StepLength=900,
                            x.pos)

print(biases)

setwd(DIR_IMG)
pdf("ResultsTort.pdf")
    par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
    plot.biases.boxplot(all.data=biases,colour.list=COLmodel,model.name=ModelSelection[,5],ylim=6,ylab=c(-4,0,4),type="boxplot")
    mtext(side=1,at=c(1,2,3,4),text=expression(0,pi/3,2*pi/3,pi),line=1)
    mtext(side=1,text="Maximum change in direction at each step (radians)",line=1.5,outer=TRUE)
    mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
dev.off()


save.data<-matrix(nrow=105,ncol=0)
iter.name<-paste("iter",1:100,sep="")
rownames(save.data)<-c("namemodel","signal_angle","camera_width","Radius","max_angle_change",iter.name)
setwd(DIR_IMG)
save.data_function(biases,vector.character=tort,save.data=save.data,save.name="max_angle_change_percentageerror.csv")





#################################
# Model selection				#
#################################

r<-10 # radius
t<-c(12,18,24)#t<-c(14,  27, 40 , 53 , 66,  79 , 92, 105 ,118 ,131 ,144),30,36,42,48,54,60,66,72
n<-c(38)
#t<-c(140 ,270,400,530,660,790,920,1050,1180,1310,1440)#c(1310,2619,3928,5237,6546,7855,9164,10473,11782,13091,14400) # time
camera<-c(1.0472,1.74533)
call<-c(3.14159,1.428)
grid.of.values<-as.matrix(expand.grid(camera, call,r,t,n))
vector.model.names<-apply(grid.of.values,1,model.name)
ModelSelection<-cbind(grid.of.values,vector.model.names)
x.pos<-matrix(ncol=2,rep(rep(1:11,each=4),2))

walks<-c(0,3.14159)
names.base<-lapply(walks,function(x){paste("REFSCommentscompareanimalcall0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=", x,",",sep="")})
setwd(DIR_DATA)
biases<-calculate.all.biases(Names=paste(names.base,"AnalysisForCommentsVSHORTTimeSubDen.csv",sep=""),
                            Names.Cameras=paste(names.base,"Sensors.csv",sep=""),
                            ModelSelection,
                            Speed=0.46,
                            Area=(7500^2),
                            StepLength=900,
                            x.pos)

setwd(DIR_IMG)
pdf("TimeSubs.pdf")
    par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
    plot.biases.boxplot(all.data=biases,colour.list=COLmodel,model.name=all.models,ylim=500,ylab=c(-400,0,400),type="notplot")
    mtext(side=1,at=c(1:11),text=round((t*900)/(60*60*24),2),line=1)
    mtext(side=1,text="Number of days",line=1.5,outer=TRUE)
    mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
    legend(x=2.5,y=-50,col=c("red","black"),lty=c(1,1),cex=0.8,legend=c(0,expression(pi)),title="max. change direction")

dev.off()







r<-10 # radius
t<-c(12)#t<-c(14,  27, 40 , 53 , 66,  79 , 92, 105 ,118 ,131 ,144),30,36,42,48,54,60,66,72
n<-c(38,77, 116,155,194)
#t<-c(140 ,270,400,530,660,790,920,1050,1180,1310,1440)#c(1310,2619,3928,5237,6546,7855,9164,10473,11782,13091,14400) # time
camera<-c(1.0472,1.74533)
call<-c(3.14159,1.428)
grid.of.values<-as.matrix(expand.grid(camera, call,r,t,n))
vector.model.names<-apply(grid.of.values,1,model.name)
ModelSelection<-cbind(grid.of.values,vector.model.names)
x.pos<-matrix(ncol=2,rep(rep(1:11,each=4),2))

walks<-c(0,3.14159)
names.base<-lapply(walks,function(x){paste("REFSCommentscompareanimalcall0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=", x,",",sep="")})
setwd(DIR_DATA)
biases<-calculate.all.biases(Names=paste(names.base,"AnalysisForCommentsVSHORTTimeSubDen.csv",sep=""),
Names.Cameras=paste(names.base,"Sensors.csv",sep=""),
ModelSelection,
Speed=0.46,
Area=(7500^2),
StepLength=900,
x.pos)

setwd(DIR_IMG)
pdf("DensitySubs.pdf")
par(mfrow=c(4,1),oma=c(3,3,0,0), mar=c(2,4,2,0.5))
plot.biases.boxplot(all.data=biases,colour.list=COLmodel,model.name=all.models,ylim=500,ylab=c(-400,0,400),type="notplot")
mtext(side=1,at=c(1:11),text=round((n*(1000^2))/(7500^2),2),line=1)
mtext(side=1,text="Density",line=1.5,outer=TRUE)
mtext(side=2,text="Percentage error between estimated and true density",line=1.5,outer=TRUE)
legend(x=4.5,y=-50,col=c("red","black"),lty=c(1,1),cex=0.8,legend=c(0,expression(pi)),title="max. change direction")
dev.off()

setwd(DIR_DATA)
x<-read.csv("REFSCommentscompareanimalcall0,Density=70,Speed=0.46,Iterations=1-101,StepLength=900,CorrWalkMaxAngleChange=0,AnalysisForCommentsVSHORTTimeSubDen.csv",header=FALSE)
sort(as.numeric(names(table(as.character(x[3,-1])))))
sort(as.numeric(names(table(as.character(x[4,-1])))))