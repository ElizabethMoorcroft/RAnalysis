library(Rcpp)

sourceCpp("/Users/student/Documents/subsample.cpp")

#captures<-c("/Users/student/Documents/Bats/Simulations/Run23Oct2013,Density=70,Speed=0.00012,Iterations=1-51,StepLength=300,CorrWalkMaxAngleChange=3.14159,Captures.csv",
#"/Users/student/Documents/Bats/Simulations/Run23Oct2013,Density=70,Speed=0.00012,Iterations=51-101,StepLength=300,CorrWalkMaxAngleChange=3.14159,Captures.csv")


captures<-c("/Users/student/Documents/Bats/Simulations/Run23Oct2013Perch0.5,Density=70,Speed=0.46,Iterations=1-51,StepLength=300,CorrWalkMaxAngleChange=0,Captures.csv","/Users/student/Documents/Bats/Simulations/Run23Oct2013Perch0.5,Density=70,Speed=0.46,Iterations=51-101,StepLength=300,CorrWalkMaxAngleChange=0,Captures.csv")

captures1<-c("/Users/student/Documents/Bats/Simulations/Run23Oct2013Perch0.5,Density=70,Speed=0.46,Iterations=1-51,StepLength=300,CorrWalkMaxAngleChange=0,Captures.csv")
settings<-"/Users/student/Documents/Bats/Simulations/Run23Oct2013,Density=70,Speed=0.00012,Iterations=1-51,StepLength=300,CorrWalkMaxAngleChange=3.14159,Settings.csv"
sensors<-"/Users/student/Documents/Bats/Simulations/Run23Oct2013,Density=70,Speed=0.00012,Iterations=1-51,StepLength=300,CorrWalkMaxAngleChange=3.14159,Sensors.csv"

subsample(captures,sensors,settings)
mergefiles(captures1)