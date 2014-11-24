allfiles<-list.files()
alltest<-allfiles[grep("Test",allfiles)]
steplength<-alltest[grep("StepLength=1.5",alltest)]
Caps<-steplength[grep(",Captures.csv",steplength)]
unlist(strsplit(Caps,",Captures.csv"))