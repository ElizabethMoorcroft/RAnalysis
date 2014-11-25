# For each model output list
select.columns<-function(model.values, header,Cameras){ #model.values<-ModelSelection[i,]
    

	#row numbers for time, sensor, call, and density
	timerow		<-which(header[,1]=="Time")
	sensorrow	<-which(header[,1]=="Sensor")
	callrow		<-which(header[,1]=="Call")
	densityrow	<-which(header[,1]=="Density")
    
    # Calculates the
    CamerarowI	<-Cameras[which(model.values[1]==Cameras[,5] & model.values[3]==Cameras[,6]),1]
    
    selectedcols<-which(header[sensorrow,]== CamerarowI &
                        header[callrow,]  == model.values[2] &
                        header[timerow,]  == model.values[4])
    
    #print("selectedcols");print(selectedcols[1])
    
    #returnsvalues
    return(selectedcols[1]-1)
}


#### Calcaultes the %error  mean and std and plots it
calculate.bias<-function(profilewidth, Data, Speed, Time, Density){
    
    print("Calculating bias")
    print(paste("Speed: ", Speed,", Time: ",Time,", Density: ",Density, ", Profile:", profilewidth,sep=""))
    
    #Calucalte the estimated density
    tempest<-(1/profilewidth)*Data/(Speed*Time)
    #print("tempest");print(tempest)
    
    # Calcaulates the percentage error
    Values<-100*(tempest-Density)/Density
    
    #print("Values");print(Values)
    return(Values)

}

CalBias<-function(ModelSelection, Cameras, x, Speed, Density, StepLength ){
	    
    #List to be outputted
	Output<-vector(mode="list",length=4)
    
    #Splits data into header and data
    Data<-x[-c(1:4),-1]
	header<-x[c(1:4),]
	
    # Loops through all of the modelSelections
	for(i in 1:dim(ModelSelection)[1]){
        
        #print(paste("CalBias loop #:",i))
        #Selects the coloumn from the Chosen points
        model.values<-ModelSelection[i,]
		
        #Calculates profile width
        profilewidth<-calcProfileWidth(theta_a =as.numeric( model.values[2])*2, theta_s = as.numeric(model.values[1])*2, r= as.numeric(model.values[3]))#theta_a, theta_s,
        #print("profilewidth");print(profilewidth[[1]])
        
        #print("Selecting columns")
        #Calculates the selected columns from the Data
		selectedcols<-select.columns(model.values, header,Cameras)
        
        #Calculate bias
        bias<-calculate.bias(profilewidth=profilewidth[[1]], Data=Data[selectedcols], Speed=Speed, Time=as.numeric(model.values[4])*StepLength, Density=Density)
        
        # Adds bias to returned list
        Output[[i]]<-bias
    }
    
    return(Output)
}

calculate.all.biases<-function(Names,Names.Cameras,ModelSelection,Speed, Density, StepLength){
    
    #List to be outputted
    Output<-vector(mode="list",length=length(Names))
    
    for(current.name in 1:length(Names)){
        #print(paste("calculated.all.biases loop #:",current.name))
        #Load in data for the current data set
        current.data<-read.csv(Names[[current.name]],header = FALSE)
        current.cameras<-read.csv(Names.Cameras[[current.name]])
        
        Output[[current.name]]<-CalBias(ModelSelection=ModelSelection, Cameras=current.cameras, x=current.data, Speed, Density, StepLength )
        
    }
    
    return(Output);
}


plot.biases.boxplot<-function(all.data,colour.list,model.name){
    
    # for each model
    for(model in 1:length(all.data[[1]])){
        
        # draws plot
        plot(0,0,type="n", ylim=c(-6,6), xlim=c(-0.2,3.2), ylab="",xlab="", axes=FALSE ,main="")
        box()
        # zero bias line
        abline(h=0,col="grey",lty=2)
        # Colour for model
        colour<-colour.list[model,2]
        # Puts the tick marks on the graph
        axis(1,labels=FALSE,at=c(0,1,2,3) )
        axis(2,labels=FALSE,at=c(-4,0,4) )
        # labels y axis and the title
        mtext(side=2,at=c(-4,0,4),text=expression(-4,0,4),line=1)
        mtext(text=model.name[model],side=3,line=0)
        
        # For each of the data, draw a box plot the the "plot.number"th
        for(i in 1:length(all.data)){
            boxplot(x=all.data[[i]][[model]],at=(i-1),add=T,col=colour,axes=FALSE)
        
        }
    }

}


