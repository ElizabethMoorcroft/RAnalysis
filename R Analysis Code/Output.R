# For each model output list
select.columns<-function(model.values, header,Cameras){ #model.values<-ModelSelection[i,]
    

	#row numbers for time, sensor, call, and density
	timerow		<-which(header[,1]=="Time")
	sensorrow	<-which(header[,1]=="Sensor")
	callrow		<-which(header[,1]=="Call")
	densityrow	<-which(header[,1]=="Density")
    
    # So that the header column numbers match the Data column numbers
    header<-header[,-1]
    
    # Calculates the Sensor number
    CamerarowI	<-Cameras[which(model.values[1]==Cameras[,5] & model.values[3]==Cameras[,6]),1]
    
    # Works out the correct column
    selectedcols<-which(header[sensorrow,]== CamerarowI &
                        header[callrow,]  == model.values[2] &
                        header[timerow,]  == model.values[4])
    
    print("selectedcols");print(selectedcols[1])
    
    #returnsvalues
    return(selectedcols[1])
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

CalBias<-function(ModelSelection, Cameras, x, Speed, Density, StepLength, x.position, file.input ){
	    
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
        profilewidth<-calcProfileWidth(theta_a = as.numeric(model.values[2])*2, theta_s = as.numeric(model.values[1])*2, r = as.numeric(model.values[3]))
        #print("profilewidth");print(profilewidth[[1]])
        
        #print("Selecting columns")
        #Calculates the selected columns from the Data
		selectedcols<-select.columns(model.values, header,Cameras)
        
        #Calculate bias
        bias<-calculate.bias(profilewidth=profilewidth[[1]], Data=Data[selectedcols], Speed=Speed, Time=as.numeric(model.values[4])*StepLength, Density=Density)
        
        # Adds bias to returned list
        Output[[i]]<-list(bias,as.character(model.values[5]),x.position[i],file.input)# saves: bias values; model name; position on the x axis
    }
    
    return(Output)
}

calculate.all.biases<-function(Names,Names.Cameras,ModelSelection,Speed, Density, StepLength, x.position){
    
    #List to be outputted
    Output<-vector(mode="list",length=length(Names))
    
    for(current.name in 1:length(Names)){
        #print(paste("calculated.all.biases loop #:",current.name))
        #Load in data for the current data set
        current.data<-read.csv(Names[[current.name]],header = FALSE)
        current.cameras<-read.csv(Names.Cameras[[current.name]])
        
        Output[[current.name]]<-CalBias(ModelSelection=ModelSelection, Cameras=current.cameras, x=current.data, Speed, Density, StepLength, x.position[,current.name],current.name )
        
    }
    
    return(Output);
}

#unlist(biases,recursive=FALSE)
select.model<-function(Data,model.name,colour,type){

    list.numbers<-which(lapply(Data,function(x){which(x[[2]]==model.name)})==1)
    data.from.model<-Data[list.numbers]
    
    for(i in 1:length(list.numbers)){
        temp<-Data[list.numbers[i]][[1]]
        plot.type(data=temp,type,colour)
    }
    
}

plot.type<-function(data,type,colour){
    if(type=="boxplot"){
        boxplot(x=data[[1]],at=data[[3]],add=T,col=colour,axes=FALSE)
    }
    else{
        d<-as.matrix(data[[1]])
        meanadj<-mean(d);
        stardarderr<-sd(d)/sqrt(length(d))
        #print(paste("meanadj",meanadj,"stardarderr",stardarderr))
        #if(data[[4]]==1){plotCI(x=data[[3]],y=meanadj,uiw=(1.96*stardarderr),add=T,col=colour)}
        #else{plotCI(x=data[[3]]+0.2,y=meanadj,uiw=(1.96*stardarderr),add=T,col="black")}
        if(data[[4]]==1){boxplot(x=data[[1]],at=data[[3]]-0.1,add=T,col=colour,axes=FALSE,boxwex=0.3)}
        else{boxplot(x=data[[1]],at=data[[3]]+0.1,add=T,col="grey",axes=FALSE,boxwex=0.3)}
    }
}


plot.biases.boxplot<-function(all.data,colour.list,model.name,ylim,ylab,type){
    
    Data<-unlist(all.data,recursive=FALSE)
    no.boxplots.per.graph<-max(unlist(lapply(unlist(all.data,recursive=FALSE),function(x){return(x[[3]])})),na.rm=TRUE)
    
    # for each model
    for(model in 1:length(model.name)){
        
        name.of.model<-model.name[model]
        
        # Colour for model
        colour<-colour.list[which(colour.list[,1]==name.of.model),2]
        # draws plot
        plot(0,0,type="n", ylim=c(-ylim,ylim), xlim=c(0.8, no.boxplots.per.graph+0.2), ylab="",xlab="", axes=FALSE ,main="")
        box()
        # zero bias line
        abline(h=0,col="grey",lty=2)
        # Puts the tick marks on the graph
        axis(1,labels=FALSE,at=c(1:no.boxplots.per.graph))
        axis(2,labels=FALSE,at=ylab )
        # labels y axis and the title
        mtext(side=2,at=ylab,text=ylab,line=1)
        mtext(text=name.of.model,side=3,line=0)
        
        # For each of the data, draw a boxplot the "model" data
        select.model(Data,model.name,colour,type)
    }

}

