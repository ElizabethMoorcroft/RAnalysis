
save.data_function<-function(biases,vector.character,save.data,save.name){
  for(i in 1:length(biases)){
    temp<-biases[[i]]
    for(j in 1:length(temp)){
      temp2<-temp[[j]]
      temp.matrix<-matrix(c(temp2[[2]],temp2$call,temp2$sensor,temp2$radius,vector.character[i],as.matrix(temp2[[1]])),ncol=1)
      save.data<-cbind(save.data,temp.matrix)
    }
  }
  write.csv(file=save.name,x=save.data,col.names=FALSE)
}

save.data_valuesformat_function<-function(data,call,sensor,radius,name,save.data){
    print(dim(save.data))
    temp.matrix<-matrix(c(name,call,sensor,radius,data),ncol=1)
    print(dim(temp.matrix))
    save.data<-cbind(save.data,temp.matrix)
    return(save.data)
}