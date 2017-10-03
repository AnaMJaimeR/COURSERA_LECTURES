complete<-function(directory, id=1:332){
  complete_data<-data.frame(id=numeric(length(id)),nobs=numeric(length(id)))

  k<-0
  for (i in id){
    if(i<10){
      j<-paste("00",i,sep="")
    }else if(i<100){
      j<-paste("0",i,sep="")
    }else{
      j<-i
    }
    k<-1+k
    data<-read.csv(paste(paste("~/COURSERA/JHU-Data Science Specialization", directory, j ,sep="/"),".csv",sep=""))
   
    complete_data$id[k]<-j
    complete_data$nobs[k]<-sum(complete.cases(data))
    
    
  }
  
  #complete_data<-as.data.frame(complete_data)
  #colnames(complete_data)<-c("id","nobs")
  complete_data$nobs<-as.numeric(complete_data$nobs)
  complete_data
  
}