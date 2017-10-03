pollutantmean<-function(directory,pollutant,id=1:332){
  k<-0
  sum_data<-matrix(nrow=length(id),ncol=1)
  length_data<-matrix(nrow=length(id),ncol=1)
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
                        sum_data[k,1]<-sum(na.omit(data[[pollutant]]))
                        length_data[k,1]<-length(na.omit(data[[pollutant]]))
  }
  mean_pollutant<-sum(sum_data[,1])/sum(length_data[,1])
}


