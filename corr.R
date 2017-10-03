corr<-function(directory,thershold=0){
  cc<-complete(directory)
  

  corr_data<-matrix(nrow=332,ncol=1)
  
 for(i in 1:332){
   
    if(cc$nobs[i]>thershold){
      
        if(i<10){
          j<-paste("00",i,sep="")
        }else if(i<100){
          j<-paste("0",i,sep="")
        }else{
          j<-i
        }
      
        data<-read.csv(paste(paste("~/COURSERA/JHU-Data Science Specialization", directory, j ,sep="/"),".csv",sep=""))
        data<-na.omit(data)
        corr_data[i,1]<-cor(data$nitrate,data$sulfate)
        
    }else{
      corr_data[i,1]<-NA
    }
   
 }
  corr_data<-as.data.frame(corr_data)
  corr_data<-na.omit(corr_data)
  cor_data<-as.numeric(corr_data[,1])
  cor_data
}