#ex 1: adding two numbers
add2<-function(x,y){
  x+y
}

#ex 2: extract any number above 10 inside a vector
above10<-function(x){
  use<-x>10
  x[use]
}

#ex 3: extract any number above an arbitrary value inside a vector
above<-function(x,n){
  use<-x>n
  x[use]
}
#it will print an error if n is not specified while calling the function


#ex 4: to write de ex3 function BUT specifying DEFAULT values
above<-function(x,n=10){
  use<-x>n
  x[use]
}
#it will NOT print an error if n is not specified while calling the function
#because it will just take the default value


#ex 5: take a matrix or data frame and calculate the mean of each row or col
colummean<-function(y,removeNA=TRUE){
  nc<-ncol(y) #calculate the length of the column
  means<-numeric(nc) #create a numeric vector with length nc
  for(i in 1:nc){
    means[i]<-mean(y[,i],na.rm=removeNA)  #assign to each position the mean of each column
                                          #ignoring NAs on the column if the condition is set TRUE (default)
    }
  means #to return the vector of the means
}