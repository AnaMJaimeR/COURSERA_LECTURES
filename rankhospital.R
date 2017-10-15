#Programming Assignment 3

#The data for this assignment come from the Hospital Compare web site (http://hospitalcompare.hhs.gov)
#run by the U.S. Department of Health and Human Services. The purpose of the web site is to provide data and
#information about the quality of care at over 4,000 Medicare-certified hospitals in the U.S. This dataset essentially
#covers all major U.S. hospitals. This dataset is used for a variety of purposes, including determining
#whether hospitals should be fined for not providing high quality care to patients (see http://goo.gl/jAXFX
#for some background on this particular topic).
#The Hospital Compare web site contains a lot of data and we will only look at a small subset for this
#assignment. The zip file for this assignment contains three files
#• outcome-of-care-measures.csv: Contains information about 30-day mortality and readmission rates
#for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
#• hospital-data.csv: Contains information about each hospital.
#• Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book).
#A description of the variables in each of the files is in the included PDF file named Hospital_Revised_Flatfiles.pdf.

#directory
setwd("~/COURSERA/JHU-Data Science Specialization/LECTURES/C2-R Programming/Simulation and Profiling")

#read the data
outcome_df<-read.csv("outcome-of-care-measures.csv",colClasses = "character",stringsAsFactors = FALSE,na.strings = "Not Available")

#Ranking hospitals by outcome in a state

#Write a rankhospital function which takes 3 arguments:
#-the abbreviated name of a state (col 7) --state--
#-an outcome name ("heart attack" (col 11), "heart failure" (col 17), "pneumonia" (col 23)) --outcome--
#-the ranking of that state for that outcome ("best","worst", or an integer indicating the ranking) --num--

#the function reads the .csv file and returns a character vector with the name of the hospital
#that has the ranking specified by the num argument

#note for the --num-- argument that if the integer is larger than the number of hospitals, the function
#should return NA.

#hospitals not containing data in a particular outcome should be excluded

#hospitals with the same mortality rate should be broken by using hospital name



rankhospital<-function(state,outcome,num="best"){
  if(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia"){  #check valid --outcome--
    
    if (outcome=="heart attack"){     #set the column depending on --outcome--
      a<-11
    } else if (outcome=="heart failure"){
      a<-17
    } else {
      a<-23
    } 
    
    data<-outcome_df[,c(2,7,a)]   #subset the data by --outcome--
    #col 2 is Hospital.name and col 7 is state abbreviated
    
    check_state<-which(data$State==state)   #check valid --state--
    
    if (length(check_state)==0){ 
      result<-"INVALID"
      stop("invalid state")
    }
    
    data<-data[check_state,]   #subset by --state--
    
    data[[3]]<-as.numeric(data[[3]]) #read --outcome-- as numeric
    data<-na.omit(data)     #omit NAs
    ordered_data<-data[order(data[[3]],data[[1]]),]#arrange the data from lower --outcome--,--hosp.name--
    
    if(is.numeric(num)){  #verifying --num-- is numeric
      num2<-as.integer(num)  #verifying --num-- is an integer
      b<-num2==num
    } else{ 
      b<-FALSE    
    }
    
    #reading --num--
    if (num=="best"){        #print the name of the best hospital
      result<-ordered_data$Hospital.Name[1]
    } else if (num=="worst"){      #print the name of the worst hospital
      result<-ordered_data$Hospital.Name[nrow(ordered_data)]
    } else if (b==TRUE&num!=0&num<=nrow(ordered_data)) { #if num is an integer&between 0 and nrows
                                                         #print the name of the --num-- hospital
      result<-ordered_data$Hospital.Name[num]
    } else {
      result<-NA  #otherwise print NA
    }
    print(result)
    
  } else {
    result<-"INVALID"
    stop("invalid outcome")
  }
  
}


