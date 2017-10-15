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

#check out the first rows
head(outcome_df)

#number of cols and rows
ncol(outcome_df) #46 variables
nrow(outcome_df) #4706 obs

#names of the cols
names(outcome_df)

#graph a simple histogram of the 30-day rates from heart attacks
outcome_df[,11]<-as.numeric(outcome_df[,11]) #All variables are chr. Need to coerce into num
                                       #NAs warning is ok
hist(outcome_df[,11])

#create a function with 2 arguments:
#-the abbreviated name of a state (col 7) --state--
#-an outcome name ("heart attack" (col 11), "heart failure" (col 17), "pneumonia" (col 23)) --outcome--
#the function has to read the .csv file and return the name of the hospital which has the 
#best (lowest) 30-day mortality for the specified outcome in that state
#the name provided is the name hospital.name (col 2)

#note that hospitals that do not have data on a particular outcome should be excluded from
#the set when deciding the rankings

#if there is a tie, the the hospital names should be sorted alphabetical, and the first
#hospital in that set should be chosen

best<-function(state,outcome){
  if(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia"){  #check valid --outcome--
  
    if (outcome=="heart attack"){  #set the column depending on --outcome--
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
        stop("invalid state")
      }
      
      data<-data[check_state,]   #subset by --state--
      
      data[[3]]<-as.numeric(data[[3]]) #read --outcome-- as numeric
      na<-is.na(data[[3]])     #eliminate NAs
      data<-data[which(na==FALSE),]
  
      ordered_data<-data[order(data[[3]],data[[1]]),]#arrange the data from lower --outcome--,--hosp.name--
      print(ordered_data$Hospital.Name[1])     #print the name of the best hospital
  
  } else {
    stop("invalid outcome")
  }
}




