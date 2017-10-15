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

#best rankings by state

#Write a rankall function which takes 2 arguments:
#-an outcome name ("heart attack" (col 11), "heart failure" (col 17), "pneumonia" (col 23)) --outcome--
#-the ranking for that outcome ("best","worst", or an integer indicating the ranking) --num--

#the function reads the .csv file and and returns a 2-column data frame containing the hospital in each state
#that has the ranking in the specified --num--

#note that some hospital names may be NA
#the first column in the data frame is named hospital, and the second is named state

#hospitals that do not have data on a particular outcome should be excluded.
#the rankall() should handle ties in the same way as the rankhospital() does

rankall<-function(outcome,num="best"){
  
  if(outcome=="heart attack"|outcome=="heart failure"|outcome=="pneumonia"){  #check valid --outcome--
    
    states_names<-levels(as.factor(outcome_df$State)) #vector with the names of all states
    states_names<-states_names[order(states_names)]   #alphabetic order
    
    rank_df<-data.frame(hospital=character(length(states_names)),state=character(length(states_names))) 
    rank_df$hospital<-as.character(rank_df$hospital)
    rank_df$state<-as.character(rank_df$state)
    #create empty df and establish the objects as characters
    
    source("rankhospital.R")    #set the source of the rankhospital()
    
    for(i in 1:length(states_names)){
      state<-states_names[i]   #fulfill df with the names of the states
      rank_df$state[i]<-state
      
      result<-rankhospital(state=state,outcome=outcome,num=num)  #run the rankhospital() for each state
      
      rank_df$hospital[i]<-result   #fulfill with the name of the hospital
      
      
    }
    rank_df  #return the resultant df
    
  } else {
    stop("invalid outcome")
  }
  
}