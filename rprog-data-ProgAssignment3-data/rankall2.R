## This function takes in 2 arguments: the outcome name, and the num argument that takes the 
## ranking "best", "worst", or an integer. 
## The funtion will read in the outcome-of-care-measures.csv file and return
## a 2-column dataframe containing the hospital in each state that has the ranking specified
## in num. The function will return a value for every state (some may be NA). The first column
## contains the hospital name, and the second contains the abbreviated name of the state. 
## Hospitals that do not have data on certain outcomes will be excluded when determining rankings. 
## Tied rankings are decided by alphabetical order. 

rankall <- function(outcome, num = "best") {
  ## Read outcome data and hospital data
  outcome_dataset <- read.csv("outcome-of-care-measures.csv")
  state <- sort(unique(outcome_dataset$State))
  hospital <- rep("", length(state))
  
  ## check that the outcome is valid, get the outcome type
  
  for (i in 1:length(state)) {
    state_data <- outcome_dataset[outcome_dataset$State == state[i],]
    if (outcome == 'heart attack') {
      mortality_rate <- as.numeric(state_data[,11]) }
    else if (outcome == 'heart failure') {
      mortality_rate <- as.numeric(state_data[,17]) }
    else if (outcome == 'pneumonia') {
      mortality_rate <- as.numeric(state_data[,23]) }
    else {
      stop('invalid outcome')
    }
    
    state_data <- na.omit(state_data)
    
    a <- rank(mortality_rate, na.last=NA)
    
    # Determine what position in the file want num to be 
    if (num == "best") { position <- 1 }
    else if (num == "worst") { position <- length(a) }
    else if (num <= length(a)) { position <- num }
    else { position <- NA }
    
    ## For each state, find the hospital with the given rank 
    if (is.na(position)) {
      hospital[i] <- NA
    } else {
      hospital[i] <- as.character(state_data$Hospital.Name[order(mortality_rate, state_data$Hospital.Name)[position]]) }
  }
  # print(head(state_data))
  return(data.frame(hospital = hospital, state = state))
}rankall2 <- function(outcome, num = "best" ){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state <- data$State
  state <- sort(unique(state))
  
  hospital <- rep("", length(state))
  
  for (i in 1:length(state)) {
    statedata<- data[data$State==state[i],]
    if (outcome == 'heart attack') {
      death <- as.numeric(statedata[,11])
    } else if (outcome == 'heart failure') {
      death <- as.numeric(statedata[,17])
    } else if (outcome == 'pneumonia') {
      death <- as.numeric(statedata[,23])
    } else {
      stop("invalid outcome")
    }
    
    a <- rank(death, na.last=NA)
    
    if (num=="best") {
      r <- 1
    } else if (num =="worst") {
      r <- length(a)
    } else if (num <= length(a) ) {
      r <- num
    } else {
      r <- NA
    }
    
    if (is.na(r)) {
      hospital[i] <- NA
    } else {
      hospital[i] <- statedata$Hospital.Name[order(death, statedata$Hospital.Name)[r]]
    }
    
  }
  
  return(data.frame(hospital=hospital, state=state))
}