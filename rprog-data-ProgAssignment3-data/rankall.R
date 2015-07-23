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
  if (outcome == 'heart attack') {
    i <- 11 }
  else if (outcome == 'heart failure') {
    i <- 17 }
  else if (outcome == 'pneumonia') {
    i <- 23 }
  else {
    stop('invalid outcome')
  }
  
  ## Create the subset of the data that has the state and outcome, omitting the NAs
  extracted_data <- outcome_dataset[,c(2,7,i)]
  extracted_data[,3] <- suppressWarnings(as.numeric(as.character(extracted_data[,3])))
  extracted_data <- na.omit(extracted_data)
  
  for (j in 1:length(state)) {
    state_data <- extracted_data[extracted_data$State==state[j],]
    
    order <- order(state_data[,3], state_data[,1])
    state_data_ordered <- state_data[order,]
    n <- nrow(state_data_ordered)
    
    if (num == 'best') { p <- 1 }
    else if (num == 'worst') { p <- n }
    else if (num <= n) { p <- num }
    else { p <- NA }
    
    hospital[j] <- as.character(state_data_ordered[p,1])
  }
  
  return(data.frame(hospital=hospital, state=state))
}