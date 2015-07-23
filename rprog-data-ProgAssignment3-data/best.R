## This function takes in 2 arguments: the abbreviated state name and the outcome
## name. The funtion will read in the outcome-of-care-measures.csv file and return
## the name of the hospital that has the lowest 30-day mortality for the outcome
## in that state. The outcomes can be heart attack, heart failure, or pneumonia. 
## Hospitals without data on specific outcomes will be excluded. 

best <- function(state, outcome) {
  ## Read outcome data and hospital data
  outcome_dataset <- read.csv("outcome-of-care-measures.csv")
  
  ## check that the state and outcome are valid
  if(!any(state == outcome_dataset$State)) {
    stop('invalid state')
  }
  
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
  state_data <- subset(outcome_dataset, outcome_dataset$State == state, select = c(2,i))
  state_data[,2] <- suppressWarnings(as.numeric(as.character(state_data[,2])))
  state_data <- na.omit(state_data)
  
  ## Return hospital name in that state with the lowest 30 day mortality rate.
  order <- order(state_data[,2])
  state_data_ordered <- state_data[order,]
  return(as.character(state_data_ordered[1,1]))
}