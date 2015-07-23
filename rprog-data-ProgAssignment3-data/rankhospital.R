## This function takes in 3 arguments: the abbreviated state name, the outcome
## name, and the num argument that takes the ranking "best", "worst", or an integer. 
## The funtion will read in the outcome-of-care-measures.csv file and return
## a character vector with the name of the hospital that has the ranking specified
## by the num argument. If the num exceeds the number of the hospitals in the state, 
## then the function returns NA. Hospitals that do not have data on a particular
## outcome then it will be excluded from the set of hospitals when determining
## rankings. Ranking ties will be decided by alphabetical order. 

rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank 30 day death rate
  order <- order(state_data[,2], state_data[,1])
  state_data_ordered <- state_data[order,]
  n <- nrow(state_data_ordered)
  hospital_name <- if (num == 'best') {
    as.character(state_data_ordered[1,1]) }
  else if (num == 'worst') {
    as.character(state_data_ordered[n,1]) }
  else if (is.numeric(num)) {
    as.character(state_data_ordered[num,1]) }
  else {
    stop('NA')
  }
  return(hospital_name)
}