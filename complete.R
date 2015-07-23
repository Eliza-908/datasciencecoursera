complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # initialize where to store the completed cases 
  complete_cases <- rep(0,length(id))
  
  # read in the files from the specdata directory
  files <- as.character(list.files(directory))
  path <- paste(directory, files, sep = "")
  j <- 1
  for(i in id) {
    working_monitor <- read.csv(path[i])
    complete_cases[j] = sum(complete.cases(working_monitor))
    j <- j + 1
  }
  # create the data frame with the complete cases
  result <- data.frame(id = id, nobs = complete_cases)
  return(result)
}