pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # Create the vector of the means of the pollutant
  pollutant_mean <- c()
  
  # read in all the csv files from specdata
  files <- as.character(list.files(directory))
  path <- paste(directory, files, sep = "")
  for(i in id) {
    working_monitor <- read.csv(path[i])
    pollutant
    no_na <- working_monitor[!is.na(working_monitor[, pollutant]), pollutant]
    pollutant_mean <- c(pollutant_mean, no_na)
  }
  answer <- mean(pollutant_mean)
  rounded <- round(answer,3)
  return(rounded)
}