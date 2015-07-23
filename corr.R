corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  # call complete function, determine which rows uphold the threshold
  completed <- complete("specdata",1:332)
  nobs <- completed$nobs
  ids <- completed$id[nobs > threshold]
  
  # initialize vector to store correlations
  corr_vector <- c()
  
  # read in the files from the specdata directory
  files <- as.character(list.files(directory))
  path <- paste(directory, files, sep = "")
  j <- 1
  for(i in ids) {
    working_monitor <- read.csv(path[i])
    corr_vector[j] <- cor(working_monitor$sulfate, working_monitor$nitrate, use="complete.obs")
    j <- j+1
  }
  result <- corr_vector
  return(result)
}
