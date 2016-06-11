pollutantmean <- function(directory, pollutant, id = 1:332) {
  #initialize vector to store means from all the files
  means <- vector(mode="numeric", length=length(id))
  #calculate mean from each file, without NA values
  for (i in 1:length(id)) {
    filename <- sprintf("%03d.csv", id[i])
    file_loc <- paste(getwd(), directory, filename, sep="/")
    mydata <- read.csv(file_loc) #fails if file cannot be read
    d <- mydata[pollutant] #fails if incorrect column name is specified
    d_filt <- d[!is.na(d)]
    print(mean(d_filt))
    means[i] <- mean(d_filt)
  }
  mean(means)
}