complete <- function(directory, id = 1:332) {
  #initialize returned data frame
  df <- data.frame(id=integer(), nobs=integer())
  for (i in 1:length(id)) {
    filename <- sprintf("%03d.csv", id[i])
    file_loc <- paste(getwd(), directory, filename, sep="/")
    mydata <- read.csv(file_loc) #fails if file cannot be read
    filt_data <- mydata[complete.cases(mydata),]
    df[i,] <- c(id[i], nrow(filt_data))
  }
  df
}