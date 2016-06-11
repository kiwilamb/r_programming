corr <- function(directory, threshold=0) {
  result <- NULL
  c <- complete(directory)
  c <- subset(c, c$nobs>threshold)
  ids <- c$id
  result <- vector(mode="numeric", length=length(ids))
  if (length(ids) > 0) {
    for (i in 1:length(ids)) {
      filename <- sprintf("%03d.csv", ids[i])
      file_loc <- paste(getwd(), directory, filename, sep="/")
      mydata <- read.csv(file_loc)
      filt_data <- mydata[complete.cases(mydata),]
      correlation <- cor(filt_data$sulfate, filt_data$nitrate)
      result[i] <- correlation
    }
  }
  if (length(result) == 0) {
    result <- numeric()
  }
  result
}