rankHospital <- function(state, outcome, num = "best") {
  ## Check variables are correct
  outcomes = c("heart attack", "heart failure", "pneumonia")
  states = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", "WV", "WI", "WY", "GU")
  if (!state %in% states) {
    stop("invalid state")
  }
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  ## Load the data
  outcome <- tolower(outcome)
  file_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_data <- file_data[file_data[,"State"] == state,]
  
  ## Create proper field name
  field = "Hospital.30.Day.Death..Mortality..Rates.from"
  outcome <- unlist(strsplit(outcome, " "))
  for (i in 1:length(outcome)) {
    outcome[i] <- paste(toupper(substr(outcome[i], 1, 1)), substr(outcome[i], 2, nchar(outcome[i])), sep="")
    field <- paste(field, outcome[i], sep=".")
  }
  
  ## Select lowest value from field
  outcome_data <- state_data[,c("Hospital.Name", field)]
  filt_outcome_data <- outcome_data[outcome_data[,field] != "Not Available",]
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = nrow(filt_outcome_data)
  }
  sorted_outcome_data <- filt_outcome_data[order(as.numeric(filt_outcome_data[,2]), filt_outcome_data[,1]),]
  sorted_outcome_data[num,1]
}