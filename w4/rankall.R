rankall <- function(outcome, num = "best") {
  ## Check variables are correct
  outcomes = c("heart attack", "heart failure", "pneumonia")
  states = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  ## Load the data
  outcome <- tolower(outcome)
  file_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Create proper field name
  field = "Hospital.30.Day.Death..Mortality..Rates.from"
  outcome <- unlist(strsplit(outcome, " "))
  for (i in 1:length(outcome)) {
    outcome[i] <- paste(toupper(substr(outcome[i], 1, 1)), substr(outcome[i], 2, nchar(outcome[i])), sep="")
    field <- paste(field, outcome[i], sep=".")
  }
  
  ## Initialize results data frame
  result <- data.frame(hospital=character(), state=as.numeric())
  
  ## Select lowest value from field
  for (i in 1:length(states)) {
   state_data <- file_data[file_data[, "State"] == states[i], ]
   outcome_data <- state_data[,c("Hospital.Name", field)]
   filt_outcome_data <- outcome_data[outcome_data[,field] != "Not Available", ]
   if (num == "best") {
     number = 1
   } else if (num == "worst") {
     number = nrow(filt_outcome_data)
   }
   sorted_outcome_data <- filt_outcome_data[order(as.numeric(filt_outcome_data[,2]), filt_outcome_data[,1]), ]
   result <- rbind(result, data.frame(hospital = sorted_outcome_data[number, 1], state = states[i]))
  }
  rownames(result) <- states
  result
}