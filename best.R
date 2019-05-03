setwd("C:/Users/user pc/Desktop")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)

outcome[,11] <-  as.numeric(outcome[,11])
hist(outcome[,11])

###2
##hosp.data <- read.csv("hospital-data.csv", colClasses = "")

best <- function(state, outcome){
  outcomes =c("heart attack", "heart failure", "pneumonia")
  
  if(outcome %in% outcomes == F)
  {
    stop("Invalid Outcome")
  }
  data <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2,7,11,17,23)]
  ##for names
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  ##for state
  states <-  data[ ,2]
  states <- unique(states)
    if(state %in% states == F){
      stop("Invalid State")
    }
  
  data <- data[data$state == state & data[outcome] != 'Not Available', ]
  vals <-  data[, outcome]
  
  ##for row
  rownum <- which.min(vals)
  
  ##return
  data[rownum, ]$name
}
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
