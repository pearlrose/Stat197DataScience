setwd("C:/Users/user pc/Desktop")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)

outcome[,11] <-  as.numeric(outcome[,11])
hist(outcome[,11])

### part2

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


##part3

rankhospital <- function(state, outcome, num) {
  
  ##data
  data <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2,7,11,17,23)]
  
  ##for names
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ##outcomes
  outcomes =c("heart attack", "heart failure", "pneumonia")
  
  if(outcome %in% outcomes == F)
  {
    stop("Invalid Outcome")
  }
  
  ##for state
  states <-  data[ ,2]
  states <- unique(states)
  if(state %in% states == F){
    stop("Invalid State")
  }

## rows with state value
  if(num != "best" && num != "worst" && num%%1 !=0){
    stop("imvalid number")
  }
  
  data <-  data[data$state == state & data[outcome] != 'Not Available', ]

  ##order by date and outcome
  
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing= F), ]
  data <- data[order(data[outcome], decreasing= F), ]

  ## for row index
  
  vals <- data[, outcome]
  if(num == "best") {
    rownum <- which.min(vals)
  } else if(num == "worst"){
    rownum <- which.max(vals)
  } else{
    rownum <-  num
  }
    #return
  data[rownum, ]$name
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
