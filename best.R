best <- function(state, outcome) {
  if (outcome == "heart attack") {
    out <- 11
  }
  else {
    if (outcome == "heart failure") {
      out <- 17
    }
    else {
      if (outcome == "pneumonia") {
        out <- 23
      }
      else {
        stop("invalid outcome")
      }
    }
  }
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[,c(2,7,out)]
  data <- data[data[,2]==state,]
  if (nrow(data)== 0) stop("invalid state")
  suppressWarnings(data[,3] <- as.numeric(data[,3]))
  data[,1] <- as.factor(data[,1])
  colnames(data) <- c('hospital','state','outc')
  data <- data[order(data$outc,data$hospital),]
  data <- data[complete.cases(data),]
  return(as.character(data[1,1]))
}