rankhospital <- function(state, outcome, num = "best") {
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
  if (num == "best") {
    num <- 1
  }
  else {
    if (num == "worst") {
      num <- nrow(data)
    }
  }
  ifelse(nrow(data)<num,wynik <- NA,wynik <- as.character(data[num,1]))
  return(wynik)
}