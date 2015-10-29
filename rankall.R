rankall <- function(outcome, num = "best") {
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
  if (num != "best" & num != "worst" & is.numeric(num)==FALSE ) stop("invalid num")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[,c(2,7,out)]
  suppressWarnings(data[,3] <- as.numeric(data[,3]))
  data[,1] <- as.factor(data[,1])
  data[,2] <- as.factor(data[,2])
  colnames(data) <- c('hospital','state','outc')
  data <- data[order(data$state,data$outc,data$hospital),]
  data <- data[complete.cases(data),]
  if (num == "best") {
    num <- 1
  }
  get.hospital <- function(x,num) {
    x <- as.vector(x)
    if (num!="worst") {
      return(x[num])
    }
    else {
      n<-length(x)
      return(x[n])
    }
  }
  wynik <- tapply(data$hospital,INDEX = data$state,FUN = get.hospital,num = num)
  wynik <- as.data.frame(wynik)
  colnames(wynik) <- "hospital"
  wynik$state <- rownames(wynik)
  return(wynik)
}