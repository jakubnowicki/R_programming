pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(path = directory)
  old.dir <- getwd()
  setwd(dir = directory)
  poll<-NULL
  for (i in id) {
    tmp <- read.csv(files[i],header = T)
    poll<-c(poll,tmp[,pollutant])
  }
  setwd(old.dir)
  poll.mean <- mean(poll,na.rm = T)
  return(poll.mean)
}