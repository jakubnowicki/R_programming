complete <- function(directory, id = 1:332) {
  files <- list.files(path = directory)
  old.dir <- getwd()
  setwd(dir = directory)
  nobs.df <- data.frame(id = id,nobs = 0)
  for (i in 1:length(id)) {
    tmp <- read.csv(files[id[i]],header = T)
    compl<-complete.cases(tmp)
    nobs.df[i,"nobs"]<-nrow(tmp[compl,])
  }
  setwd(dir = old.dir)
  return(nobs.df)
}