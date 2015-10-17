corr <- function(directory, treshold = 0) {
  files <- list.files(path = directory)
  old.dir <- getwd()
  corr.vector <- NULL
  compl <- complete(directory)
  compl <- compl[compl$nobs > treshold,]
  if (nrow(compl) > 0) {
    setwd(dir = directory)
    for (i in 1:nrow(compl)) {
      tmp <- read.csv(files[compl[i,1]],header = T)
      correlation <- cor(x = tmp$sulfate, y = tmp$nitrate, use = "complete.obs")
      corr.vector <- c(corr.vector,correlation)  
    }
    setwd(dir = old.dir)
  }
  return(corr.vector)
}