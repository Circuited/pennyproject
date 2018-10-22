
filelocation <- file.choose()

idpennypercentile <- function( filelocation, pennydef){

  #to assign the class of each column, taking 5k as standard
  first5k <- read.csv(filelocation, nrows = 5000)
  classes <- sapply(first5k, class)

  #Reading in all values
  taball <- read.csv(filelocation, colClasses = classes)

  #Converting date to date data type
  library(lubridate)
  first5k$datadate <- ymd(first5k$datadate)
  taball$datadate <- ymd(taball$datadate)

  #finding uniquedates
  library(zoo)
  uniquedates <- unique(as.Date(first5k$datadate))

  #dataframe to store data
  result <- data.frame(date=as.Date(character()),
                     percentilerank = numeric(), 
                     stringsAsFactors=FALSE) 

  #finally finding percentile rank
  resultlength <- length(uniquedates)
  for( i in 1:resultlength ){
    specificdatedata <- taball[taball$datadate == as.Date(uniquedates[i]),]
    percentilerank <- ecdf(specificdatedata$prccd)(pennydef)
    result[i, 1] <- uniquedates[i]
    result[i, 2] <- percentilerank
  }
  result
}


  