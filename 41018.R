
filelocation <- file.choose()

idpennypercentile <- function( filelocation, pennydef){
  
  #to assign the class of each column, taking 5k as standard
  first5k <- read.csv(filelocation, nrows = 5000)
  classes <- sapply(first5k, class)
  
  #Reading in all values
  taball <- read.csv(filelocation, colClasses = classes)
  
  #Converting date to date data type
  library(lubridate)
  first5k$datadate <- ymd(first5k$ClosingBestAskDate)
  taball$datadate <- ymd(taball$ClosingBestAskDate)
  
  #finding uniquedates
  library(zoo)
  uniquedates <- unique(as.Date(taball$ClosingBestAskDate))
  
  #dataframe to store data
  result <- data.frame(date=as.Date(character()),
                       percentilerank = numeric(), 
                       stringsAsFactors=FALSE) 
  
  #finally finding percentile rank
  resultlength <- length(uniquedates)
  for( i in 1:resultlength ){
    specificdatedata <- taball[taball$ClosingBestAskDate == as.Date(uniquedates[i]),]
    percentilerank <- ecdf(specificdatedata$LastPrice)(pennydef)
    result[i, 1] <- uniquedates[i]
    result[i, 2] <- percentilerank
  }
  result
}
OTCmarket <- idpennypercentile(filelocation,5)

