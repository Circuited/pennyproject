filelocation <- file.choose()

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
                     percentile1 = numeric(),
                     percentile2 = numeric(),
                     percentile3 = numeric(),
                     stringsAsFactors=FALSE)

#finally finding percentile rank
resultlength <- length(uniquedates)
for( i in 1:resultlength ){
  specificdatedata <- taball[taball$datadate == as.Date(uniquedates[i]),]
  result[i, 1] <- uniquedates[i]
  result[i, 2] <- quantile(specificdatedata$cshtrd, c(0.10), na.rm = TRUE)
  result[i, 3] <- quantile(specificdatedata$cshtrd, c(0.5), na.rm = TRUE)
  result[i, 4] <- quantile(specificdatedata$cshtrd, c(0.9), na.rm = TRUE)
}
result