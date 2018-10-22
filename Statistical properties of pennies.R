filelocation<- file.choose()
first5k <- read.csv(filelocation, nrows = 5000)
classes <- sapply(first5k, class)
taball <- read.csv(filelocation)
library(lubridate)
first5k$datadate <- ymd(first5k$datadate)
taball$datadate <- ymd(taball$datadate)

#finding uniquedates
library(zoo)
uniquedates <- unique(as.Date(first5k$datadate))

summary(taball$prccd)
