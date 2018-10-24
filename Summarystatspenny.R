filelocation<- file.choose()
first5k <- read.csv(filelocation, nrows = 5000)
classes <- sapply(first5k, class)
taball <- read.csv(filelocation, colClasses = classes)
library(lubridate)
first5k$datadate <- ymd(first5k$datadate)
taball$datadate <- ymd(taball$datadate)

#finding uniquedates
library(zoo)
uniquedates <- unique(as.Date(first5k$datadate))

#Cleaning dataset,keeping only complete data and saving that 
taball1 <- taball[complete.cases(taball$prccd),]
(table(taball1$tic) == 250) 
Alldaystrade <- (table(taball1$tic) == 250)
Alldaystrade1 <- which(Alldaystrade == TRUE)
write.csv(Alldaystrade1, file = filelocation)

#USing CRSP found data, importing to R
filelocationpenny <- file.choose()
first5k <- read.csv(filelocationpenny, nrows = 5000)
classes <- sapply(first5k, class)
taballpenny <- read.csv(filelocationpenny, colClasses = classes)
library(lubridate)
first5k$date <- ymd(first5k$date)
taballpenny$date <- ymd(taballpenny$date)

#finding uniquedates
library(zoo)
uniquedatespenny <- unique(as.Date(first5k$date)) 

#Finding Summary stats for unique dates
result <- data.frame(date=as.Date(character()),
                     percentile0.025 = numeric(),
                     percentile0.5 = numeric(), 
                     percentile0.975 = numeric(), 
                     percentilevol0.025 = numeric(), 
                     percentilevol0.5 = numeric(), 
                     percentilevol0.975 = numeric(), 
                     stringsAsFactors=FALSE) 

resultlength <- length(uniquedatespenny)
for( i in 1:resultlength ){
  specificdatedata <- taballpenny[taballpenny$date == as.Date(uniquedatespenny[i]),]
  result[i, 1] <- uniquedatespenny[1]
  result[i,2] <- quantile(specificdatedata$PRC, c(0.025))
  result[i,3] <- quantile(specificdatedata$PRC, c(0.5))
  result[i,4] <- quantile(specificdatedata$PRC, c(0.975))
  result[i,5] <- quantile(specificdatedata$VOL, c(0.025), na.rm = TRUE)
  result[i,6] <- quantile(specificdatedata$VOL, c(0.5), na.rm = TRUE)
  result[i,5] <- quantile(specificdatedata$VOL, c(0.975), na.rm = TRUE)
  
}
