filelocation<- file.choose()
first5k <- read.csv(filelocation, nrows = 5000)
classes <- sapply(first5k, class)
classes[5:6] <- "numeric" 

#Make sure the classes are correct. Above code may not be able to succesfully model the classes 
taball <- read.csv(filelocation, colClasses = classes)
library(lubridate)
first5k$datadate <- ymd(first5k$datadate)
taball$datadate <- ymd(taball$datadate)

#finding uniquedates
library(zoo)
uniquedates <- unique(as.Date(first5k$datadate))
lenuniquedates <- length(uniquedates)

#finding unique companies
uniquecompanies <- unique(taball$tic)
uniquecompanies <- as.character(uniquecompanies)


###Plotting graph for each company 

#Creating space for 3 graphs 
par(mfrow=c(3,1))

#Funtion to provide daily return 
dailyreturn <- function(closeprice)
{
  resultlength <- length(closeprice)
  result <- data.frame(V1 = numeric(),
                       stringsAsFactors=FALSE)
  for(i in 1:(resultlength-1))
  {
    result[i,1] <- (closeprice[i+1] - closeprice[i])*100/closeprice[i]
  }
  result
}

#Initiate graph maker
i = 1

#Run Graphs!

i <- i + 1
specificcompany <- taball[taball$tic == uniquecompanies[i],]
result <- dailyreturn(specificcompany$prccd)
 
plot(specificcompany$datadate, specificcompany$cshtrd)
plot(specificcompany$datadate, specificcompany$prccd)
plot(specificcompany$datadate, specificcompany$prccd*specificcompany$cshtrd, type ="l")
