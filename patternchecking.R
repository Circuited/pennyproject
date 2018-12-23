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
i = 0

#Run Graphs!

i <- i + 1
#specificcompany <- taball[taball$tic == uniquecompanies[i],]

#Get specific company list from Percentileranker.R

specificcompany <- taball[taball$tic == specificcompanylist[i,1],]
result <- dailyreturn(specificcompany$prccd)
 
plot(specificcompany$datadate, log(specificcompany$cshtrd+2),type ="b")

#The 2 is added to log to avoid NA values
plot(specificcompany$datadate, specificcompany$prccd)
plot(specificcompany$datadate[-1], (result$V1), type ="b")


xmat = cbind(log(specificcompany$cshtrd+2),specificcompany$prccd,specificcompany$prchd,specificcompany$prcld,specificcompany$prcod);
cr=cor(xmat)
cr

summary(lm(log(specificcompany$cshtrd+2)~specificcompany$prccd))


par(mfrow=c(2,1))
plot(log(specificcompany$cshtrd+2), specificcompany$prccd)
plot(specificcompany$prccd,log(specificcompany$cshtrd+2))

