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

#specificcompany <- taball[taball$tic == uniquecompanies[i],]
#Get specific company list from Percentileranker.R

#Initiate graph maker
i = 0
#Run Graphs!
i <- i + 1

specificcompany <- taball[taball$tic == specificcompanylist[i,1],]
result <- dailyreturn(specificcompany$prccd)

#Creating space for 3 graphs 
par(mfrow=c(3,1))

plot(specificcompany$datadate, log(specificcompany$cshtrd+2),type ="b")
#The 2 is added to log to avoid NA values
plot(specificcompany$datadate, specificcompany$prccd)
plot(specificcompany$datadate[-1], (result$V1), type ="b")
length(which(result$V1 == 0))

xmat = cbind((specificcompany$cshtrd+2),specificcompany$prccd,specificcompany$prchd,specificcompany$prcld,specificcompany$prcod);
cr=cor(xmat)
cr
#With log trade volume
xmat = cbind(log(specificcompany$cshtrd+2),specificcompany$prccd,specificcompany$prchd,specificcompany$prcld,specificcompany$prcod);
cr=cor(xmat)
cr

#correlation  with absolute value of results
xmat = cbind(log(specificcompany$cshtrd[-1]+2),(result$V1));
cr=cor(xmat)
cr


summary(lm((specificcompany$cshtrd+2)~specificcompany$prccd))
#With log trade volume
reg1<-summary(lm(log(specificcompany$cshtrd+2)~specificcompany$prccd))
reg1

reg2<-summary(lm(abs(result$V1)~log(specificcompany$cshtrd[-1]+2)))
reg2

reg3<-summary(lm((result$V1)~log(specificcompany$cshtrd[-1]+2)))
reg3

par(mfrow=c(1,1))

plot(specificcompany$prccd,log(specificcompany$cshtrd+2))
abline(reg1)

plot(log(specificcompany$cshtrd[-1]+2),abs(result$V1))
abline(reg2)

plot(log(specificcompany$cshtrd[-1]+2),(result$V1))
abline(reg3)
