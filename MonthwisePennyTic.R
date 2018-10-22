filelocation<- file.choose()
yearinput <- 2014
idpennymonthbased <- function( filelocation,pennydef, yearinput){
  library(zoo)
  library(lubridate)
  
  first5k <- read.csv(filelocation, nrows = 5000)
  classes <- sapply(first5k, class)
  taball <- read.csv(filelocation, colClasses = classes)
  first5k$datadate <- ymd(first5k$datadate)
  taball$datadate <- ymd(taball$datadate)

  #finding unique dates
  taball <- taball[year(taball$datadate) == (yearinput),]
  
  #finding unique companies
  uniquecompanies <- unique(taball$tic)
  uniquecompanies <- as.character(uniquecompanies)

  result <- data.frame(tic=character(),
                     prccmmean = numeric(), 
                     prclmmean = numeric(),
                     stringsAsFactors=FALSE)
  resultlength <- length(uniquecompanies)

  for( i in 1:resultlength ){
    specificcompany <- taball[taball$tic == uniquecompanies[i],]
    prccmmean <- mean(specificcompany$prccm)
    prclmmean <- mean(specificcompany$prclm)
    result[i,1] <- uniquecompanies[i]
    result[i,2] <- prccmmean
    result[i,3] <- prclmmean
  }

  listtic <- result[which(result$prclmmean <= 5),]
  listticlength1 <- nrow(listtic)
  listticlength2 <- min(table(taball$datadate))
  listtic[listticlength1+1,1] <- listticlength1
  listtic[listticlength1+1,2] <- listticlength2
  
  listtic
  

}
