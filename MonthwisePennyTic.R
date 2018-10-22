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
  
  #Making a dataframe that comprises of these companies
  result <- data.frame(tic=character(),
                     prccmmean = numeric(), 
                     prclmmean = numeric(),
                     stringsAsFactors=FALSE)
  resultlength <- length(uniquecompanies)
  
  #For loop to loop over finding minniumum in the year. 
  for( i in 1:resultlength ){
    
    #Specific company talks about 1 company 
    specificcompany <- taball[taball$tic == uniquecompanies[i],]
    
    #Mean of stock prices in the year.
    prccmmean <- mean(specificcompany$prccm)
    prclmmean <- mean(specificcompany$prclm)
    result[i,1] <- uniquecompanies[i]
    result[i,2] <- prccmmean
    result[i,3] <- prclmmean
  }
  #You find whether the mean is below the benchmark price
  listtic <- result[which(result$prclmmean <= pennydef),]
  listticlength1 <- nrow(listtic)
  listticlength2 <- min(table(taball$datadate))
  listtic[listticlength1+1,1] <- listticlength1
  listtic[listticlength1+1,2] <- listticlength2
  
  listtic

}
