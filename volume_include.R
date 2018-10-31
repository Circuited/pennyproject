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

#dataframe to store data
result <- data.frame(date=as.Date(character()),
                     percentilerank = numeric(), 
                     stringsAsFactors=FALSE) 

specificdatedata <- numeric(length = lenuniquedates)
for(i in 1:lenuniquedates ){
  specificdatedata[i] <- median(taball[taball$datadate == as.Date(uniquedates[i]),11  ])
}

taball1 <- taball[complete.cases(taball[ , 7]),]

specificdatedata <- taball[taball$datadate == as.Date(uniquedates[1]),11]
sum(specificdatedata$cshtrd)
quantile(specificdatedata$cshtrd, c(0.025, 0.975), na.rm = TRUE)
boxplot(specificdatedata$cshtrd, na.rm = TRUE)
outl <- boxplot(specificdatedata$cshtrd, na.rm = TRUE)$out
outl
boxplot(specificdatedata$cshtrd[!specificdatedata$cshtrd %in% outl])
intcomp <- specificdatedata[which(specificdatedata$cshtrd > median(specificdatedata$cshtrd, na.rm = TRUE)),]
intcomp
gv3036 <- taball[which(taball$gvkey == 3036),]
gv3036
head(gv3036)
plot(x = gv3036$datadate , y = gv3036$cshtrd/1000000, type = 'l')
hist(gv3036$cshtrd, nclass = 30)
plot(gv3036$datadate,gv3036$prccd, type = 'l')
max(gv3036$prccd[which(year(gv3036$datadate) == 2015)])
max(gv3036$prccd[which(year(gv3036$datadate) == 2016)])
min(gv3036$prccd[which(year(gv3036$datadate) == 2016)])
plot(gv3036$datadate ,gv3036$prccd, type ='l')


