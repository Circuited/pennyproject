#percentile calc is the market cap of the company
percentilecalc <- NULL
percentilecalc <- (taball$cshoc*taball$prccd)

taball <- cbind(taball, percentilecalc)

percentilecreator <- data.frame(V1 = character(),
                                V2 = numeric(),
                                stringsAsFactors=FALSE)

#retreive unique companies from pattern checking
for(i in 1:length(uniquecompanies))
{
  specificcompany <- taball[taball$tic == uniquecompanies[i],]
  percentilecreator[i,1]<- uniquecompanies[i]
  percentilecreator[i,2]<- mean(specificcompany$percentilecalc)
}

quantile(percentilecreator$V2, c(0.05,0.95), na.rm = TRUE)

percentilelevels <- quantile(percentilecreator$V2, c(0.05,0.95), na.rm = TRUE)

specificcompanylist <- percentilecreator[percentilecreator$V2 <= percentilelevels[1],]
 
specificcompanylist <- specificcompanylist[complete.cases(specificcompanylist),] 
