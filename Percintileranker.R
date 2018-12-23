#percentile calc is the market cap of the company
percentilecalc <- NULL
percentilecalc <- (taball$cshoc*taball$prccd)

taball <- cbind(taball, percentilecalc)

percentilecreator <- data.frame(tic = character(),
                                avgmktcap = numeric(),
                                stringsAsFactors=FALSE)

#retreive unique companies from pattern checking
for(l in 1:length(uniquecompanies))
{
  specificcompany <- taball[taball$tic == uniquecompanies[l],]
  percentilecreator[l,1]<- uniquecompanies[l]
  percentilecreator[l,2]<- mean(specificcompany$percentilecalc)
}

quantile(percentilecreator$avgmktcap, c(0.05,0.95), na.rm = TRUE)

percentilelevels <- quantile(percentilecreator$avgmktcap, c(0.05,0.95), na.rm = TRUE)

#Company with lower than 5%ile marketcap
specificcompanylist <- percentilecreator[percentilecreator$avgmktcap <= percentilelevels[1],]

#Company with higher than 95%ile marketcap
#specificcompanylist <- percentilecreator[percentilecreator$avgmktcap >= percentilelevels[2],]

specificcompanylist <- specificcompanylist[complete.cases(specificcompanylist),] 
