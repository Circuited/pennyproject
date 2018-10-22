StockPriceDeciles = function(){

BaseStock = read.csv("20171016-BSEstockprice.csv", header = TRUE)
dim(BaseStock)
quant = quantile(BaseStock$Closing.Price)
dec = quantile(BaseStock$Closing.Price, prob = seq(0, 1, length = 21))
print(quant)
print(dec)
}


MarDeciles = function(){
  
  BaseStock = read.csv("20171016-BSEstockprice.csv", header = TRUE)
  dim(BaseStock)
  quantile(BaseStock$Market.Capitalisation)
  print(BaseStock)
  quantile(BaseStock$Market.Capitalisation, prob = seq(0, 1, length = 11))
}



#Boxplotting 

Stock = read.csv("20171016-BSEonlystockprice.csv", header = FALSE)
boxplot(Stock)$out
Stock[!Stock %in% boxplot.stats(Stock)$out]
