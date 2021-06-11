
setRealSectors <- function(realSectoredStocks, data) {
  require(readxl)
  realSectoredStocks$conm
  realSectors = levels(as.factor(realSectoredStocks$Sector))
  stockNames = levels(as.factor(data$conm))
  data$tic[data$conm == realSectoredStocks$conm[1]][1]
  
  tickers = c("")
  
  for (t in 1:length(realSectoredStocks$conm)) {
    tickers[t] = data$tic[data$conm == realSectoredStocks$conm[t]][1]
  }
  
  realSectoredStocks["Tickers"] = tickers
  
  sectorsWithStocks = list()
  for (sec in 1:length(realSectors)) {
    sectorsWithStocks[[sec]] = realSectoredStocks$Tickers[realSectoredStocks$Sector == realSectors[sec]]
  }
  names(sectorsWithStocks) = realSectors
  return(sectorsWithStocks)
}


getSectorPrices <- function(allSectors, specificSector, begin, end) {
  require(quantmod)
  stocks = allSectors[[specificSector]]
  dummyStock = getSymbols(stocks[1], from = begin, to = end, env = NULL)[,6]
  numStocks = length(dummyStock)
  output = matrix(nrow = numStocks, ncol = length(stocks))
  for (stock in 1:length(stocks)) {
    output[,stock] = getSymbols(stocks[stock], from = begin, to = end, env = NULL)[,6]
  }
  output = as.data.frame(output)
  colnames(output) = stocks
  rownames(output) = index(dummyStock)
  return(output)
}

realSectoredStocks <- read_excel("sector list.xlsx")
data <- read.csv("Fundamentals Quarterly Data.csv")

allSectors <- setRealSectors(realSectoredStocks, data)
materialsPrices <- getSectorPrices(allSectors, "Materials", "2018-01-01", "2018-03-31")


