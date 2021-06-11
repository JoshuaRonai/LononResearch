
FilteredData <- function(data, financials, actualTickers) {
  require(dplyr)
  data.useful <- data %>% filter(tic %in% actualTickers & fyearq %in% seq(2010, 2019))
  data2 = data.useful[c("fyearq", "fqtr","tic",financials)]
  data2[is.na(data2)] = 0
  return(data2)
}


MakeHierarchicalSectors <- function(data, financials, numSectors, actualTickers) {
  require(dplyr)
  filteredData = FilteredData(data, financials, actualTickers)
  years = rep(seq(2010, 2019, 1), each = 4)
  quarters = rep(1:4, 10)
  labels = paste(as.character(years),"-Q",as.character(quarters), sep = "")
  output = list()
  for (date in 1:40) {
    clusterable = filteredData %>% filter(fyearq == years[date] & fqtr == quarters[date])
    rownames(clusterable) = clusterable$tic
    clusterable = clusterable[4:length(clusterable)]
    dclust = dist(clusterable)
    hclust.stocks = hclust(dclust, method = "average")
    cutting = cutree(hclust.stocks, numSectors)
    stockNames = names(cutting)
    sectors = data.frame(Names = stockNames, Sectors = as.numeric(cutting))
    #sectors = data.frame(Sectors = as.numeric(cutting))
    output[[date]] = sectors
  }
  names(output) = labels
  return(output)
}


StocksSectors <- function(clusteredStocks) {
  stocks = sort(clusteredStocks[[1]][[1]])
  numStocks = length(stocks)
  output = matrix(nrow = numStocks, ncol = 40)
  for (date in 1:40) {
    cluster = clusteredStocks[[date]]
    for (stock in 1:numStocks) {
      output[stock, date] = cluster$Sectors[cluster$Names == stocks[stock]]
    }
  }
  years = rep(seq(2010, 2019, 1), each = 4)
  quarters = rep(1:4, 10)
  labels = paste(as.character(years),"-Q",as.character(quarters), sep = "")
  rownames(output) = stocks
  colnames(output) = labels
  return(output)
}

PlotQuarterHistogram <- function(clusterResults, financials, year, quarter) {
  require(ggplot2)
  date = paste(as.character(year),"-Q",as.character(quarter), sep = "")
  newSectors = clusterResults[["By-Quarter"]][[date]]
  p = ggplot(newSectors, aes(x = Sectors)) + 
    geom_histogram(binwidth = 0.5, fill = "dark green") +
    theme(legend.title = element_blank()) +
    labs(x = "Sectors", y = "Number of Stocks", title = paste(paste(toupper(financials), collapse = "/"), paste(as.character(year),"-Q",as.character(quarter), sep = ""), sep = ": "))
  
  numSectors = max(newSectors$Sectors)
  counts = rep(0, numSectors)
  for (sec in 1:numSectors) {
    counts[sec] = sum(newSectors$Sectors == sec)
  }
  counts = ifelse(counts > 10, 0, counts)
  
  for (sec in 1:numSectors) {
    if (counts[sec] != 0) {
      names = newSectors$Names[newSectors$Sectors == sec]
      height = 10
      for (stock in names) {
        p = p + annotate("text", x = sec, y = height, label = stock)
        height = height + 5
      }
    }
  }
  return(p)
}


HierarchicalClusters <- function(financials, numSectors) {
  require(xlsx)
  data = read.csv("Fundamentals Quarterly Data.csv")
  actualTickers = read_excel("sector list 2.xlsx", sheet = "Useful-Tickers")$Tickers
  HClusts = MakeHierarchicalSectors(data, financials, numSectors, actualTickers)
  sectoredStocks = StocksSectors(HClusts)
  output = list(HClusts, sectoredStocks)
  names(output) = c("By-Quarter", "Matrix-Form")
  return(output)
}


any("epsf12" == colnames(data))


financials <- c("apq", "atq", "capxy", "chq", "ciq", "dpq", "epsf12", "intanq", "lcoq")
numSectors <- 10

outputs <- HierarchicalClusters(financials, numSectors)

PlotQuarterHistogram(outputs, financials, 2010, 1)
PlotQuarterHistogram(outputs, financials, 2010, 2)
PlotQuarterHistogram(outputs, financials, 2010, 3)
PlotQuarterHistogram(outputs, financials, 2010, 4)
