# rm(list=ls(all=TRUE))

loadData <- function(location='/home/wijnand/R_workspace_ads/resources/train.csv')
{
  require(data.table)
  
  # nrows train: 45.840.618
  # nrows test: 6.042.136
  columnClasses <- sapply(read.csv("/home/wijnand/R_workspace_ads/resources/1000rows.csv",nrows=1000),class)
  numberOfRows <- 45840618
  
  rawData <- fread(input = location, sep = ",", header=T, stringsAsFactors = F, colClasses = columnClasses, 
                   nrows = numberOfRows)
}