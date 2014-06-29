# rm(list=ls(all=TRUE))
# nrows train.csv 45.840.618
# nrows test.csv 6.042.136

loadData <- function(location='/home/wijnand/R_workspace_ads/resources/500krows.csv')
{
        columnClasses <- sapply(read.csv("/home/wijnand/R_workspace_ads/resources/1000rows.csv",nrows=1000),class)
        
        require(data.table)
        rawData <- fread(input = location, sep = ",", header=T)
        
        # convert all character columns to factors, due to stupid bug in data.table
        for(i in 1:26)
        {
                columnName <- paste0("C", i)
                rawData[[columnName]] <- as.factor(rawData[[columnName]])
                rawData[[columnName]] <- as.numeric(rawData[[columnName]])
                rawData[[columnName]] <- as.factor(rawData[[columnName]])
        }
        print(str(rawData))
        rawData
}