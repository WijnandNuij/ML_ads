# rm(list=ls(all=TRUE))
# nrows train.csv 45.840.618
# nrows test.csv 6.042.136

runTraining <- function(percentageTrain=0.7)
{
        ads <- loadData()
        
        # row number of x% of the data for training set
        numberTrainingSet <- round(nrow(ads) * percentageTrain, digits=0) - 1
        trainingSet <- ads[1:numberTrainingSet,]
        testSet <- ads[(numberTrainingSet+1):nrow(ads),]
        
        #trainedModel <- c50(trainingSet)
        trainedModel <- m5tree(trainingSet)
        result <- predict(trainedModel, testSet)
        
        printPredictionResults(result, testSet$Label)
}

loadData <- function(location='/home/wijnand/R_workspace_ads/resources/500krows.csv', levelTreshHold = 20)
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
                
                # to start simple, remove columns with a lot of values
                if(nlevels(rawData[[columnName]]) > levelTreshHold)
                {
                        rawData[[columnName]] <- NULL
                }
        }
        
        #rawData[["Label"]] <- as.factor(rawData[["Label"]])
        
        # randomize
        rawData <- rawData[order(runif(nrow(rawData))),]
        
        print(str(rawData))
        rawData
}


m5tree <- function(data)
{
        require(RWeka)
        trainedModel <- M5P(Label ~ . , data)
}

c50 <- function(data)
{
        require(C50)
        trainedModel <- C5.0(Label ~ . , data)
}

printPredictionResults <- function(prediction, actual)
{
        #require(gmodels)
        #table <- CrossTable(prediction, testset$Label, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
        #                    prop.t = TRUE, dnn=(c('actual result', 'predicted result')))
        #precision <- (table$prop.tbl[1,1] + table$prop.tbl[2,2]) * 100
        #print(paste0("Precision as percentage: ", round(precision, digits=3)))
        
        # Logarithmic loss - kaggle-method for score calc
        epsilon <- .000000000000001
        yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
        logloss <- -mean(actual*log(yhat) + (1-actual)*log(1 - yhat))
        print(paste0("Logarithmic loss: ", logloss))
}