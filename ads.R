# rm(list=ls(all=TRUE))
# nrows train.csv 45.840.618
# nrows test.csv 6.042.136

runTraining <- function(percentageTrain=0.5)
{
        ads <- loadData()
        ads[["C9"]] <- NULL
        ads[["C17"]] <- NULL
        #print(str(ads))
        
        # row number of x% of the data for training set
        numberTrainingSet <- round(nrow(ads) * percentageTrain, digits=0) - 1
        trainingSet <- ads[1:numberTrainingSet,]
        testSet <- ads[(numberTrainingSet+1):nrow(ads),]
        
        print(str(trainingSet))
        print(table(trainingSet$C9))
        print(length(trainingSet$C9))
        
        print(str(testSet))
        print(table(testSet$C9))
        print(length(testSet$C9))
        
        trainedModel <- m5tree(trainingSet)
        #trainedModel <- linearModel(trainingSet)
        #trainedModel <- generalizedLinearModel(trainingSet)
        
        print(summary(trainedModel))
        result <- predict(trainedModel, testSet)
        result <- ifelse(result < 0, yes = 0, result)
        result <- ifelse(result > 1, yes = 1, result)
        
        
        printPredictionResults(result, testSet$Label)
}

loadData <- function(location='/home/wijnand/R_workspace_ads/resources/500krows.csv', numberOfValues=5000)
{
        columnClasses <- sapply(read.csv("/home/wijnand/R_workspace_ads/resources/1000rows.csv",nrows=1000),class)
        
        require(data.table)
        rawData <- fread(input = location, sep = ",", header=T, na.strings = "")
        
        rawData$Id <- NULL
        
        # randomize
        set.seed(123)
        rawData <- rawData[order(runif(nrow(rawData))),]
        
        # convert all character columns to factors, due to stupid bug in data.table
        for(i in 1:26)
        {
                columnName <- paste0("C", i)
                #rawData[[columnName]] <- as.factor(rawData[[columnName]])
                
                # select levels with more than x values
                relevantLevels <- table(rawData[[columnName]]) >= numberOfValues
                
                # get their names
                relevantLevels <- names(relevantLevels[relevantLevels==TRUE])
                #print(relevantLevels)
                
                # if not relevant relevant, NA
                rawData[[columnName]] <- ifelse(rawData[[columnName]] %in% relevantLevels, rawData[[columnName]], "no_value")
                #rawData[[columnName]] <- ifelse(rawData[[columnName]] == "", rawData[[columnName]], "no_value")
                rawData[[columnName]] <- factor(rawData[[columnName]])
                print(nlevels(rawData[[columnName]]))
                if(nlevels(rawData[[columnName]]) <= 2)
                {
                        
                        # remove when no levels reach the treshhold
                        rawData[[columnName]] <- NULL
                        print("removing column")
                }
                else
                {
                        print("keeping column")
                        #print(table(rawData[[columnName]]))
                }
                
        }
        rawData
}

m5tree <- function(data)
{
        require(RWeka)
        trainedModel <- M5P(Label ~ . , data)
}

linearModel <- function(data)
{
        print(str(data))
        trainedModel <- lm(Label ~ . , data)
}

generalizedLinearModel <- function(data)
{
        print(str(data))
        trainedModel <- glm(Label ~ . , data, family = "binomial")
}

printPredictionResults <- function(prediction, actual)
{
        print(summary(prediction))
        print(summary(actual))
        
        # Logarithmic loss - kaggle-method for score calc
        epsilon <- .000000000000001
        yhat <- pmin(pmax(prediction, rep(epsilon)), 1-rep(epsilon))
        logloss <- -mean(actual*log(yhat) + (1-actual)*log(1 - yhat))
        print(paste0("Logarithmic loss: ", logloss))
        print(paste0("First 10 predicions: ", prediction[1:10]))
}