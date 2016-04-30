load("gamlr_model.RData")

library(tm)
library(gamlr)

load("test_of_test.RData")
# with testX get column names to line up with trainX
#' trainFeatures is a vector from train$dimnames[[2]]
alignTest <- function(test, trainFeatures){
    testFeatures <- test$dimnames[[2]]
    # do test names align already with trainFeatures?
    if(isTRUE(all.equal(testFeatures, trainFeatures))){
       print("Test is ready for prediction.")
       return(TRUE) 
    }else{
        InTrainNotTest <- setdiff(trainFeatures, testFeatures)
        InTestNotTrain <-  setdiff(testFeatures, trainFeatures)

        # remove test features not in training
        newTest <- test[, c(testFeatures %in% trainFeatures)]

        # add train features not in test
        newTest$dimnames[[2]] <- c(newTest$dimnames[[2]], InTrainNotTest)
        newTest$ncol <- newTest$ncol + length(InTrainNotTest)

        newTestFeatures <- newTest$dimnames[[2]]
        # rearrange test naems to train
        # get train column order
        trainOrder <- data.frame(x=trainFeatures, newindex=1:length(trainFeatures))
        testOrder <- data.frame(x=newTestFeatures, index=1:length(newTestFeatures))

        lookup <- merge(trainOrder, testOrder, by="x")

        output <- newTest[, newTest$dimnames[[2]][lookup$newindex]]

        print("Train is now ready for prediction.")
        return(output)
    }
}

testX <- cbind(dtm_STT, dtmPD)

testX <- alignTest(testX, trainX$dimnames[[2]])

save(list=c("testX"), file="test_data.RData")
