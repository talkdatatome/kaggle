load("test_of_test.RData")
library(tm)
library(gamlr)
library(SnowballC)


dtm_STT$dimnames[[2]] <- paste("ST", dtm_STT$dimnames[[2]], sep="_")
dtmPD$dimnames[[2]] <- paste("DS", dtmPD$dimnames[[2]], sep="_")
testX <- cbind(dtm_STT, dtmPD)
### TRY - I'm still missing terms
# try adding ncol and dimnames for empty names

save.image("test_of_test.RData")
#library(tm)
#library(SnowballC)
#library(gamlr)

# this fails because we didn't add back in terms that were in the train dtm_ST but not in dtm_STT
#pred <- predict(m1, testX)

