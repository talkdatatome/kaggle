setwd("~/HOMEDEPOT/")



#load("baseDataPrep.RData")



# Ghetto TEXT MINER



####################

### START OF FUNCTIONS

####################

#' cleans data to point as basis for all further datasets

baseDataCleaning <- function(corpus_ST){

  ##### Pre-processing ####

  #create the toSpace content transformer

  toSpace <- content_transformer(function(x, pattern){return (gsub(pattern," ", x))})

  corpus_ST<- tm_map(corpus_ST, toSpace, "-")

  corpus_ST<- tm_map(corpus_ST, toSpace, ":")

  

  #toX <- content_transformer(function(y, pattern){return (gsub(pattern,"x", y))})

  #corpus_ST<- tm_map(corpus_ST, toX, "*")

  

  

  ##### Transformation #####

  # Elminate Extra Whitespace #

  myCorpus <- tm_map(corpus_ST, stripWhitespace)

  # Convert to Lower Case #

  myCorpus <- tm_map(myCorpus, content_transformer(tolower))

  # remove punctuation

  myCorpus <- tm_map(myCorpus, removePunctuation)

  for(j in seq(myCorpus))   

  {   

    myCorpus[[j]]$content <- gsub("@", " ", myCorpus[[j]]$content)   

  } 

  # remove numbers

  #myCorpus <- tm_map(myCorpus, removeNumbers)

  # remove stopwords

  remove <- stopwords("english")

  myStopwords <- remove[-143]

  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

  # Elminate Extra Whitespace #

  myCorpus <- tm_map(myCorpus, stripWhitespace)

  

  myCorpus

}





#' cleaned corpus to documenttermmatrix

CleanedCorpusToDTM <- function(corpus){

  #Final Preprocess Step

  myCorpus <- tm_map(corpus, PlainTextDocument)  

  

  ######### Data Matrix #######

  dtm <- DocumentTermMatrix(myCorpus) 

  dtm_ST<-(removeSparseTerms(dtm, 0.999))

  

  dtm_ST

}



####################

### END OF FUNCTIONS

####################

library(tm)

library(dplyr)

library(SnowballC)

options(header=F, stringsAsFactors=F)



# Set working directory 

#setwd("C:/Users/campje01/Desktop/Home Depot/Data ") 

#save.image("temp.RData")

# Load the data

mydataT <- read.csv("train.csv", header=T)

corpus_ST <- Corpus(VectorSource(mydataT$search_term))

myCorpusT <- baseDataCleaning(corpus_ST)

dtm_ST <- CleanedCorpusToDTM(myCorpusT)



mydata_descT <- read.csv("product_descriptions.csv", header=T)

merged_dataT <- merge(mydataT,mydata_descT,by="product_uid",all.x = TRUE)



save.image("baseDataPrep.RData")



corpus_PT <- Corpus(VectorSource(merged_dataT$product_title))

myCorpus_PT <- baseDataCleaning(corpus_PT)

dtm_PT <- CleanedCorpusToDTM(myCorpus_PT)





corpus_DS <- Corpus(VectorSource(merged_dataT$product_description))

myCorpus_DS <- baseDataCleaning(corpus_DS)

dtm_DS <- CleanedCorpusToDTM(myCorpus_DS)



final_data_question_markT <- cbind(dtm_DS, dtm_PT, dtm_ST)





save.image("trainModel.RData")

load("trainModel.RData")
library(tm)
library(SnowballC)
library(gamlr)

# add prefix to names
dtm_ST$dimnames[[2]] <- paste("ST", dtm_ST$dimnames[[2]], sep="_")
dtm_DS$dimnames[[2]] <- paste("DS", dtm_DS$dimnames[[2]], sep="_")
trainX <- cbind(dtm_ST, dtm_DS)
trainY <- mydataT$relevance

m1 <- cv.gamlr(x=trainX,y=trainY, family="gaussian")
save.image("gamlr_model2.RData")
baseDataCleaning <- function(corpus_ST){
  ##### Pre-processing ####
  #create the toSpace content transformer
  toSpace <- content_transformer(function(x, pattern){return (gsub(pattern," ", x))})
  corpus_ST<- tm_map(corpus_ST, toSpace, "-")
  corpus_ST<- tm_map(corpus_ST, toSpace, ":")
  
  #toX <- content_transformer(function(y, pattern){return (gsub(pattern,"x", y))})
  #corpus_ST<- tm_map(corpus_ST, toX, "*")
  
  
  ##### Transformation #####
  # Elminate Extra Whitespace #
  myCorpus <- tm_map(corpus_ST, stripWhitespace)
  # Convert to Lower Case #
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  for(j in seq(myCorpus))   
  {   
    myCorpus[[j]]$content <- gsub("@", " ", myCorpus[[j]]$content)   
  } 
  # remove numbers
  #myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove stopwords
  remove <- stopwords("english")
  myStopwords <- remove[-143]
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  # Elminate Extra Whitespace #
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  myCorpus
}


#' cleaned corpus to documenttermmatrix
CleanedCorpusToDTM <- function(corpus){
  #Final Preprocess Step
  myCorpus <- tm_map(corpus, PlainTextDocument)  
  
  ######### Data Matrix #######
  dtm <- DocumentTermMatrix(myCorpus) 
  dtm_ST<-(removeSparseTerms(dtm, 0.999))
  
  dtm_ST
}
####################
### END OF FUNCTIONS
####################
library(tm)
library(gamlr)
library(SnowballC)

load("gamlr_model2.RData")
test <- read.csv("head_test.csv")
corpus_STT <- Corpus(VectorSource(test$search_term))
myCorpusT <- baseDataCleaning(corpus_STT)
dtm_STT <- CleanedCorpusToDTM(myCorpusT)
# remove any terms not in Terms(dtm_ST) # but now the issue is adding sparse terms
myCorpusT <- tm_map(myCorpusT, removeWords, intersect(Terms(dtm_ST), Terms(dtm_STT)))
dtm_STT <- CleanedCorpusToDTM(myCorpusT)

mydata_descT <- read.csv("product_descriptions.csv", header=T)
merged_dataT <- merge(test,mydata_descT,by="product_uid",all.x = TRUE)

corpusPD <- Corpus(VectorSource(merged_dataT$product_description))
myCorpusPDT <- baseDataCleaning(corpusPD)
dtmPD <- CleanedCorpusToDTM(myCorpusPDT)
# remove any terms in dtmPD
myCorpusPDT <- tm_map(myCorpusPDT, removeWords, intersect(Terms(dtm_DS), Terms(dtmPD)))
dtmPD <- CleanedCorpusToDTM(myCorpusPDT)


testX <- cbind(dtm_STT, dtmPD)
### TRY - I'm still missing terms
# try adding ncol and dimnames for empty names
missings <- setdiff(c(Terms(dtm_ST), Terms(dtm_DS)), Terms(dtm_STT))
testX$ncol <- testX$ncol + length(missings)
testX$dimnames[[2]] <- c(testX$dimnames[[2]], missings)
print(dim(trainX))
print(dim(testX))
print(setdiff(trainX$dimnames[[2]], testX$dimnames[[2]]))
### TRY

save.image("test_of_test.RData")
#library(tm)
#library(SnowballC)
#library(gamlr)

# this fails because we didn't add back in terms that were in the train dtm_ST but not in dtm_STT
#pred <- predict(m1, testX)

