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
    myCorpus[[j]] <- gsub("@", " ", myCorpus[[j]])   
  } 
  # remove numbers
  #myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove stopwords
  remove <- stopwords("english")
  myStopwords <- remove[-143]
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  # Elminate Extra Whitespace #
  myCorpus <- tm_map(corpus_ST, stripWhitespace)
  
  myCorpus
}


#' cleaned corpus to documenttermmatrix
CleanedCorpusToDTM <- function(corpus){
  #Final Preprocess Step
  myCorpus <- tm_map(corpus, PlainTextDocument)  
  
  ######### Data Matrix #######
  dtm_ST <- DocumentTermMatrix(myCorpus) 
  
  dtm_ST
}

####################
### END OF FUNCTIONS
####################
library(tm)
library(dplyr)
options(header=F, stringsAsFactors=F)

# Set working directory 
setwd("C:/Users/campje01/Desktop/Home Depot/Data ") 

# Load the data
mydata <- read.csv("train.csv", header=T)
corpus_ST <- Corpus(VectorSource(mydata$search_term))
myCorpus <- baseDataCleaning(corpus_ST)
dtm_ST <- CleanedCorpusToDTM(myCorpus)

save.image("baseDataPrep.RData")

corpus_PT <- Corpus(VectorSource(mydata$product_title))
myCorpus_PT <- baseDataCleaning(corpus_PT)
dtm_PT <- CleanedCorpusToDTM(myCorpus_PT)


mydata_desc <- read.csv("product_descriptions.csv", header=T)
merged_data <- merge(mydata,mydata_desc,by="product_uid",all.x = TRUE)

corpus_DS <- Corpus(VectorSource(mydata$product_description))
myCorpus_DS <- baseDataCleaning(corpus_DS)
dtm_DS <- CleanedCorpusToDTM(myCorpus_DS)



