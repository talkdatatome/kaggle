# Ghetto TEXT MINER
library(tm)
options(header=F, stringsAsFactors=F)

# Set working directory 
setwd("C:/Users/campje01/Desktop/Home Depot/Data ") 

# Load the data
mydata <- read.csv("train.csv", header=T)
corpus_ST <- Corpus(VectorSource(mydata$search_term))

# Clean Up #
myCorpus <- tm_map(corpus_ST, tolower)
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

# Combing Words 
for (j in seq(myCorpus))
{
  myCorpus[[j]] <- gsub("hole saw", "hole_saw", myCorpus[[j]])
}

#Stemming - Removing common word endings (e.g., "ing", "es", "s")
library(SnowballC)   
myCorpus <- tm_map(myCorpus, stripWhitespace)

#dictCorpus <- myCorpus
#myCorpus <- tm_map(myCorpus, stemDocument) 
#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

inspect(myCorpus[1:4])

#Final Preprocess Step
myCorpus <- tm_map(myCorpus, PlainTextDocument)  

######### Data Matrix #######
dtm <- DocumentTermMatrix(myCorpus)   
dtm   

save.image("dataPrepped.RData")
inspect(dtm[1:5, 500:520])

m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")
matrix_ST <- read.csv("dtm.csv", header=T)

sink("dtm.txt")
cat(head(dtm))
sink()

ST_Data <- table(mydata_Trim,matrix_ST)

# inspectfrequent words
findFreqTerms(dtm, lowfreq=100)


