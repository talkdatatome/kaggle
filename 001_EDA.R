setwd("/home/tnt/HOMEDEPOT")

att <- read.csv("attributes.csv", stringsAsFactors=FALSE)
desc <- read.csv("product_descriptions.csv", stringsAsFactors=FALSE)
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

save.image("datasets.RData")
# merge all together by product_uid

