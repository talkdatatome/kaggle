setwd("/home/tnt/HOMEDEPOT")

load("datasets.RData")
# merge all together by product_uid

unique.att <- data.frame(product_uid=unique(att$product_uid), att=rep(1, length(unique(att$product_uid))))
unique.desc <- data.frame(product_uid=unique(desc$product_uid), desc=rep(1, length(unique(desc$product_uid))))
unique.train <- data.frame(product_uid=unique(train$product_uid), train=rep(1, length(unique(train$product_uid))))
unique.test <- data.frame(product_uid=unique(test$product_uid), test=rep(1, length(unique(test$product_uid))))

unique_ids <- merge(unique.att, unique.desc, by="product_uid", all=TRUE) 
unique_ids <- merge(unique_ids, unique.train, by="product_uid", all=TRUE) 
unique_ids <- merge(unique_ids, unique.test, by="product_uid", all=TRUE) 

x <- unique_ids

x[is.na(x)] <- -1
save(list=c("x", "att", "desc", "train", "test"), file="unique_id_summary.RData")
