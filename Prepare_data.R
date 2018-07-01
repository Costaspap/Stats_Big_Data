setwd('Stats_Big_Data')

if (!require("data.table")){
  install.packages("data.table", dependencies=TRUE)
  library("data.table")
}else if(!require("stringr")){
  install.packages("stringr", dependencies=TRUE)
  library("stringr")
}else{
  library("data.table")
  library('stringr')
}


data = data.frame(transpose(read.csv("alldata together.csv", header = FALSE, na.strings = 'NaN')))
colnames(data) = trimws(data[1, ],which = 'right')
data = data[-1,]

data$age = str_match(data[,1], "age ([0-9][0-9])")[,2]

data$erp = str_match(data[,1], "erp (yes|no)")[,2]
data$erp = ifelse(data$erp=="yes" & !is.na(data$erp), 1, 0)


data$target = str_match(data[,1], ".5 yr survival")
data$target = as.numeric(ifelse(data$target==">5 yr survival" , 1, 0))

data = data[,-1]
data <- data.frame(lapply(data, function(x) {gsub(",", ".", x)}))

#write.csv(data,'preprocessed.csv', row.names=FALSE)


data[] <- lapply(data, function(x) {
  if(factor(x)) as.numeric(as.character(x)) else x
})
sapply(class,data)

# Lazy replacement of NAs with column mean

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#### Still working on this part
ready_data = model.matrix(target ~ .,data)

# Fit lasso


mod = glmnet(target ~ ., data = ready_data)


cv.glmmod <- glmnet(x =ready_data[,-24484], y=as.factor(ready_data[,24484]), alpha=1, family="binomial")