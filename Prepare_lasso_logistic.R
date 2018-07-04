setwd('Stats_Big_Data')

if (!require("data.table")){
  install.packages("data.table", dependencies=TRUE)
  library("data.table")
  }else{
  library("data.table")
  }

if(!require("stringr")){
  install.packages("stringr", dependencies=TRUE)
  library("stringr")
}else{
  library('stringr')
}

if (!require("glmnet")){
  install.packages("glmnet", dependencies=TRUE)
  library("glmnet")
  }else{
  library("glmnet")
  }

if (!require("caret")){
  install.packages("caret", dependencies=TRUE)
  library("caret")
  }else{
  library("caret")
  }

data = data.frame(transpose(read.csv("alldata together.csv", header = FALSE, na.strings = 'NaN')))
colnames(data) = trimws(data[1, ],which = 'right')
data = data[-1,]

data$age = str_match(data[,1], "age ([0-9][0-9])")[,2]

# Extract ERP info
data$erp = str_match(data[,1], "erp (yes|no)")[,2]
data$erp = ifelse(data$erp=="yes" & !is.na(data$erp), 1, 0)

# Build target variable
data$target = str_match(data[,1], ".5 yr survival")
data$target = as.numeric(ifelse(data$target==">5 yr survival" , 1, 0))

data = data[,-1]
data <- data.frame(lapply(data, function(x) {gsub(",", ".", x)}))

#write.csv(data,'preprocessed.csv', row.names=FALSE)


data[] <- lapply(data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

# Lazy replacement of NAs with column mean

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

# Find full NA rows 

na_cols = sapply(data, function(x) sum(is.na(as.numeric(as.character(x)))))
full_na_cols = names(na_cols[na_cols == dim(data)[1]])


data = data[,!(names(data) %in% full_na_cols)]
data_for_lasso = as.matrix(data)

# Fit lasso

lassoResults <- cv.glmnet(x =data_for_lasso[,-24191], y=data_for_lasso[,24191], 
alpha=1, family="binomial", intercept=FALSE)

bestlambda<-lassoResults$lambda.1se

results<-predict(lassoResults,s=bestlambda,type="coefficients")

choicePred<-rownames(results)[which(results !=0)]

# Logistic Regression Fitting

set.seed('2018')

# Split to train/test, we should try different percentages
intrain = sample(seq(1,dim(data)[1],1),round(3/4 * dim(data)[1]),replace = FALSE)

# Use only lasso's predictors
train <- data[intrain , c(choicePred,'target')]
test_X = data[-intrain , choicePred]
test_Y = data[-intrain , 'target']

# Build Logistic model
model <- glm(target ~ .,family=binomial, data=train)

predictions = ifelse(predict(model,test_X, type = 'response')>.5,1,0)

# Summarize
summary(model)

confusionMatrix(factor(predictions),factor(test_Y))
