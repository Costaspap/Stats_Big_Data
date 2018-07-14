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
data = data.frame(lapply(data, function(x) {gsub(",", ".", x)}))

#write.csv(transpose(data),'preprocessed.csv', row.names=FALSE)


data[] = lapply(data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

# Lazy replacement of NAs with column mean

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] = mean(data[,i], na.rm = TRUE)
}

# Find full NA rows 

na_cols = sapply(data, function(x) sum(is.na(as.numeric(as.character(x)))))
full_na_cols = names(na_cols[na_cols == dim(data)[1]])


data = data[,!(names(data) %in% full_na_cols)]
data_for_lasso = as.matrix(data)

# Fit lasso

lassoResults = cv.glmnet(x =data_for_lasso[,-24191], y=data_for_lasso[,24191], 
alpha=1, family="binomial", intercept=FALSE)

bestlambda = lassoResults$lambda.1se

results = predict(lassoResults,s=bestlambda,type="coefficients")

choicePred = rownames(results)[which(results !=0)]

# Logistic Regression Fitting

set.seed('2018')

# Split to train/test, we should try different percentages
intrain = sample(seq(1,dim(data)[1],1), round(3/4 * dim(data)[1]), replace = FALSE)

# Use only lasso's predictors
train = data[intrain , c(choicePred,'target')]
test_X = data[-intrain , choicePred]
test_Y = data[-intrain , 'target']

# Build Logistic model
model = glm(target ~ . ,family=binomial, data=train)

predictions = ifelse(predict(model, test_X, type = 'response') > .5, 1, 0)

# Summarize
summary(model)

confusionMatrix(factor(predictions),factor(test_Y))
                 
                 
                 

#---------------------------------------------------------------------------------
#-------------- Logistic Regression Fitting with different samples----------------
#---------------------------------------------------------------------------------


for(i in 1:50){
set.seed(i)

# Split to train/test, we should try different percentages
intrain = sample(seq(1,dim(data)[1],1), round(3/4 * dim(data)[1]), replace = FALSE)

# Use only lasso's predictors
train = data[intrain , c(choicePred,'target')]
test_X = data[-intrain , choicePred]
test_Y = data[-intrain , 'target']

# Build Logistic model
model = glm(target ~ . ,family=binomial, data=train)

predictions = ifelse(predict(model, test_X, type = 'response') > .5, 1, 0)

# Summarize
summary(model)

conf = confusionMatrix(factor(predictions),factor(test_Y))
conf = as.matrix(conf)
conf

print(sum(diag(conf))*100/sum(conf))}


#-------------------------------------------------------------------------
#-------------- Handicapped randomized stepwise regression----------------
#-------------------------------------------------------------------------



best_preds = c()

# Change the range depending on how many tests you want to try
for( loop in 1:20){
	
	# Get only 2 starter random features from lasso's choices

	fts = sample(1:length(choicePred),1)
	#print(c('LOOP',loop ))
	#print('')
	
	reached_100 = FALSE
	for(i in 2:length(choicePred)){
		set.seed(loop)
		
		# In every Loop add 1 random feature
		possible_indexes = (1:length(choicePred))[!(1:length(choicePred)) %in% fts]

		fts = c(fts,sample(possible_indexes,1))

		# Split to train/test, we should try different percentages
		intrain = sample(seq(1,dim(data)[1],1), round(3/4 * dim(data)[1]), replace = FALSE)

		# Use only lasso's predictors
		train = data[intrain , c(choicePred[fts] ,'target')]
		test_X = data[-intrain , choicePred[fts] ]
		test_Y = data[-intrain , 'target']


		# Build Logistic model
		model = glm(target ~ . ,family=binomial, data=train)

		predictions = ifelse(predict(model, test_X, type = 'response') > .5, 1, 0)

		# Summarize
		#summary(model)

		conf = confusionMatrix(factor(predictions),factor(test_Y))
		conf = as.matrix(conf)
		conf

		Accuracy = as.character(sum(conf[1,1],conf[2,2])*100/sum(conf))

		added_feature = tail(choicePred[fts], n=1)
		
		#Monitor the increase in accuracy after the new feature was added
		#print(c(Accuracy ,added_feature))
		
		# Keep predictor that made accuracy reach 100
		if((Accuracy == 100) & (reached_100 == FALSE)){
			best_preds = c(best_preds,added_feature)
			reached_100 = TRUE

			}

											} 
												}


# Check how many times did each predictor help the model reach 100 accuracy
table(best_preds[order(as.numeric(table(best_preds)))])
    
#-------------------------------------------------------------------------
#-------------- feature importance check no 1 ----------------
#-------------------------------------------------------------------------

#bckp_data_for_lasso <- data_for_lasso
data_for_lasso <- bckp_data_for_lasso
data_for_lasso[1:3,1:3]

# Fit lasso
lassoResults = cv.glmnet(x =data_for_lasso[,-24191], y=data_for_lasso[,24191], 
                         alpha=1, family="binomial", intercept=FALSE)
#bestlambda = lassoResults$lambda.1se
bestlambda = lassoResults$lambda.min
bestlambda
results = predict(lassoResults,s=bestlambda,type="coefficients")
results
choicePred = rownames(results)[which(results !=0)]
choicePred

# Logistic Regression Fitting
set.seed('2018')

tmp<-as.data.frame(data_for_lasso)
tmp<-tmp[,c(choicePred,'target')]
tmp$target <- as.factor(tmp$target)
tmp[1:3,1:3]
# load the library
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#?trainControl
# train the model
tmp
model <- train(target~., data=tmp, method="glm", preProcess=c("center", "scale"), trControl=control)
?train
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#-------------------------------------------------------------------------
#-------------- feature importance check no 2 ----------------
#-------------------------------------------------------------------------

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#?rfe
colnames(tmp)

results <- rfe(tmp[,1:39],tmp[,40], sizes=c(1:39), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
