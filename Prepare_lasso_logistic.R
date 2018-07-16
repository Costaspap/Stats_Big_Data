#myDir <- "/home/datum/msc_active_repos/stats_big_data"
#setwd(myDir)
#setwd('Stats_Big_Data')

# Check if packages are already installed and then load them.
for (package in c("data.table","stringr","glmnet","caret","mlbench","RANN")){
  if (!require(package, character.only=TRUE)){
    install.packages(package, character.only=TRUE, dependencies=TRUE)
  }
  library(package, character.only=TRUE)
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
data$target = as.numeric(ifelse(data$target==">5 yr survival" , 0, 1))

data = data[,-1]
data = data.frame(lapply(data, function(x) {gsub(",", ".", x)}))

#write.csv(transpose(data),'preprocessed.csv', row.names=FALSE)

data[] = lapply(data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

# Find full NA rows 

na_cols = sapply(data, function(x) sum(is.na(as.numeric(as.character(x)))))
full_na_cols = names(na_cols[na_cols == dim(data)[1]])


data = data[,!(names(data) %in% full_na_cols)]

target = data$target

impute = preProcess(data[,-24191], method = "knnImpute", k = 10)
data = predict(impute,data[,-24191])
data$target = target
lasso_data = as.matrix(data)


# Fit lasso

lassoResults = cv.glmnet(x =lasso_data[,-24191], y=lasso_data[,24191], 
                         alpha=1, family="binomial", intercept=FALSE)

bestlambda = lassoResults$lambda.min

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



#-------------------------------------------------------------------------
#-------------- feature importance RFE ----------------
#-------------------------------------------------------------------------

tmp<-as.data.frame(lasso_data)
tmp<-tmp[,c(choicePred,'target')]
n<-dim(tmp)[2]
n

tmp[1:3,1:3]
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#?rfe
colnames(tmp)

results <- rfe(tmp[,1:n-1],tmp[,n], sizes=c(1:n-1), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
