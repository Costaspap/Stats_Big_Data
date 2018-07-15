setwd('C:\\Users\\eh363vg\\Desktop\\Msc_Data_Science\\Courses\\3rd Quarter\\Statistics for big Data\\Homeworks-Assignments\\FINAL')


if (!require("MASS")){
  install.packages("MASS", dependencies=TRUE)
  library("MASS")
}else{
  library("MASS")
}



if (!require("RANN")){
  install.packages("RANN", dependencies=TRUE)
  library("RANN")
}else{
  library("RANN")
}


if (!require("tidyverse")){
  install.packages("tidyverse", dependencies=TRUE)
  library("tidyverse")
}else{
  library("tidyverse")
}


if (!require("caret")){
  install.packages("caret", dependencies=TRUE)
  library("caret")
}else{
  library("caret")
}



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

data = data.frame(transpose(read.csv("alldatatogether.csv", header = FALSE, na.strings = 'NaN')))
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

data[] = lapply(data, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})


#----------------------------- DR -----------------------------#
# Check the number of missing values in each attribute

s = as.data.frame(apply(data,2,function(x) { sum(is.na(x))} ))
s = round((s/dim(data)[1])*100,2)
s = s[order(-s),,drop = F]
s

#---------------------------- end -----------------------------#


# Lazy replacement of NAs with column mean
counts = sapply(data, function(x) sum(is.na(x)))
thres = 0.5 
# The threshold is not needed. There are 293 variables full with NA and out of the rest, the "max" 
# NA percentage is 16.71%. Thus, either the threshold will be the latter number, or will not be 
# used at all

cols2null <- NULL

for(i in 1:ncol(data)){
  if(counts[i] < nrow(data)*thres){
    data[is.na(data[,i]), i] = mean(data[,i], na.rm = TRUE)
  }
  else{
    cols2null<-c(cols2null,c(i))
  }
}
data <- subset(data, select=-cols2null)


# Find full NA rows 

na_cols = sapply(data, function(x) sum(is.na(as.numeric(as.character(x)))))
full_na_cols = names(na_cols[na_cols == dim(data)[1]])


data = data[,!(names(data) %in% full_na_cols)]
data_for_lasso = as.matrix(data)

#--------------------------------- DR ------------------------------#
# Firstly, I exclude the features which are full of NAs
c = rownames(s[s==100,,drop=F])
data = data[,!(colnames(data) %in% c)]


# Secondly, I will try multiple hot deck imputation.
trial_data = preProcess(data, method = "knnImpute", k = 10)
lasso_data = predict(trial_data,data)
lasso_data = as.matrix(lasso_data)


# Finally, I search for rows filled with NA (if any had left)
a = as.data.frame(apply(t(lasso_data),2,function(x) { sum(is.na(x))} ))
a = round((a/dim(lasso_data)[2])*100,2)

# So , as we can see no rows with NAs left. Time for LASSO
lambdas <- 10 ^ seq(8,-8,length=250)

lasso_res = cv.glmnet(x =lasso_data[,-24191], y=lasso_data[,24191], 
          alpha=1, family="binomial", intercept=FALSE, lambda=lambdas )

bestlambda = lasso_res$lambda.min
res = predict(lasso_res,s=bestlambda,type="coefficients")

# Let's print the significant coefficients according to LASSO
coeffs = data.frame(coef.name = dimnames(coef(lasso_res,s = lasso_res$lambda.min))[[1]], coef.value = matrix(coef(lasso_res,s = lasso_res$lambda.min)))
coef = coeffs[as.numeric(coeffs$coef.value)>0,]

# As a last step I apply Logistic regression to the abovementioned features
n = coef$coef.name
log_data = lasso_data[,(colnames(lasso_data) %in% n)]
logistic = as.data.frame(cbind(data[,24191],log_data))
colnames(logistic)[1] = "Target"
# Apply Logistic Regression

set.seed('2018')

# Split to train/test
intrain = sample(seq(1,dim(lasso_data)[1],1), round(3/4 * dim(lasso_data)[1]), replace = FALSE)


# Use only lasso's predictors
train = logistic[intrain , ]
test = logistic[-intrain , ]
test_Y = logistic[-intrain , 1]


# Build Logistic model
model = glm(Target ~ . ,family=binomial, data=train)

predictions = ifelse(predict(model, test, type = 'response') > .5, 1, 0)

# Summarize
summary(model)

step.model <- model %>% stepAIC(trace = FALSE)
coef(step.model)

confusionMatrix(factor(predictions),factor(test_Y))

# --------------------------- end ----------------------------------#


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