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


#-------------------------------------------------------------------------
#-------------- feature importance check no 3 , ----------------
#-------------- checking on precision of high class, while removing atts ----------------
#-------------------------------------------------------------------------
# Build Logistic model
library(boot)

# bestlambda = lassoResults$lambda.min
# min error lambda is 0.107045
# l = c(0.0005, 0.001, 0.005, 0.01, 0.05, 0.1 , 0.5 , 1)
# Fit lasso
# lassoResults = cv.glmnet(x =data_for_lasso[,-24191], y=data_for_lasso[,24191], 
#                          alpha=1, lambda = l, family="binomial", intercept=FALSE)
lassoResults = cv.glmnet(x =data_for_lasso[,-24191], y=data_for_lasso[,24191], 
                         alpha=1, family="binomial", intercept=FALSE)
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
n<-dim(tmp)[2]
n
# Split to train/test, we should try different percentages
intrain = sample(seq(1,dim(tmp)[1],1), round(3/4 * dim(tmp)[1]), replace = FALSE)

train = tmp[intrain,]
test_X = train[-intrain, 1:(n-1) ]
test_Y = train[-intrain , n]

# CV on train set
as.matrix(tmp[,n])
model = cv.glmnet(x =as.matrix(tmp[,1:(n-1)]), y=as.matrix(tmp[,n]),
                  alpha=0, family="binomial", intercept=FALSE)

# The test set is actually holdout data
predictions = ifelse(predict(model, as.matrix(test_X), type = 'response') > .5, 1, 0)

# Summarize
summary(model)

# Confusion matrix
conf = confusionMatrix(factor(predictions),factor(test_Y))
conf = as.matrix(conf)
conf

# The accuracy
Accuracy = as.character(sum(conf[1,1],conf[2,2])*100/sum(conf))
Accuracy

tmp2 <- tmp
# colnames(tmp)[-40]
for( name in colnames(tmp2)[-n] ){
  tmp2<-tmp
  tmp2[name]<-NULL
  
  # print(name)
  # Split to train/test, we should try different percentages
  intrain2 = sample(seq(1,dim(tmp2)[1],1), round(3/4 * dim(tmp2)[1]), replace = FALSE)
  
  train2 = tmp2[intrain2,]
  test_X2 = train2[-intrain2, 1:(n-2) ]
  test_Y2 = train2[-intrain2 , (n-1)]
  
  # CV on train set
  model2 = cv.glmnet(x =as.matrix(tmp2[,1:(n-2)]), y=as.matrix(tmp2[,(n-1)]),
                     alpha=0, family="binomial", intercept=FALSE)
  
  # The test set is actually holdout data
  predictions2 = ifelse(predict(model2, as.matrix(test_X2), type = 'response') > .5, 1, 0)
  
  # Summarize
  summary(model2)
  
  # Confusion matrix
  conf2 = confusionMatrix(factor(predictions2),factor(test_Y2))
  conf2 = as.matrix(conf2)
  conf2
  
  # The accuracy
  Accuracy2 = as.character(sum(conf2[1,1],conf2[2,2])*100/sum(conf2))
  Accuracy2
  Precision2 = as.character(conf2[2,2]*100/sum(conf2[2,1],conf2[2,2]))
  Precision2
  cat(Accuracy2," ",Precision2, " ",name,"\n")
}
# For min l, all atts dont worsen precision of class 1 if removed.
# For other l, again all atts dont worsen precision if removed.
# Only the accuracy is worse.
# This means we can have further removing of atts.
# UPD: I tried and for 2 level removing, still precision is untouched.
# I will recover the original min l and retry.
# Again precision or accuracy wont fall.
# I have to go inverse way, to build up the combinations.

dim(tmp)
tmp2 <- tmp
# colnames(tmp)[-40]
print(colnames(tmp)[-n])
for( name in colnames(tmp)[-n] ){
  tmp2<-tmp
  tmp2[name]<-NULL
  tmp3<-tmp2
  # for( name2 in colnames(tmp2)[-c(n-1,n)] ){
  for( name2 in colnames(tmp2)[-c(n-1)] ){
      
    # print(tmp3[,'target'])
    tmp3<-tmp2
    tmp3[name2]<-NULL
    # print(tmp3[,'target'])
    
    # print(name)
    # Split to train/test, we should try different percentages
    intrain2 = sample(seq(1,dim(tmp3)[1],1), round(3/4 * dim(tmp3)[1]), replace = FALSE)
    
    train2 = tmp3[intrain2,]
    test_X2 = train2[-intrain2, 1:(n-3) ]
    test_Y2 = train2[-intrain2 , (n-2)]
    
    # CV on train set
    model2 = cv.glmnet(x =as.matrix(tmp3[,1:(n-3)]), y=as.matrix(tmp3[,(n-2)]),
                       alpha=0, family="binomial", intercept=FALSE)
    
    # The test set is actually holdout data
    predictions2 = ifelse(predict(model2, as.matrix(test_X2), type = 'response') > .5, 1, 0)
    
    # Summarize
    summary(model2)
    
    # Confusion matrix
    conf2 = confusionMatrix(factor(levels = c("0","1"),predictions2),factor(levels = c("0","1"),test_Y2))
    conf2 = as.matrix(conf2)
    conf2
    
    # The accuracy
    Accuracy2 = as.character(sum(conf2[1,1],conf2[2,2])*100/sum(conf2))
    Accuracy2
    Precision2 = as.character(conf2[2,2]*100/sum(conf2[2,1],conf2[2,2]))
    Precision2
    cat(Accuracy2," ",Precision2, " ",name," ",name2,"\n")
  }
}

#-------------------------------------------------------------------------
#-------------- feature importance check no 4 , ----------------
#-------------- checking on precision of high class, while adding atts----------------
#-------------------------------------------------------------------------

dim(tmp)
cols <- colnames(tmp)[-n]
for( name in cols ){
  cols2 <- cols [! cols %in% name]
  for( name2 in cols2 ){
    # print(c(name,name2,"target"))
    tmp4<-tmp[,c(name,name2,"target")]
    
    # print(name)
    # Split to train/test, we should try different percentages
    intrain2 = sample(seq(1,dim(tmp4)[1],1), round(3/4 * dim(tmp4)[1]), replace = FALSE)
    
    train2 = tmp4[intrain2,]
    test_X2 = train2[-intrain2,1:2]
    test_Y2 = train2[-intrain2,3]
    
    # CV on train set
    model2 = cv.glmnet(x =as.matrix(tmp4[,1:2]), y=as.matrix(tmp4[,3]),
                       alpha=0, family="binomial", intercept=FALSE)
    
    # The test set is actually holdout data
    predictions2 = ifelse(predict(model2, as.matrix(test_X2), type = 'response') > .5, 1, 0)
    
    # Summarize
    summary(model2)
    
    # Confusion matrix
    conf2 = confusionMatrix(factor(levels = c("0","1"),predictions2),factor(levels = c("0","1"),test_Y2))
    conf2 = as.matrix(conf2)
    conf2
    
    # The accuracy
    num_acc = round(sum(conf2[1,1],conf2[2,2])*100/sum(conf2),2)
    Accuracy2 = as.character(num_acc)
    Accuracy2
    num_prec = round(conf2[2,2]*100/sum(conf2[2,1],conf2[2,2]),2)
    Precision2 = as.character(num_prec)
    Precision2
    if((num_acc > 95) && (num_prec == 100)){
      cat(Accuracy2," ",Precision2, " ",name," ",name2,"\n")
    }
  }
}
# Only those combinations were efficient:
# 100   100   NM_003147.SSX2   NM_005243.EWSR1 
# 100   100   NM_005243.EWSR1   AA555029_RC 
# 100   100   NM_003862.FGF18   NM_003147.SSX2 
# 100   100   AJ011306.DKFZP586J0119   NM_004709.CXorf1 
# 100   100   AA555029_RC   AL080059 
# 100   100   AL080059   Contig15164_RC 
# 100   100   AL080059   Contig32125_RC 
# 100   100   NM_006544.SEC10L1   M26880.UBC 
# 100   100   NM_016564.LOC51286   NM_013306.SNX15 
# 100   100   NM_000436.OXCT   Contig23399_RC 
# 100   100   Contig32125_RC   NM_013306.SNX15 
# 
