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
data$target = ifelse(data$target==">5 yr survival" , 1, 0)

data = data[,-1]