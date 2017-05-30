path <-  #set path
setwd(path)


# Load libraries and Data -------------------------------------------------

library(data.table)
library(jsonlite)
library(purrr)
library(stringr)
library(ranger)

#### Train
train <- fromJSON("train_data.json")

train_data <- data.table(ID = unlist(names(train)))
train_data[, `:=` (genres = unlist(lapply(train, '[',1)),
                   titles = unlist(lapply(train, '[',2)),
                   cities = unlist(lapply(train, '[', 3)),
                   segment = unlist(lapply(train, '[',4)),
                   dow = unlist(lapply(train, '[',5)),
                   tod = unlist(lapply(train, '[', 6))
)]


#### Test 
test <- fromJSON("test_data.json")

test_data <- data.table(ID  = unlist(names(test)))
test_data[,`:=` (genres = unlist(lapply(test, '[',1)),
                 titles = unlist(lapply(test, '[',2)),
                 tod = unlist(lapply(test, '[', 3)),
                 cities = unlist(lapply(test, '[',4)),
                 dow = unlist(lapply(test, '[',5))
)]


#### Check Train and Test
str(train)
str(test)



### Encode target
train_data[,segment := ifelse(segment == 'neg',0,1)]


# Feature Engineering -----------------------------------------------------

## Creating new column per genres

train_data[,g1 := lapply(genres, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))]
train_data[,g1 := lapply(g1, unlist, use.names=F)]

uniq_genres <- unique(unlist(lapply(train_data$genres, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))))
length(uniq_genres)

toColumns <- function(data, variables){
  
  for(i in variables){
    
    data[,paste0(i,"_gen") := lapply(g1, function(x) any(match(i,x)))]
    
  }
  return (data)
  
}

toColumns(train_data, uniq_genres)
train_data[,g1 := NULL]

## see how it looks
head(train_data)

## encode TRUE and NA
genx <- grep(pattern = "_gen", x = colnames(train_data), value = T)

for(k in genx)
  set(train_data, i = which(is.na(train_data[[k]])), j = k, value = 0)

for(k in genx)
  set(train_data, i = which(train_data[[k]] == TRUE), j= k ,value = 1)


### make changes in test data

test_data[,g1 := lapply(genres, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))]
test_data[,g1 := lapply(g1, unlist, use.names=F)]

uniq_genres <- unique(unlist(lapply(test_data$genres, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))))
length(uniq_genres)

toColumns <- function(data, variables){
  
  for(i in variables){
    
    data[,paste0(i,"_gen") := lapply(g1, function(x) any(match(i,x)))]
    
  }
  return (data)
  
}

toColumns(test_data, uniq_genres)
test_data[,g1 := NULL]

genx <- grep(pattern = "_gen", x = colnames(test_data), value = T)

for(k in genx)
  set(test_data, i = which(is.na(test_data[[k]])), j = k, value = 0)

for(k in genx)
  set(test_data, i = which(test_data[[k]] == TRUE), j= k ,value = 1)



## sum watch time from title

train_data[,t1 := lapply(titles, function(k) strsplit(x = k, split = ","))]
train_data[,t1 := lapply(t1, unlist, use.names = F)]
train_data[,t1 := lapply(t1, function(k) gsub(pattern = ".*\\:([0-9]+)",replacement = "\\1",x = k))]

train_data[,t1 := lapply(t1, function(x) paste(x,sep = " ", collapse = "+"))]
train_data[,title_sum := lapply(t1, function(x)eval(parse(text = x)))]
train_data[,title_sum := lapply(title_sum, function(x) ifelse(is_empty(x),0,x))] 
train_data[,t1 := NULL]

test_data[,t1 := lapply(titles, function(k) strsplit(x = k, split = ","))]
test_data[,t1 := lapply(t1, unlist, use.names = F)]
test_data[,t1 := lapply(t1, function(k) gsub(pattern = ".*\\:([0-9]+)",replacement = "\\1",x = k))]

test_data[,t1 := lapply(t1, function(x) paste(x,sep = " ", collapse = "+"))]
test_data[,title_sum := lapply(t1, function(x)eval(parse(text = x)))] #12 NA
test_data[,title_sum := lapply(title_sum, function(x) ifelse(is_empty(x),0,x))] 
test_data[,t1 := NULL]



## create count variables

train_data[,title_count := lapply(titles, function(x) str_count(string = x, pattern = ":"))]
train_data[,genres_count := lapply(genres, function(x) str_count(string = x, pattern = ":"))]
train_data[,cities_count := lapply(cities, function(x) str_count(string = x, pattern = ":"))]
train_data[,dow_count := lapply(dow, function(x) str_count(string = x, pattern = ":"))]
train_data[,tod_count := lapply(tod, function(x) str_count(string = x, pattern = ":"))]

test_data[,title_count := lapply(titles, function(x) str_count(string = x, pattern = ":"))]
test_data[,genres_count := lapply(genres, function(x) str_count(string = x, pattern = ":"))]
test_data[,cities_count := lapply(cities, function(x) str_count(string = x, pattern = ":"))]
test_data[,dow_count := lapply(dow, function(x) str_count(string = x, pattern = ":"))]
test_data[,tod_count := lapply(tod, function(x) str_count(string = x, pattern = ":"))]


## convert list to vectors - train
pd <- names(train_data)[sapply(train_data, is.list)]
train_data[, (pd) := lapply(.SD, unlist), .SDcols = pd]

pd <- names(test_data)[sapply(test_data, is.list)]
test_data[, (pd) := lapply(.SD, unlist), .SDcols = pd]


## remove variables for modeling - train
test_id <- test_data$ID
train_data[,c('ID','genres','titles','cities','dow','tod') := NULL]
test_data[,c('ID','genres','titles','cities','dow','tod') := NULL]



# Random Forest -----------------------------------------------------------

train_data[,segment := as.factor(segment)]
rf.model <- ranger(segment ~ ., data = train_data, num.trees = 500, mtry = 7, probability = T, replace = F, seed = 1221)
rf.model

rf.predict <- predict(rf.model, data = test_data,num.trees = 500)

#predict value for 1
pred <- rf.predict$predictions[,2]

#create submission file
sub_RF <- data.table(ID = test_id, segment = pred)
fwrite(sub_RF,"starterRF.csv") #~0.79
























