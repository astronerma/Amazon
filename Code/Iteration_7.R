setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

source("../Utils/submission_utils.R")

iteration = 7

library("e1071")
library("hash")
library("verification")

# Transform the data
new_train <- train
new_test <- test

transform_column<-function(sufix,column,dataframe)
{
  dataframe[[column]]<-lapply(dataframe[[column]],function(x) paste(as.character(x),sufix,sep=""))
  return(dataframe)
}

new_train<-transform_column("c2",2,new_train)
new_train<-transform_column("c3",3,new_train)
new_train<-transform_column("c4",4,new_train)
new_train<-transform_column("c5",5,new_train)
new_train<-transform_column("c6",6,new_train)
new_train<-transform_column("c7",7,new_train)
new_train<-transform_column("c8",8,new_train)
new_train<-transform_column("c9",9,new_train)
new_train<-transform_column("c10",10,new_train)

new_test<-transform_column("c2",2,new_test)
new_test<-transform_column("c3",3,new_test)
new_test<-transform_column("c4",4,new_test)
new_test<-transform_column("c5",5,new_test)
new_test<-transform_column("c6",6,new_test)
new_test<-transform_column("c7",7,new_test)
new_test<-transform_column("c8",8,new_test)
new_test<-transform_column("c9",9,new_test)
new_test<-transform_column("c10",10,new_test)

# Train and test combined
all <- rbind(new_train[,2:10],new_test[,2:10])



# ----------------------------------
# Initiate the dictionary with all 0

slownik0 <- hash()
slownik1 <- hash()

uniquew <- c(
  unique(all[[2]]),
  unique(all[[3]]),
  unique(all[[4]]),
  unique(all[[5]]),
  unique(all[[6]]),
  unique(all[[7]]),
  unique(all[[8]]),
  unique(all[[9]]),
  unique(all[[1]])
)

for (word in uniquew)
{
  slownik0[[word]] <- 0
  slownik1[[word]] <- 0
}

# ------------------------------
# Fill the dictionary - Calculate word frequency dictionary
# It will give me the numerator for the Naive Bayes equation

w <- c(NA,NA)
w[1] <- 0
w[2] <- 0

for (col in 2:ncol(new_train))
{
  for (row in 1:nrow(new_train))
  {
    word <- new_train[row,col][[1]] 
    if (new_train[row,1] == as.integer(0))
    {
      slownik0[[word]] <- slownik0[[word]] + 1
      w[1] <- w[1] + 1
    }
    else
    {
      slownik1[[word]] <- slownik1[[word]] + 1
      w[2] <- w[2] + 1
    }
  }
}

# Little test
slownik0[["39353c2"]]
slownik1[["39353c2"]]

# ------------------------------------------------------
# Note: The denominator is sum all words in given class, even repeated!
# in my case it is a value w.
# I Laplace smoothing, we add 1 to numerator and 1 to each unique word in denominator
# but we add one irrespectively if it is in the given class or not!
# So in practice we add the size of our vocabulary, i.e. number of unique words
# in all our classes but not yet in test set!

# Calculating probabilities
p <- c(NA,NA)
# Probability of action 0 and 1
p[1] <- nrow(train[train$ACTION==0,])/nrow(train)
p[2] <- nrow(train[train$ACTION==1,])/nrow(train)

# Length of vocabulary in training set!
vocabulary <- function(dataframe,columns)
{
  s <- 0
  for( col in columns)
  {
    s <- s + length(unique(dataframe[[col]]))
  }
  return(s)
}

# ---------------------------------------
# With Laplace smoothing
# This is only for training set!
# Tuining parameters
alpha <- 1
beta <- 10000
pp_word<-function(list_of_words,action)
{
  V <- vocabulary(new_train,2:10)
  mult <- 0
  if(action == 0)
  {
    for (word in list_of_words)
    {
      mult <- mult + log( (slownik0[[word]]+alpha) / ( w[action+1] + beta ) )
    }
  }
  else
  {
    for (word in list_of_words)
    {
      mult <- mult + log( (slownik1[[word]]+alpha) / ( w[action+1] + beta ) )
    }
  }
  return(log(p[action+1])+mult)
}

# A little test
row = 69
new_train[row,]
l <- unlist(unname(new_train[row,2:ncol(new_train)]))
pp_word(l,0)
pp_word(l,1)

# ----------------------------------------
# Calculate probabilities for both classes

t <- as.data.frame(matrix(NA,nrow=nrow(new_train),ncol=2))
for (row in 1:nrow(new_train))
{
  print (row)
  l <- unlist(unname(new_train[row,2:ncol(new_train)]))
  t[row,1]<-pp_word(l,0)
  t[row,2]<-pp_word(l,1)
}
system("say i am done")

# ---------------------------------
# Make prediction for training set
t0 <- c()
for (i in 1:nrow(t))
{
  if(t[i,1]>t[i,2])
  {
    # action 0
    t0[i] <- 0
  }
  else
  {
    # action 1
    t0[i] <- 1
  }
}

roc.area(train[[1]], t0)

# -------------------------
# Prediction for test set
# It takes into account that there may be some words which were not in the distionary
# When a new word shows up when working with test set
# when we encounter this word, we include it in our vocabulary and we increase its
# size (just once) by 1

predtest <- as.data.frame(matrix(NA,nrow=nrow(new_test),ncol=2))
for (row in 1:nrow(new_test))
{
  print (row)
  l <- unlist(unname(new_test[row,2:ncol(new_test)]))
  predtest[row,1]<-pp_word(l,0)
  predtest[row,2]<-pp_word(l,1)
}

system("say i am done calculating")

s0 <- c()
for (i in 1:nrow(predtest))
{
  if(predtest[i,1]>predtest[i,2])
  {
    # action 0
    s0[i] <- 0
  }
  else
  {
    # action 1
    s0[i] <- 1
  }
}

# -----------------------
# Submission file

id <- test[[1]]
prediction <- s0
test_submission<-as.data.frame(matrix(data = NA, nrow = length(prediction),ncol=2))
test_submission[[1]] <- id
test_submission[[2]] <- prediction
names(test_submission)<-c("Id","Action")

# write file
submission_file_name = paste("./Submissions/submission",as.character(iteration),".csv",sep="")
submission_file_name

write.csv(test_submission,file=submission_file_name,row.names=FALSE,quote=FALSE)


diffsub(4,6,2,"Amazon")

# -----------
# Tests

funique <- function(dataframe,columns)
{
  s <- 0
  for( col in columns)
  {
    s <- s + length(unique(dataframe[[col]]))
  }
  return(s)
}

funique(train,2:10)
funique(new_train,2:10)
funique(new_test,2:10)
funique(test,2:10)
funique(all,1:9)
length(keys(vocabulary))



