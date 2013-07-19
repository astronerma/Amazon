setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

source("../Utils/submission_utils.R")

iteration = 4

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

all <- rbind(new_train[,2:10],new_test[,2:10])

# Prepare bag of words

#bag <- as.data.frame(matrix(rep(NA,nrow(all)),nrow=nrow(all),ncol=1))
#names(bag)<-c("action","words")
#bag[[1]] <- apply(all[,2:ncol(all)],1,function(x) as.vector(x))
#bag[[1]] <- lapply(bag[[2]],function(x) unname(unlist(x)))
#bag[[1]] <- lapply(bag[[2]],function(x) x[1])

# Calculating probabilities

n <-c(NA,NA)
p <- c(NA,NA)
w <- c(NA,NA)
# Probability of action 0
p[1] <- nrow(train[train$ACTION==0,])/nrow(train)
n[1] <- nrow(train[train$ACTION==0,])


# Probability of action 1
p[2] <- nrow(train[train$ACTION==1,])/nrow(train)
n[2] <- nrow(train[train$ACTION==1,])

# Unique words
#uwords<-unique(unlist(bag$words))

# word frequency dictionary
slownik0 <- hash()
slownik1 <- hash()
vocabulary <- hash()


for (col in 1:ncol(all))
{
  for (row in 1:nrow(all))
  {
    word <- all[row,col][[1]]
    slownik0[[word]] <- 0
    slownik1[[word]] <- 0
    vocabulary[[word]] <- 0
  }
}


# Fill the dictionary
w[1] <- 0
w[2] <- 0
for (col in 2:ncol(new_train))
{
 for (row in 1:nrow(new_train))
 {
   word <- new_train[row,col][[1]]
 
  #print(word)
  # increment count appropriately 
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



slownik0[["39353c2"]]
slownik1[["39353c2"]]

#pp_table <- as.data.frame(matrix(rep(NA,length(uwords)),nrow=length(uwords),ncol=3))
#names(pp_table) <- c("word","action0","action1")

# With Laplace smoothing

pp_word<-function(list_of_words,action)
{
    V <- length(vocabulary)
    mult <- 0
    if(action == 0)
    {
      for (word in list_of_words)
      {
          mult <- mult + log( (slownik0[[word]]+1) / (w[action+1]+V) )
        
      }
    }
    else
    {
      for (word in list_of_words)
      {
          mult <- mult + log( (slownik1[[word]]+1) / (w[action+1]+V) )
      }
    }
    return(log(p[action+1])+mult)
}

row = 1500
l <- unlist(unname(new_train[row,2:ncol(new_train)]))
pp_word(l,0)
pp_word(l,1)

t <- as.data.frame(matrix(NA,nrow=nrow(new_train),ncol=2))
for (row in 1:nrow(train))
{
  l <- unlist(unname(new_train[row,2:ncol(new_train)]))
  t[row,1]<-pp_word(l,0)
  t[row,2]<-pp_word(l,1)
}

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

predtest <- as.data.frame(matrix(NA,nrow=nrow(new_test),ncol=2))
for (row in 1:nrow(new_test))
{
  l <- unlist(unname(new_test[row,2:ncol(new_test)]))
  predtest[row,1]<-pp_word(l,0)
  predtest[row,2]<-pp_word(l,1)
}


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

source("../Utils/submission_utils.R")
diffsub(1,4,2,"Amazon")
