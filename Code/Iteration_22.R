# Info about this idea:
# http://www.kaggle.com/c/amazon-employee-access-challenge/forums/t/5060/using-response-for-new-features

setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Remove redundant column
#train <- train[,c(-10)]
#test <- test[,c(-10)]


source("../Utils/submission_utils.R")

iteration = 22

library("randomForest")
library("tree")
library("e1071")
library("hash")
library("verification")
library('glmnet')

# --------------------
# Unique resources
ures <- unique(train[[2]])

# Lets create a hash to store them
# One for each column
hash_c3 <- hash(ures,rep(1,length(ures)))
hash_c4 <- hash(ures,rep(1,length(ures)))
hash_c5 <- hash(ures,rep(1,length(ures)))
hash_c6 <- hash(ures,rep(1,length(ures)))
hash_c7 <- hash(ures,rep(1,length(ures)))
hash_c8 <- hash(ures,rep(1,length(ures)))
hash_c9 <- hash(ures,rep(1,length(ures)))
hash_c10 <- hash(ures,rep(1,length(ures)))

# for each column get the numbers of unique 
for(r in ures)
{
  hash_c3[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,3]))
  hash_c4[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,4]))
  hash_c5[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,5]))
  hash_c6[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,6]))
  hash_c7[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,7]))
  hash_c8[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,8]))
  hash_c9[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,9]))
  hash_c10[[as.character(r)]] <- length(unique(train[train$RESOURCE==r,10]))
}

# Lets create data frame to keep it all for later
new_features <- as.data.frame(matrix(NA,nrow=length(ures),ncol=9))
new_features[[1]] <- ures

i <- 1
for(r in ures)
{
  new_features[i,2] <- hash_c3[[as.character(r)]]
  new_features[i,3] <- hash_c4[[as.character(r)]]
  new_features[i,4] <- hash_c5[[as.character(r)]]
  new_features[i,5] <- hash_c6[[as.character(r)]]
  new_features[i,6] <- hash_c7[[as.character(r)]]
  new_features[i,7] <- hash_c8[[as.character(r)]]
  new_features[i,8] <- hash_c9[[as.character(r)]]
  new_features[i,9] <- hash_c10[[as.character(r)]]
  i <- i + 1
}
system("say done")


# -------------------------
# Now lets fill the training and test datasets

new_feature_sets <- function(train,test)
{
  
  # ---------------
  # My new datasets
  
  new_train <- as.data.frame(matrix(NA,nrow = nrow(train),ncol=24))
  names(new_train) <- c("action","resource0","resource1","mgr0","mgr1","role1_0","role1_1","role2_0", "role2_1", "dept0","dept1","title0","title1","family0","family1","role0","role1")
  new_train[[1]] <- train[[1]]
  
  new_test <- as.data.frame(matrix(NA,nrow = nrow(test),ncol=24))
  names(new_test) <- c("id","resource0","resource1","mgr0","mgr1","role1_0","role1_1","role2_0", "role2_1", "dept0","dept1","title0","title1","family0","family1","role0","role1")
  new_test[[1]] <- test[[1]]
  
  for(col in 2:ncol(train)) 
  {  
    #print(col)
    c0 <- 2*col-2
    c1 <- 2*col-1
    uni <- unique(train[[col]])
    uni_test <- unique(test[[col]])
    diff <- setdiff(uni_test,uni)
    
    slownik0 <- hash(uni,rep(0,length(uni)))
    slownik1 <- hash(uni,rep(0,length(uni))) 
    
    # populate slownik
    for (row in 1:nrow(train))
    {
      if (train[row,1] == 1)
      {   
        slownik1[[ as.character(train[row,col]) ]] <- slownik1[[ as.character(train[row,col]) ]] + 1
      }
      else
      {
        slownik0[[ as.character(train[row,col]) ]] <- slownik0[[ as.character(train[row,col]) ]] + 1
      }
    }  
    
    # populate new_train and new_test
    tmp <- as.data.frame(matrix(NA,nrow = length(uni),ncol=3))
    for (row in 1:length(uni))
    {
      #print(row)
      word <- uni[row]
      s0 <- slownik0[[as.character(word)]]
      s1 <- slownik1[[as.character(word)]]
      tmp[row,1] <- word
      tmp[row,2] <- s0/(s0+s1)
      tmp[row,3] <- s1/(s0+s1)
    }   
    
    new_train[[c0]] <- tmp[match(train[[col]],tmp[[1]]),2]
    new_train[[c1]] <- tmp[match(train[[col]],tmp[[1]]),3]
    new_test[[c0]] <- tmp[match(test[[col]],tmp[[1]],incomparables=diff,nomatch=NA),2]
    new_test[[c1]] <- tmp[match(test[[col]],tmp[[1]],incomparables=diff,nomatch=NA),3]  
    

    
    
    }
  
  new_test <- as.data.frame(impute(new_test,"median"))
  
  log_train<-new_train
  log_train[,2:17]<-log10(new_train[,2:17]+1e-3)
  log_train[[1]] <- as.factor(log_train[[1]])
  
  log_test <- new_test
  log_test[,2:17]<-log10(new_test[,2:17]+1e-3)
  log_test[[1]] <- as.factor(log_test[[1]])
  
  m <- match(train[[2]],new_features[[1]])
  new_train[[18]] <- new_features[m,2]
  new_train[[19]] <- new_features[m,3]
  new_train[[20]] <- new_features[m,4]
  new_train[[21]] <- new_features[m,5]
  new_train[[22]] <- new_features[m,6]
  new_train[[23]] <- new_features[m,7]
  new_train[[24]] <- new_features[m,8]
  
  m <- match(test[[2]],new_features[[1]])
  new_test[[18]] <- new_features[m,2]
  new_test[[19]] <- new_features[m,3]
  new_test[[20]] <- new_features[m,4]
  new_test[[21]] <- new_features[m,5]
  new_test[[22]] <- new_features[m,6]
  new_test[[23]] <- new_features[m,7]
  new_test[[24]] <- new_features[m,8]
  
  
  
  return(list(log_train,log_test))
  
} # end 


new_data <- new_feature_sets(train,test)



log_train <- new_data[[1]]
log_test <- new_data[[2]]


system("say done")

reshash

# -----------------------
# Submission file

id <- test[[1]]
prediction <- prediction_test
test_submission<-as.data.frame(matrix(data = NA, nrow = length(prediction),ncol=2))
test_submission[[1]] <- id
test_submission[[2]] <- prediction
names(test_submission)<-c("Id","Action")

# write file
submission_file_name = paste("./Submissions/submission",as.character(iteration),".csv",sep="")
submission_file_name

write.csv(test_submission,file=submission_file_name,row.names=FALSE,quote=FALSE)


diffsub(13,12,2,"Amazon")

# ---------------------
plot(log(new_train[[2]]+1e-7),log(new_train[[5]]+1e-7),col=new_train[[1]],pch=19)



