# Info about this idea:
# http://www.kaggle.com/c/amazon-employee-access-challenge/forums/t/5060/using-response-for-new-features

setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Remove redundant column
train <- train[,c(-10)]
test <- test[,c(-10)]


source("../Utils/submission_utils.R")

iteration = 13

library("tree")
library("e1071")
library("hash")
library("verification")
library('Matrix')

# ---------------
# My new datasets

new_train <- as.data.frame(matrix(NA,nrow = nrow(train),ncol=17))
names(new_train) <- c("action","resource0","resource1","mgr0","mgr1","role1_0","role1_1","role2_0", "role2_1", "dept0","dept1","title0","title1","family0","family1","role0","role1")
new_train[[1]] <- train[[1]]


new_test <- as.data.frame(matrix(NA,nrow = nrow(test),ncol=17))
names(new_test) <- c("id","resource0","resource1","mgr0","mgr1","role1_0","role1_1","role2_0", "role2_1", "dept0","dept1","title0","title1","family0","family1","role0","role1")
new_test[[1]] <- test[[1]]

# -------------------------
# Now lets fill the training and test datasets

for (col in 2:9)
{
    print(col)
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
    
    # populate new_train
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
system("say done")







# -------------------
# Lets train a tree :)

t1 <- tree(action ~.,data=new_train)
plot(t1)
text(t1)
summary(t1)
predict_train <- predict(t1)

roc.area(train[[1]], predict_train)$A

# -----------------------------------------
# Now, we need some cross validation here!

cv1 <- cv.tree(t1,,prune.tree,K=15)
par(mfrow=c(1,1))
plot(cv1)
par(mfrow=c(1,2))
plot(cv1$dev)
plot(cv1$k)


kfold.tree<-function(data,k,prune)
{
  n<-as.integer(nrow(data)/k)
  err.vect<-rep(NA,k)
  for (i in 1:k)
  {
    subset<-((i-1)*n+1):(i*n)
    train<-data[-subset,]
    test<-data[subset,]
    treepred<-tree(action~.,data=train)
    treepred <- prune.tree(treepred,best=prune)
    predict_prune <- predict(treepred,newdata=test)
    err<-roc.area(test[[1]],predict_prune)$A
    err.vect[i]<-err
  }
  return(err.vect)
}

k<-kfold.tree(new_train,7,6)
k
mean(k)


t1 <- tree(action ~.,data=new_train)
treepred <- prune.tree(t1,best = 6)
prediction_test <- predict(treepred,newdata = new_test)



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

