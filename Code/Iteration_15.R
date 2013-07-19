# Info about this idea:
# http://www.kaggle.com/c/amazon-employee-access-challenge/forums/t/5060/using-response-for-new-features

setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Remove redundant column
train <- train[,c(-10)]
test <- test[,c(-10)]


source("../Utils/submission_utils.R")

iteration = 15

library("randomForest")
library("tree")
library("e1071")
library("hash")
library("verification")
library("caret")

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







# ------------------------
# Take log10 of the training ata
log_train<-new_train
log_train[,2:17]<-log10(new_train[,2:17]+1e-3)
log_train[[1]] <- as.factor(log_train[[1]])

# ----------------------------------------
# Replace missing values in test set
new_test <- as.data.frame(impute(new_test,"median"))
# ------------------------
# Take log10 of the test
log_test <- new_test
log_test[,2:17]<-log10(new_test[,2:17]+1e-3)
log_test[[1]] <- as.factor(log_test[[1]])


# -------------------------------
# GLM

t2 <- glm(action ~.,data=log_train[,c(1,2,3,4,5,12,14,17)],family="binomial")
summary(t2)
predict_train <- predict(t2,type="response")
#predict_train
roc.area(train[[1]], predict_train)$A


kfold.glm<-function(data,k)
{
  n<-as.integer(nrow(data)/k)
  err.vect<-rep(NA,k)
  for (i in 1:k)
  {
    #print(i)
    s1<-((i-1)*n+1)
    s2<-(i*n)
    #print(paste(s1,s2))
    subset<-s1:s2
    
    train<-data[-subset,]
    test<-data[subset,]
    # ----------
    fit <- glm(action ~.,data=train[,c(1,2,3,4,5,12,14,17)],family="binomial")
    
    prediction <- predict(fit,newdata=test,type="response")
    # ----------
    labels<-as.numeric(as.character(test[[1]]))
    err <- roc.area(labels,prediction)$A
    err.vect[i]<-err
  }
  return(err.vect)
}

kfold.glm<-function(data,k)
{
  s<-sample(1:nrow(data),nrow(data),replace=FALSE)
  n<-as.integer(nrow(data)/k)
  err.vect<-rep(NA,k)
  for (i in 1:k)
  {
    s1<-((i-1)*n+1)
    s2<-(i*n)
    subset<-s[s1:s2]
    #print(subset[1:10])
    train<-data[-subset,]
    test<-data[subset,]
    fit <- glm(action ~.,data=train,family="binomial")
    prediction <- predict(fit,newdata=test,type="response")

    labels<-as.numeric(as.character(test[[1]]))
    err <- roc.area(labels,prediction)$A
    err.vect[i]<-err
  }
  return(err.vect)
}





a<-kfold.glm(log_train[,c(1,2,3,4,5,12,14,17)],5)
a

# ------------------------
# Plots
plot(jitter(log_train[[2]]),jitter(log_train[[14]]),col=log_train[[1]])




# ----------------
# Predict test set
prediction_test <- predict(t2,newdata = log_test[,c(1,2,3,4,5,12,14,17)],type="response")



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




