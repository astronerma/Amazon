# Info about this idea:
# http://www.kaggle.com/c/amazon-employee-access-challenge/forums/t/5060/using-response-for-new-features

setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Remove redundant column
train <- train[,c(-10)]
test <- test[,c(-10)]


source("../Utils/submission_utils.R")

iteration = 21

library("randomForest")
library("tree")
library("e1071")
library("hash")
library("verification")
library('glmnet')


# -------------------------
# Now lets fill the training and test datasets

new_feature_sets <- function(train,test)
{
  
  # ---------------
  # My new datasets
  
  new_train <- as.data.frame(matrix(NA,nrow = nrow(train),ncol=25))
  names(new_train) <- c("action","resource0","resource1","mgr0","mgr1","role1_0","role1_1","role2_0", "role2_1", "dept0","dept1","title0","title1","family0","family1","role0","role1","Nresource","Nmgr","Nrole1","Nrole2","Ndept","Ntitle","Nfamily","Nrole")
  new_train[[1]] <- train[[1]]
  
  new_test <- as.data.frame(matrix(NA,nrow = nrow(test),ncol=25))
  names(new_test) <- c("id","resource0","resource1","mgr0","mgr1","role1_0","role1_1","role2_0", "role2_1", "dept0","dept1","title0","title1","family0","family1","role0","role1","Nresource","Nmgr","Nrole1","Nrole2","Ndept","Ntitle","Nfamily","Nrole")
  new_test[[1]] <- test[[1]]
  
  for(col in 2:ncol(train)) 
  {  
    #print(col)
    c0 <- 2*col-2
    c1 <- 2*col-1
    c2 <- 17+col-1
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
    tmp <- as.data.frame(matrix(NA,nrow = length(uni),ncol=4))
    for (row in 1:length(uni))
    {
      #print(row)
      word <- uni[row]
      s0 <- slownik0[[as.character(word)]]
      s1 <- slownik1[[as.character(word)]]
      tmp[row,1] <- word
      tmp[row,2] <- s0/(s0+s1)
      tmp[row,3] <- s1/(s0+s1)
      tmp[row,4] <- s0+s1
    }   
    
    new_train[[c0]] <- tmp[match(train[[col]],tmp[[1]]),2]
    new_train[[c1]] <- tmp[match(train[[col]],tmp[[1]]),3]
    new_train[[c2]] <- tmp[match(train[[col]],tmp[[1]]),4]
    
    new_test[[c0]] <- tmp[match(test[[col]],tmp[[1]],incomparables=diff,nomatch=NA),2]
    new_test[[c1]] <- tmp[match(test[[col]],tmp[[1]],incomparables=diff,nomatch=NA),3]
    new_test[[c2]] <- tmp[match(test[[col]],tmp[[1]],incomparables=diff,nomatch=NA),4]  
    
  }
  
  new_test <- as.data.frame(impute(new_test,"median"))
  
  log_train<-new_train
  log_train[,2:17]<-log10(new_train[,2:17]+1e-3)
  log_train[[1]] <- as.factor(log_train[[1]])
  
  log_test <- new_test
  log_test[,2:17]<-log10(new_test[,2:17]+1e-3)
  log_test[[1]] <- as.factor(log_test[[1]])
  
  return(list(log_train,log_test))
  
} # end 


system("say done")

new_data <- new_feature_sets(train,test)

log_train <- new_data[[1]]
log_test <- new_data[[2]]




# -------------------------------
# GLM

t2 <- glm(action ~.,data=log_train[,c(1,2,3,4,5,12,14,17,20)],family="binomial",epsilon=1e-9)
summary(t2)
predict_train <- predict(t2,type="response")
#predict_train
roc.area(train[[1]], predict_train)$A

# -------------------------------
# SVM

t1 <- svm(action ~.,data=log_train[,c(1,2,3,4,5,12,14,17)],kernel="radial")
summary(t1)
predict_train <- as.numeric(as.character(predict(t1,type="response",probability=T)))
predict_train
roc.area(train[[1]], predict_train)$A

# -------------------------------
# tree

t3 <- tree(action ~.,data=log_train[,c(1,2,3,4,5,12,14,17)])
summary(t3)
#predict_train <- as.numeric(as.character(predict(t3,type="class")))
predict_train <- predict(t3,type="vector")[,2]
predict_train
roc.area(train[[1]], predict_train)$A

# -------------------------------
# glmnet

t4 <- glmnet(as.matrix(log_train[,2:17]),log_train[[1]],family="binomial")
plot(t4,xvar="dev")
summary(t4)
t4cv<-cv.glmnet(as.matrix(log_train[,2:17]),log_train[[1]],family="binomial",type.measure="auc")
plot(t4cv)


#predict_train <- as.numeric(as.character(predict(t3,type="class")))
predict_train <- predict(t4,type="vector")[,2]
predict_train
roc.area(train[[1]], predict_train)$A



# --------------------
# - Cross validation -

kfold.glm<-function(data,k,cols)
{
  s<-sample(1:nrow(data),nrow(data),replace=FALSE)
  n<-as.integer(nrow(data)/k)
  err.vect<-rep(NA,k)
  for (i in 1:k)
  {
    print (i)
    print(cols)
    # Prepare new training and test set
    s1<-((i-1)*n+1)
    s2<-(i*n)
    subset<-s[s1:s2]
    #print(subset[1:10])
    train<-data[-subset,]
    test<-data[subset,]
    
    # Generate new features
    new_data <- new_feature_sets(train,test)
    log_train <- new_data[[1]]
    log_test <- new_data[[2]]
    
    # Make a fit
    fit <- glm(action ~.,data=log_train[,cols],family="binomial",epsilon=1e-9,maxit=35)
    prediction <- predict(fit,newdata=log_test[,cols],type="response")
    
    #fit <- tree(action ~.,data=log_train[,cols])
    #prediction <- predict(fit,newdata=log_test[,cols],type="vector")[,2]
    
    labels <- as.numeric(as.character(log_test[[1]]))
    err <- roc.area(labels,prediction)$A
    err.vect[i]<-err
  }
  return(err.vect)
}

a<-kfold.glm(train,5,c(1,2,3,4,5,12,14,17,20))
a
system("say done")



# -----------------------------
kfold.glmnet<-function(data,k,cols,lambda)
{
  s<-sample(1:nrow(data),nrow(data),replace=FALSE)
  n<-as.integer(nrow(data)/k)
  err.vect<-rep(NA,k)
  for (i in 1:k)
  {
    print (i)
    #print(cols)
    # Prepare new training and test set
    s1<-((i-1)*n+1)
    s2<-(i*n)
    subset<-s[s1:s2]
    train<-data[-subset,]
    test<-data[subset,]
    
    # Generate new features
    new_data <- new_feature_sets(train,test)
    log_train <- new_data[[1]]
    log_test <- new_data[[2]]
    
    # Make a fit
    fit <- glm(action ~.,data=log_train[,cols],family="binomial")
    prediction <- predict(fit,newdata=log_test[,cols],type="response")
    
    #fit <- glmnet(as.matrix(log_train[,2:17]),log_train[[1]],family="binomial")
    #prediction <- as.numeric(predict(fit, as.matrix(log_test[,2:17]) ,type="link",s=lambda))
        
    labels <- as.numeric(as.character(log_test[[1]]))
    err <- roc.area(labels,prediction)$A
    err.vect[i]<-err
  }
  return(err.vect)
}

b<-rep(NA,5)
b[1] <- mean(kfold.glmnet(train,3,1:17,0.1))
b[2] <- mean(kfold.glmnet(train,3,1:17,0.07))
b[3] <- mean(kfold.glmnet(train,3,1:17,0.05))
b[4] <- mean(kfold.glmnet(train,3,1:17,0.03))
b[5] <- mean(kfold.glmnet(train,3,1:17,0.01))
b[6] <- mean(kfold.glmnet(train,3,1:17,0.005))
system("say done")

b


a<-kfold.glmnet(train,3,1:17,1e-6)

a

b[1]
b[2]
b[3]

# ------------------------
# Plots
plot(jitter(log_train[[3]]),jitter(log_train[[5]]),col=log_train[[1]])

plot(jitter(log_train[[2]]**2),jitter(log_train[[14]]**2),col=log_train[[1]])


plot(jitter(log_train[[2]]),jitter(log_train[[16]]**2),col=log_train[[1]])

#,xlim=c(-1,0.05),ylim=c(-1,0.05)
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



