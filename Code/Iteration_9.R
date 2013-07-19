setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

source("../Utils/submission_utils.R")

iteration = 9

library("e1071")
library("hash")
library("verification")

# Concatenate columns 2-10 in training set
new_train <- as.data.frame(matrix(NA, ncol=2,nrow=nrow(train)))
new_train[[1]] <- train[[1]]
new_train[[2]]<-apply(train,1,function(x) paste(x[2],x[4],x[5],x[6],x[7],x[8],x[9],x[10],sep="-"))
new_train[[2]] <- as.factor(new_train[[2]])

# Make the same concatenation for test set
new_test <- as.data.frame(matrix(NA, ncol=2,nrow=nrow(test)))
new_test[[1]] <- test[[1]]
new_test[[2]]<-apply(test,1,function(x) paste(x[2],x[4],x[5],x[6],x[7],x[8],x[9],x[10],sep="-"))
new_test[[2]] <- as.factor(new_test[[2]])

# Now do a pivot
l <- length(levels(new_train[[2]]))
mean_action <- as.data.frame(matrix(NA, ncol=2,nrow=l))
mean_action[[1]] <- by(new_train[[1]],new_train[[2]],mean)
mean_action[[2]] <- levels(new_train[[2]])

# Mean value in the action column
m <- mean(mean_action[[1]])

# Prepare hash table for more efficient lookup
slownik <- hash()
for (i in 1:nrow(mean_action))
{
  slownik[[mean_action[i,2]]] <- mean_action[i,1]
}



# ---------------------------------
# Make a prediction on a training set!
# Just to get an upper limit
prediction_train <- c()
for (i in 1:nrow(new_train))
{
  print (i)
  f <- new_train[i,2]
  prediction_train <- append(prediction_train,slownik[[as.character(f)]][[1]])
}

roc.area(train[[1]], prediction_train)

# ----------------------------------
# Make a prediction on a test set!



k <- keys(slownik)
prediction_test <- rep(NA,nrow(test))
for (i in 1:nrow(new_test))
{
  print (i)
  f <- new_test[i,2]
  if (f %in% k)
  {
    prediction_test[i] <- slownik[[as.character(f)]][[1]]
  }
  else
  {
    # if factor not in slownik, use mean calculated above
    prediction_test[i] <- m
  }
}
system("say I am done")
# ----------------------------------
#prediction_test[is.na(prediction_test)]<-m

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


diffsub(4,6,2,"Amazon")

