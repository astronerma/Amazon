setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

source("../Utils/submission_utils.R")

iteration = 10

library("e1071")
library("hash")
library("verification")

# Concatenate columns 2-10 in training set
new_train <- as.data.frame(matrix(NA, ncol=2,nrow=nrow(train)))
new_train[[1]] <- train[[1]]
new_train[[2]]<-apply(train,1,function(x) paste(x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],sep="-"))
#new_train[[2]] <- as.factor(new_train[[2]])

# Make the same concatenation for test set
new_test <- as.data.frame(matrix(NA, ncol=2,nrow=nrow(test)))
new_test[[1]] <- test[[1]]
new_test[[2]] <- apply(test,1,function(x) paste(x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],sep="-"))
#new_test[[2]] <- as.factor(new_test[[2]])

# Now do a pivot
mean_action <- aggregate(new_train[[1]],list(new_train[[2]]),mean)

m <- mean(mean_action[[2]])

# ----------------------------------
# Make a prediction on a test set!

prediction_train <- mean_action[match(new_train[[2]],mean_action[[1]]),2]
roc.area(train[[1]], prediction_train)

prediction_test <- mean_action[match(new_test[[2]],mean_action[[1]]),2]
sum(is.na(prediction_test))
prediction_test[is.na(prediction_test)] <- m

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


diffsub(9,10,2,"Amazon")

