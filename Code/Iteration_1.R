library(class)

iteration = 1

setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Select samples for cross validation
set.seed(12345)
train_sample <- sample(nrow(train),0.7*nrow(train),replace=FALSE)
traincv <- train[train_sample,]
testcv <- train[-train_sample,]


# Train k-nearest neighbour
k1 <- knn(traincv[,c(-1)],testcv[,c(-1)],traincv[[1]],k=2)
sum(k1==testcv[[1]])/nrow(testcv)

cv1<-knn.cv(traincv[,c(-1)],traincv[[1]],k=1)
sum(cv1==traincv[[1]])/nrow(traincv)


# Train final model on entire training set
t1 <- knn(train[,c(-1)],test[,c(-1)],train[[1]],k=1)


# -----------------------
# Submission file

id <- test[[1]]
prediction <- t1
test_submission<-as.data.frame(matrix(data = NA, nrow = length(prediction),ncol=2))
test_submission[[1]] <- id
test_submission[[2]] <- prediction
names(test_submission)<-c("Id","Action")

# write file
submission_file_name = paste("./Submissions/submission",as.character(iteration),".csv",sep="")
submission_file_name

write.csv(test_submission,file=submission_file_name,row.names=FALSE,quote=FALSE)



