library(class)
library(verification)

iteration = 3

setwd("/Users/aszostek/Projects/Kaggle/Amazon")
source("./Utils/utils.R")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Select samples for cross validation
set.seed(12345)
train_sample <- sample(nrow(train),0.7*nrow(train),replace=FALSE)
traincv <- train[train_sample,]
testcv <- train[-train_sample,]


condition_function <- function(row)
{
  if(            row$MGR_ID < 14000 &
                   row$ROLE_CODE<128000 &
                   row$RESOURCE < 150000 &
                   (row$ROLE_TITLE < 150000 | row$ROLE_TITLE>290000) &
                   row$ROLE_ROLLUP_2 < 135000 &
                   row$ROLE_ROLLUP_2 > 50000 &
                   row$ROLE_ROLLUP_1 < 122000 &
                   row$ROLE_ROLLUP_1 > 100000 &
                   (row$RESOURCE < 49000 | row$RESOURCE > 69000) &
                   (row$ROLE_ROLLUP_2 > 128000 | row$ROLE_ROLLUP_2 < 124000) & 
                   (row$MGR_ID < 9500 | row$MGR_ID > 13000) &
                   row$ROLE_FAMILY_DESC > 50000)
  {
  # Mixed area
  return(rbinom(1, 1, 0.5))
  }
  else
  {
  # Accepted 
  return(1)
  }
}

          
# -----------------------          
# Cross validation

# Entire training set
# OK, thsi method is not so great. Even on training set it sucks
v <- c()
for (i in 1:nrow(train))
{
  v<-append(v,condition_function(train[i,]))
}
roc.area(train[[1]], v)


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






