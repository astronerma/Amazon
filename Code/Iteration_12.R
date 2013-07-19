# Info about glmnet:
# http://www.moseslab.csb.utoronto.ca/alan/glmnet_presentation.pdf
# http://www.stanford.edu/~hastie/TALKS/
# http://www.youtube.com/watch?v=BU2gjoLPfDc
# Examples in the video above start around 30 min in



setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Remove redundant column
train <- train[,c(-10)]
test <- test[,c(-10)]


source("../Utils/submission_utils.R")

iteration = 12

library("e1071")
library("hash")
library("verification")
library('Matrix')
library('glmnet')

# Lets first transform the data to later be able to create the sparse matrix

new_train <- train
new_test <- test

# Rename each variable
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

new_test<-transform_column("c2",2,new_test)
new_test<-transform_column("c3",3,new_test)
new_test<-transform_column("c4",4,new_test)
new_test<-transform_column("c5",5,new_test)
new_test<-transform_column("c6",6,new_test)
new_test<-transform_column("c7",7,new_test)
new_test<-transform_column("c8",8,new_test)
new_test<-transform_column("c9",9,new_test)

# Train and test combined
all <- rbind(new_train[,2:9],new_test[,2:9])


slownik <- hash()

uniquew <- c(
  unique(all[[1]]),
  unique(all[[2]]),
  unique(all[[3]]),
  unique(all[[4]]),
  unique(all[[5]]),
  unique(all[[6]]),
  unique(all[[7]]),
  unique(all[[8]])
)

# Slownik of all unique codes each keeps its corresponding collumn number!
i<-1
for (word in uniquew)
{
    print (i)
    slownik[[word]] <- i
    i <- i + 1
}

keymax <- length(keys(slownik))
# Lets put this slownik into a data frame 
# to be able to do a match 

data_slownik <- as.data.frame(matrix(NA,ncol=2,nrow=keymax))
k <- keys(slownik)
for (i in k)
{
  row <- slownik[[i]]
  data_slownik[row,1] <- row
  data_slownik[row,2] <- i
}

trans_train <- new_train
trans_train[[2]] <- data_slownik[match(new_train[[2]],data_slownik[[2]]),1]
trans_train[[3]] <- data_slownik[match(new_train[[3]],data_slownik[[2]]),1]
trans_train[[4]] <- data_slownik[match(new_train[[4]],data_slownik[[2]]),1]
trans_train[[5]] <- data_slownik[match(new_train[[5]],data_slownik[[2]]),1]
trans_train[[6]] <- data_slownik[match(new_train[[6]],data_slownik[[2]]),1]
trans_train[[7]] <- data_slownik[match(new_train[[7]],data_slownik[[2]]),1]
trans_train[[8]] <- data_slownik[match(new_train[[8]],data_slownik[[2]]),1]
trans_train[[9]] <- data_slownik[match(new_train[[9]],data_slownik[[2]]),1]

# Do the same for test data
trans_test <- new_test
trans_test[[2]] <- data_slownik[match(new_test[[2]],data_slownik[[2]]),1]
trans_test[[3]] <- data_slownik[match(new_test[[3]],data_slownik[[2]]),1]
trans_test[[4]] <- data_slownik[match(new_test[[4]],data_slownik[[2]]),1]
trans_test[[5]] <- data_slownik[match(new_test[[5]],data_slownik[[2]]),1]
trans_test[[6]] <- data_slownik[match(new_test[[6]],data_slownik[[2]]),1]
trans_test[[7]] <- data_slownik[match(new_test[[7]],data_slownik[[2]]),1]
trans_test[[8]] <- data_slownik[match(new_test[[8]],data_slownik[[2]]),1]
trans_test[[9]] <- data_slownik[match(new_test[[9]],data_slownik[[2]]),1]


# Make a sparse matrix
makeSparse <- function(dataframe)
{
  xrows <- rep(1:nrow(dataframe),each=8)
  ycol <- c()
  i <- 1
  for (row in 1:nrow(dataframe))
  {
    print (i)  
    ycol <- append(ycol,unname(unlist(dataframe[row,2:9])))
    
    i <- i + 1
  }
  # I set here dimmentions to make sure that my train and test matrices are of the same 
  # number of variables.
  # If they are not, the predict.glmnet crashes
  return(sparseMatrix(xrows,ycol,dims=c(nrow(dataframe),length(keys(slownik)))))
}

Ms <- makeSparse(trans_train)
Ms_test <- makeSparse(trans_test)
dim(Ms_test)
dim(Ms)


# ------------------------------------------
# Now run the glmnet model <- first run just 

fit1<-glmnet(Ms,train[[1]],family="binomial")
plot(fit1,xvar="dev")

system("say done")

# Cross validation to get the right lambda
# the best lambda is given by $lambda_min
fitcv<-cv.glmnet(Ms,train[[1]],family="binomial",type.measure="auc")
plot(fitcv)
lambdamin<-fitcv$lambda.min


# Prediction for training set for best lambda see how it changes for different types
prediction_train<-as.numeric(predict(fit1,Ms,type="response",s=c(lambdamin)))
roc.area(train[[1]], prediction_train)$A

prediction_train<-as.numeric(predict(fit1,Ms,type="class",s=c(lambdamin)))
roc.area(train[[1]], prediction_train)$A


# ---------------
# Predict test set
prediction_test<-predict(fit1,Ms_test,type="response",s=c(lambdamin))

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


diffsub(10,11,2,"Amazon")

