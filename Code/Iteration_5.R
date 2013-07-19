setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

# Select samples for cross validation
set.seed(12345)
train_sample <- sample(nrow(train),0.7*nrow(train),replace=FALSE)
traincv <- train[train_sample,]
testcv <- train[-train_sample,]

library(tree)


# -----------------------------------------------------------------
# If in the training set there is a resource that shows only once
# then if it shows up in test set, then we set it to the value that 
# it had in training set - simple!
# The parameter is a string!
single_resource<-function(nb)
{
  return(train[train$RESOURCE==nb,1])
}

# -----------------------------------------------------------------
# Actions for given resource
actions<-function(nb)
{
  return(train[train$RESOURCE==nb,1])
}


train[[1]] <- as.factor(train[[1]])
train[[2]] <- as.numeric(train[[2]])
train[[3]] <- as.numeric(train[[3]])
train[[4]] <- as.numeric(train[[4]])
train[[5]] <- as.numeric(train[[5]])
train[[6]] <- as.numeric(train[[6]])
train[[7]] <- as.numeric(train[[7]])
train[[8]] <- as.numeric(train[[8]])
train[[9]] <- as.numeric(train[[9]])
train[[10]] <- as.numeric(train[[10]])


t1 <- tree(ACTION~.,data=train)
summary(t1)

tb <- table(train[[2]])
a<-as.data.frame(tb)
a<-a[order(a[[2]],decreasing=T),]
names(a)<-c("res","freq")
b1<-as.vector(a[a$freq==1,1])
b2<-as.vector(a[a$freq==2,1])
b3<-as.vector(a[a$freq==3,1])
b4<-as.vector(a[a$freq==4,1])
b100<-as.vector(a[a$freq>100,1])



length(b1)
length(b2)
length(b3)
length(b4)
length(b100)
length(b1)+length(b2)+length(b3)+length(b4)
nrow(train)

b2
actions("111911")
v = c()
for (v in b2)
{
  v <- append(v,)
  
}

d<-train[train$RESOURCE %in% b100,]

t1<-tree(RESOURCES ~ .,data=d)

