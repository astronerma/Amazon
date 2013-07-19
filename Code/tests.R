setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
true <- train[[1]]


library(class)
library(verification)


# Lets look at my rule based prediction from iteration 3

rest <- subset(train,
               train$MGR_ID < 14000 &
                 train$ROLE_CODE<128000 &
                 train$RESOURCE < 150000 &
                 (train$ROLE_TITLE < 150000 | train$ROLE_TITLE>290000) &
                 train$ROLE_ROLLUP_2 < 135000 &
                 train$ROLE_ROLLUP_2 > 50000 &
                 train$ROLE_ROLLUP_1 < 122000 &
                 train$ROLE_ROLLUP_1 > 100000 &
                 (train$RESOURCE < 49000 | train$RESOURCE > 69000) &
                 (train$ROLE_ROLLUP_2 > 128000 | train$ROLE_ROLLUP_2 < 124000) & 
                 (train$MGR_ID < 9500 | train$MGR_ID > 13000) &
                 train$ROLE_FAMILY_DESC > 50000
)

table(rest[[1]])
table(train[[1]])

plot(train)


prediction2 <-list()
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
    #return(rbinom(1, 1, 0.5))
    prediction2 <- append(prediction2,row)
    return(sample(0:1,1))
  }
  else
  {
    # Accepted 
    #return(sample(0:1,1))
    return(1)
  }
}

# ----------------------------------
# Make a prediction on a traing set 
prediction <- c()
for (i in 1:nrow(train))
{
  prediction<-append(prediction,condition_function(train[i,]))
}

a<-as.data.frame(prediction2)



# ----------------------------------
# This is a built in roc curve area function
roc.area(true, prediction)$A
roc.plot(true, prediction)

# ------------------------------
# Can I reproduce this result?

area <- function(tpf,fpf)
{
  a1 <- 0.5*fpf*tpf
  a2 <- (1-fpf)*tpf
  a3 <- 0.5*(1-fpf)*(1-tpf)
  return(a1+a2+a3)
}

# True Positive Fraction
tpf <- sum(prediction == 1 & true == 1)/sum(true == 1)
# False positive Fraction
fpf <- sum(prediction == 1 & true == 0)/sum(true == 0)
tpf
fpf
d <- data.frame(rbind(c(0,0),c(tpf,fpf),c(1,1)))
plot(d[[2]],d[[1]],type="l",xlab="False positive fraction",ylab="True positive fraction")
lines(c(0,1),c(0,1),col="red")

area(tpf,fpf)

# -----------------------------------------
# Random prediction
prediction <- sample(0:1,nrow(train),replace=TRUE)
# True Positive Fraction
tpf <- sum(prediction == 1 & true == 1)/sum(true == 1)
# False positive Fraction
fpf <- sum(prediction == 1 & true == 0)/sum(true == 0)
tpf
fpf

# -----------------------------------------
# Other
# Lets assume that we can recognize half of cases as 1s true positive
# And the rest of points is assigned randomly between 0 and 1
tpf <- 70/100
fpf <- 0/4
area(tpf,fpf)
d <- data.frame(rbind(c(0,0),c(tpf,fpf),c(1,1)))
plot(d[[2]],d[[1]],type="l",xlab="False positive fraction",ylab="True positive fraction")
lines(c(0,1),c(0,1),col="red")




