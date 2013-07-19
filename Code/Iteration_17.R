setwd("/Users/aszostek/Projects/Kaggle/Amazon")
test <- read.csv(file="./Data/test.csv")

iteration = 17

subs <- as.data.frame(matrix(NA,ncol=5,nrow=nrow(test)))
names(subs) <- c("s10","s12","s13","s14","s15")

subs$s10 <- read.csv(file="./Submissions/submission10.csv")[[2]]
subs$s12 <- read.csv(file="./Submissions/submission12.csv")[[2]]
subs$s13 <- read.csv(file="./Submissions/submission13.csv")[[2]]
subs$s14 <- read.csv(file="./Submissions/submission14.csv")[[2]]
subs$s15 <- read.csv(file="./Submissions/submission15.csv")[[2]]



subs2 <- subs[,-2]
subs2[subs[[1]] >= 0.5,1] <- 1
subs2[subs[[1]] < 0.5,1] <- 0
subs2[subs[[3]] >= 0.5,2] <- 1
subs2[subs[[3]] < 0.5,2] <- 0
subs2[[3]] <- subs[[4]]
subs2[subs[[5]] >= 0.5,4] <- 1
subs2[subs[[5]] < 0.5,4] <- 0


prediction_test<-apply(subs2,1,mean)


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
