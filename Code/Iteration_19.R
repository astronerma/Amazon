setwd("/Users/aszostek/Projects/Kaggle/Amazon")
test <- read.csv(file="./Data/test.csv")

iteration = 19

subs <- as.data.frame(matrix(NA,ncol=5,nrow=nrow(test)))
names(subs) <- c("s10","s12","s13","s15","s18")

subs$s10 <- read.csv(file="./Submissions/submission10.csv")[[2]]
subs$s12 <- read.csv(file="./Submissions/submission12.csv")[[2]]
subs$s13 <- read.csv(file="./Submissions/submission13.csv")[[2]]
subs$s15 <- read.csv(file="./Submissions/submission15.csv")[[2]]
subs$s18 <- read.csv(file="./Submissions/submission18.csv")[[2]]

weights <- c(0.82459,0.86179,0.78829,0.83817,0.88146) 

prediction_test<-apply(subs,1,function(x) weighted.mean(x,weights))

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
