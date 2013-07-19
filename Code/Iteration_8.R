setwd("/Users/aszostek/Projects/Kaggle/Amazon")
train <- read.csv(file="./Data/train.csv")
test <- read.csv(file="./Data/test.csv")

source("../Utils/submission_utils.R")

iteration = 8

library(rattle)
rattle()