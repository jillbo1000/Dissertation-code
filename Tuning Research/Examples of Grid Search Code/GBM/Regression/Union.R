#!/usr/bin/env Rscript
library("R.utils")
library(gbm)

num <- cmdArgs()
file_num <- as.integer(num[[1]])
range1 <- as.integer(num[[2]])
range2 <- as.integer(num[[3]])
num
file_num
range1
range1

union <- read.table("/scratch/general/lustre/u6007925/Grid/Data/Union.txt", header = TRUE)
union <- union[, 2:5]

# source in cv.pred function
source("/scratch/general/lustre/u6007925/Grid/Functions/GBMreg-cvpred.R")

# my own caret type function for a sparse grid. 
p <- NULL
for(i in 1:40) {
  p <- rbind(p, cbind(i * 50, 5:12))
}

p2 <- NULL
for(i in 1:3) {
  p2 <- rbind(p2, cbind(p, -i))
}
head(p2)
tail(p2)

param <- NULL
for(i in 1:10) {
  param <- rbind(param, cbind(p2, (2 * i - 1), NA, NA, NA))
}

colnames(param) <- c("NumTrees", "MinNode", "Shrinkage", "IntDepth", "MSE", "MSE.UCL", "Time")

# Crime Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  pr <- NULL
  try(pr <- cv.pred(union[, -1], union[, 1], fold = 10, tr = param[i, 1], 
                    id = param[i, 4], nmin = param[i, 2], shr = param[i, 3]))
  
  if(!is.null(pr)) {
    param[i, 5] <- pr$mse
    param[i, 6] <- mean(pr$cv.mse) + 1.96 * sd(pr$cv.mse) / sqrt(length(pr$cv.mse))
  }
  t2 <- Sys.time()
  param[i, 7] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- paste("/scratch/general/lustre/u6007925/Grid/GBM/Regression/Union/Union_Small_Grid_", file_num, ".csv", sep = "")

write.csv(param, f1, row.names = FALSE)


