#!/usr/bin/env Rscript
library("R.utils")
library(e1071)

num <- cmdArgs()
file_num <- as.integer(num[[1]])
eps <- as.numeric(num[[2]])
range1 <- as.integer(num[[3]])
range2 <- as.integer(num[[4]])
num
eps
file_num
range1
range1

wage <- read.table("/scratch/general/lustre/u6007925/Grid/Data/Wage.txt", header = TRUE)
wage <- wage[complete.cases(wage), ]

# my own caret type function for a sparse grid. 
param <- NULL
for(i in -10:25) {
  param <- rbind(param, cbind(i, -25:10, eps, NA, NA, NA))
}

colnames(param) <- c("Cost", "Gamma", "Epsilon", "MSE", "MSE_UCL", "Time")

# Wage Results

param <- param[c(range1:range2), ]

for(i in 1:nrow(param)) {
  t1 <- Sys.time()
  if(!is.null(tryCatch({
    svm(RATE ~ ., data = wage, gamma = 2^param[i, 2], 
        cost = 2^param[i, 1], epsilon = param[i, 3], cross = 10)
  }, error=function(e){}))) {
    mod <- svm(RATE ~ ., data = wage, gamma = 2^param[i, 2], 
               cost = 2^param[i, 1], epsilon = param[i, 3], cross = 10)
    param[i, 4] <- mod$tot.MSE
    param[i, 5] <- mean(mod$MSE) + 1.96 * sd(mod$MSE) / sqrt(length(mod$MSE))
    
  }
  t2 <- Sys.time()
  param[i, 6] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- paste("/scratch/general/lustre/u6007925/Grid/SVM/Regression/Wage/Wage_Grid_", file_num, ".csv", sep = "")

write.csv(param, f1, row.names = FALSE)


