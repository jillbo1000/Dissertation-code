#!/usr/bin/env Rscript
library("R.utils")
library(EZtune)
library(rpart)

num = cmdArgs()
data_name <- as.character(num[[1]])
file_num <- as.numeric(num[[2]])
method <- as.character(num[[3]])
optimizer <- as.character(num[[4]])
fast <- as.character(num[[5]])
cross <- as.character(num[[6]])

# data_name <- "Pima"
# file_num <- 10
# method <- "ada"
# optimizer <- "hjn"
# fast <- 0.25
# cross <- NULL


source("get_data.R")

results <- data.frame(data = rep(data_name, 10))
results$method <- method
results$optimizer <- optimizer
results$fast <- fast
results$cross <- cross
results$performance <- 0
results$cv_perf <- 0
results$time <- 0

fast <- convert(fast)
cross <- convert(cross)

param <- parameters(y, method)


for(i in 1:10) {
  t1 <- Sys.time()
  tmp <- eztune(x, y, method = method, optimizer = optimizer, fast = fast, cross = cross)
  param[i, ] <- param_fill(y, method, tmp)
  if(length(unique(y)) == 2) {
    results$performance[i] <- tmp$accuracy
  } else {
    results$performance[i] <- tmp$mse
  }
  t2 <- Sys.time()
  results$cv_perf[i] <- eztune_cv(x, y, tmp)
  results$time[i] <- as.numeric(t2 - t1, units = "secs")

}


final <- cbind(results[, -c(6:8)], param, results[, 6:8])

direct <- paste("/scratch/general/lustre/u6007925/Grid/EZtune/", data_name, "2/", sep = "")
f_res <- paste(data_name, method, optimizer, file_num, sep = "_")
write.csv(final, paste(direct, f_res, ".csv", sep = ""), row.names = FALSE)

