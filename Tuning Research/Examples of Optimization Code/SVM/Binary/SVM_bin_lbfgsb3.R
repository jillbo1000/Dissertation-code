#!/usr/bin/env Rscript
library("R.utils")
library(lbfgsb3)
library(e1071)

num <- cmdArgs()
data_name <- as.character(num[[1]])
file_num <- as.integer(num[[2]])
seed <- as.integer(num[[3]])
cross <- as.integer(num[[4]])
cmin <- as.integer(num[[5]])
cmax <- as.integer(num[[6]])
gmin <- -1 * as.integer(num[[7]])
gmax <- as.integer(num[[8]])
cstart <- as.integer(num[[9]])
gstart <- -1 * as.integer(num[[10]])

# Load the data

set.seed(seed)

f1 <- "/scratch/general/lustre/u6007925/Grid/Data/"
f2 <- "_Data.R"

source(paste(f1, data_name, f2, sep = ""))

dat <- switch(data_name, 
              "BreastCancer" = bc, 
              "Ionosphere" = Ionosphere, 
              "Lichen" = lichen, 
              "Mullein" = mullein, 
              "Pima" = pid,
              "Sonar" = Sonar)

colnames(dat)[1] <- "y"

# Function that is to be optimized

svm.opt <- function(params){
  pr <- NULL
  try(pr <- svm(as.factor(y) ~ ., data = dat, cost = params[1], gamma = 2^params[2], cross = cross))
  if(!is.null(pr)){
    err <- 100 - pr$tot.accuracy
  } else {
    err <- 100
  }
  err  
}


# Optimization grid

name <- paste("lbfgsb3.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 4))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  lbfgsb3.obj <- lbfgsb3(prm = c(cstart, gstart), fn = svm.opt, 
                         lower = c(cmin, gmin), upper = c(cmax, gmax))
  t2 <- Sys.time()
  if(!is.null(lbfgsb3.obj)) {
    alldat$Cost[i] = lbfgsb3.obj$prm[1]
    alldat$Gamma[i] = lbfgsb3.obj$prm[2]
    alldat$Error[i] = lbfgsb3.obj$f * 0.01
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}

# clims <- paste("[", cmin, ", ", cmax, "]", sep = "")
# glims <- paste("[", gmin, ", ", gmax, "]", sep = "")
# 
# summary.opt <- cbind.data.frame(name, clims, glims, cstart, gstart, 
#                                 mean(alldat$Time), sd(alldat$Time), 
#                                 mean(alldat$Time) + 1.96 * sd(alldat$Time) / sqrt(nrow(alldat)),
#                                 mean(alldat$Error), sd(alldat$Error), 
#                                 mean(alldat$Error) + 1.96 * sd(alldat$Error) / sqrt(nrow(alldat)))
# 
# colnames(summary.opt) <- c("Optimizer", "Cost Range", "Gamma Range", "Start Cost", 
#                            "Start Gamma", "Mean Time (S)", "SD Time", "Time UCL",
#                            "Mean Error", "SD Error", "Error UCL")

f1 <- "/scratch/general/lustre/u6007925/Grid/SVM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

# fsum <- paste(f1, data_name, "/", data_name, "_opt_summary.csv", sep = "")
# 
# write.csv(summary.opt, fsum, row.names = FALSE, col.names = FALSE, append = TRUE)

