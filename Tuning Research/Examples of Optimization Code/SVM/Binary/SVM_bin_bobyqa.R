#!/usr/bin/env Rscript
library("R.utils")
library(minqa)
library(e1071)

num <- cmdArgs()
num
data_name <- as.character(num[[1]])
data_name
file_num <- as.integer(num[[2]])
file_num
seed <- as.integer(num[[3]])
seed
cross <- as.integer(num[[4]])
cross
cmin <- as.integer(num[[5]])
cmin
cmax <- as.integer(num[[6]])
cmax
gmin <- -1 * as.integer(num[[7]])
gmin
gmax <- as.integer(num[[8]])
gmax
cstart <- as.integer(num[[9]])
cstart
gstart <- -1 * as.integer(num[[10]])
gstart

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


# optimization grid

name <- paste("bobyqa.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 4))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  bobyqa.obj <- bobyqa(par = c(cstart, gstart), fn = svm.opt, 
                       lower = c(cmin, gmin), upper = c(cmax, gmax))
  t2 <- Sys.time()
  if(!is.null(bobyqa.obj)) {
    alldat$Cost[i] = bobyqa.obj$par[1]
    alldat$Gamma[i] = bobyqa.obj$par[2]
    alldat$Error[i] = bobyqa.obj$fval * 0.01
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

