#!/usr/bin/env Rscript
library("R.utils")
library(GA)
library(e1071)

num <- cmdArgs()
data_name <- as.character(num[[1]])
file_num <- as.integer(num[[2]])
seed <- as.integer(num[[3]])
cross <- as.integer(num[[4]])
cmin <- as.numeric(num[[5]])
cmax <- as.numeric(num[[6]])
gmin <- -1 * as.numeric(num[[7]])
gmax <- as.numeric(num[[8]])
cstart <- as.numeric(num[[9]])
gstart <- -1 * as.numeric(num[[10]])

# Load the data

set.seed(seed)

f1 <- "/scratch/general/lustre/u6007925/Grid/Data/"
source("/scratch/general/lustre/u6007925/Grid/Functions/SVM_opt_funs.R")

source("scratch")
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

svm.opt2 <- function(params){
  1 - svm.optN
}


# Optimization grid

name <- paste("ga.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 4))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  ga.obj <- ga(type = "real-valued", fitness = svm.opt2, parallel = 2, 
               seed = seed, maxiter = 10, run = 5, 
               lower = c(cmin, gmin), upper = c(cmax, gmax))
  t2 <- Sys.time()
  if(!is.null(ga.obj)) {
    alldat$Cost[i] = ga.obj@solution[1, 1]
    alldat$Gamma[i] = ga.obj@solution[1, 2]
    alldat$Error[i] = 1 - ga.obj@fitnessValue
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}


f1 <- "/scratch/general/lustre/u6007925/Grid/SVM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

# fsum <- paste(f1, data_name, "/", data_name, "_opt_summary.csv", sep = "")
# 
# write.csv(summary.opt, fsum, row.names = FALSE, col.names = FALSE, append = TRUE)

