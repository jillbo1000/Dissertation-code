#!/usr/bin/env Rscript
library("R.utils")
library(metaheuristicOpt)
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
cmin <- as.numeric(num[[5]])
cmin
cmax <- as.numeric(num[[6]])
cmax
cstart <- as.numeric(num[[7]])
cstart
gmin <- -1 * as.numeric(num[[8]])
gmin
gmax <- as.numeric(num[[9]])
gmax
gstart <- -1 * as.numeric(num[[10]])
gstart
emin <- as.numeric(num[[11]])
emin
emax <- as.numeric(num[[12]])
emax
estart <- as.numeric(num[[13]])
estart

# Load the data and the optimization functions

source("/scratch/general/lustre/u6007925/Grid/Functions/SVM_opt_funs_reg.R")


# optimization grid

name <- paste("alo.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 5))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Epsilon", "Time", "MSE")
rangeVar <- matrix(c(cmin, gmin, emin, cmax, gmax, emax), nrow = 2, byrow = TRUE)

for(i in 1:10) {
  t1 <- Sys.time()
  alo.obj <- ALO(FUN = svm.opt, optimType = "MIN", numVar = 5,  
                 numPopulation = 10, maxIter = 10, rangeVar)
  t2 <- Sys.time()
  if(!is.null(alo.obj)) {
    alldat$Cost[i] = alo.obj[1]
    alldat$Gamma[i] = round(alo.obj[2])
    alldat$Epsilon[i] = round(alo.obj[3])
    alldat$MSE[i] = svm.opt(alo.obj)
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/SVM/Regression/Optimization/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)


