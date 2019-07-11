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
opt <- as.character(num[[11]])
opt

# Load the data and the optimization functions

source("/scratch/general/lustre/u6007925/Grid/Functions/SVM_opt_funs.R")

control <- switch(opt, 
                  "ALO" = list(numPopulation = 10, maxIter = 10), 
                  "DA" = list(numPopulation = 10, maxIter = 10), 
                  "FFA" = list(numPopulation = 10, maxIter = 10, B0 = 1, gamma = 1, alpha = 0.2), 
                  "GOA" = list(numPopulation = 10, maxIter = 10),
                  "GWO" = list(numPopulation = 10, maxIter = 10),
                  "HS" = list(numPopulation = 10, maxIter = 10, PAR = 0.3, HMCR = 0.95, bandwith = 0.05),
                  "MFO" = list(numPopulation = 10, maxIter = 10),
                  "PSO" = list(numPopulation = 10, maxIter = 10, Vmax = 2, ci = 1.49445, cg = 1.49445, w = 0.729),
                  "SCA" = list(numPopulation = 10, maxIter = 10),
                  "WOA" = list(numPopulation = 10, maxIter = 10))


# optimization grid

name <- paste(opt, ".", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 4))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Time", "Error")
rangeVar <- matrix(c(cmin, gmin, cmax, gmax), nrow = 2, byrow = TRUE)

for(i in 1:10) {
  t1 <- Sys.time()
  mho.obj <- metaOpt(FUN = svm.opt, optimType = "MIN", algorithm = opt, 
                     numVar = 2, rangeVar = rangeVar, control = control, 
                     seed = seed)
  t2 <- Sys.time()
  if(!is.null(mho.obj)) {
    alldat$Cost[i] = mho.obj$result[1]
    alldat$Gamma[i] = mho.obj$result[2]
    alldat$Error[i] = svm.opt(mho.obj$result) * 0.01
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/SVM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

