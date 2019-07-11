#!/usr/bin/env Rscript
library("R.utils")
library(GA)
library(gbm)

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
tmin <- as.integer(num[[5]])
tmin
tmax <- as.integer(num[[6]])
tmax
dmin <- as.integer(num[[7]])
dmin
dmax <- as.integer(num[[8]])
dmax
nmin <- as.integer(num[[9]])
nmin
nmax <- as.integer(num[[10]])
nmax
smin <- as.numeric(num[[11]])
smin
smax <- as.numeric(num[[12]])
smax
tstart <- as.integer(num[[13]])
tstart
dstart <- as.integer(num[[14]])
dstart
nstart <- as.integer(num[[15]])
nstart
sstart <- as.numeric(num[[16]])
sstart

# Load the data and the optimization functions

source("/scratch/general/lustre/u6007925/Grid/Functions/GBM_opt_funs_reg.R")

gbm2.opt <- function(param) {
  -1 * gbm.optN(param)
}

# Optimization grid

name <- paste("ga.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 6))
colnames(alldat) = c("Optimizer", "Trees", "Depth", "Nmin", "Shrinkage", "MSE", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  ga.obj <- ga(type = "real-valued", fitness = gbm2.opt, parallel = 2, 
               seed = seed, maxiter = 10, run = 5, 
               lower = c(tmin, dmin, nmin, smin), 
               upper = c(tmax, dmax, nmax, smax))
  t2 <- Sys.time()
  if(!is.null(ga.obj)) {
    alldat$Trees[i] = ga.obj@solution[1, 1]
    alldat$Depth[i] = round(ga.obj@solution[1, 2])
    alldat$Nmin[i] = round(ga.obj@solution[1, 3])
    alldat$Shrinkage[i] = ga.obj@solution[1, 4]
    alldat$MSE[i] = -1 * ga.obj@fitnessValue
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/GBM/Regression/Optimization/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

