#!/usr/bin/env Rscript
library("R.utils")
library(GA)
library(ada)

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
nmin <- as.numeric(num[[5]])
nmin
nmax <- as.numeric(num[[6]])
nmax
imin <- as.integer(num[[7]])
imin
imax <- as.integer(num[[8]])
imax
dmin <- as.integer(num[[9]])
dmin
dmax <- as.integer(num[[10]])
dmax
nstart <- as.numeric(num[[11]])
nstart
istart <- as.integer(num[[12]])
istart
dstart <- as.integer(num[[13]])
dstart

# Load the data and the optimization functions

source("/scratch/general/lustre/u6007925/Grid/Functions/ADA_opt_funs.R")

ada2.opt <- function(param) {
  1 - ada.opt(param)
}

# Optimization grid

name <- paste("ga.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 5))
colnames(alldat) <- c("Optimizer", "Nu", "Iter", "Depth", "Error", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  ga.obj <- ga(type = "real-valued", fitness = ada2.opt, parallel = 2, 
               seed = seed, maxiter = 10, run = 5, 
               lower = c(nmin, imin, dmin), upper = c(nmax, imax, dmax))
  t2 <- Sys.time()
  if(!is.null(ga.obj)) {
    alldat$Nu[i] <- ga.obj@solution[1, 1]
    alldat$Iter[i] <- round(ga.obj@solution[1, 2])
    alldat$Depth[i] <- round(ga.obj@solution[1, 3])
    alldat$Error[i] <- 1 - ga.obj@fitnessValue
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/ADA/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

