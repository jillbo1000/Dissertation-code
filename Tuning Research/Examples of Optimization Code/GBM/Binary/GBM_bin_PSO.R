#!/usr/bin/env Rscript
library("R.utils")
library(metaheuristicOpt)
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

source("/scratch/general/lustre/u6007925/Grid/Functions/GBM_opt_funs.R")


# optimization grid

name <- paste("pso.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 6))
colnames(alldat) <- c("Optimizer", "Trees", "Depth", "Nmin", "Shrinkage", "Error", "Time")
rangeVar <- matrix(c(tmin, dmin, nmin, smin, tmax, dmax, nmax, smax), nrow = 2, byrow = TRUE)

for(i in 1:10) {
  t1 <- Sys.time()
  pso.obj <- PSO(FUN = gbm.opt, optimType = "MIN", numVar = 5,  
               numPopulation = 10, maxIter = 10, rangeVar)
  t2 <- Sys.time()
  if(!is.null(pso.obj)) {
    alldat$Trees[i] <- pso.obj[1]
    alldat$Depth[i] <- round(pso.obj[2])
    alldat$Nmin[i] <- round(pso.obj[3])
    alldat$Shrinkage[i] <- pso.obj[4]
    alldat$Error[i] <- gbm.opt(pso.obj)
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/GBM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)


