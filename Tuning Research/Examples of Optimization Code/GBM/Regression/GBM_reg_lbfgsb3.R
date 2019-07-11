#!/usr/bin/env Rscript
library("R.utils")
library(lbfgsb3)
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


# Optimization grid

name <- paste("lbfgsb3.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 6))
colnames(alldat) <- c("Optimizer", "Trees", "Depth", "Nmin", "Shrinkage", "MSE", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  lbfgsb3.obj <- lbfgsb3(prm = c(tstart, dstart, nstart, sstart), 
                         fn = gbm.opt, 
                         lower = c(tmin, dmin, nmin, smin), 
                         upper = c(tmax, dmax, nmax, smax))
  t2 <- Sys.time()
  if(!is.null(lbfgsb3.obj)) {
    alldat$Trees[i] <- lbfgsb3.obj$prm[1]
    alldat$Depth[i] <- round(lbfgsb3.obj$prm[2])
    alldat$Nmin[i] <- round(lbfgsb3.obj$prm[3])
    alldat$Shrinkage[i] <- lbfgsb3.obj$prm[4]
    alldat$MSE[i] <- lbfgsb3.obj$f
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/GBM/Regression/Optimization/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

