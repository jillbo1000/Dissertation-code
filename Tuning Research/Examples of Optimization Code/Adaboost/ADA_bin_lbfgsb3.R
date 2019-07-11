#!/usr/bin/env Rscript
library("R.utils")
library(lbfgsb3)
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


# Optimization grid

name <- paste("lbfgsb3.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 5))
colnames(alldat) <- c("Optimizer", "Nu", "Iter", "Depth", "Error", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  lbfgsb3.obj <- lbfgsb3(prm = c(nstart, istart, dstart), fn = ada.opt, 
                         lower = c(nmin, imin, dmin), upper = c(nmax, imax, dmax))
  t2 <- Sys.time()
  if(!is.null(lbfgsb3.obj)) {
    alldat$Nu[i] <- lbfgsb3.obj$prm[1]
    alldat$Iter[i] <- round(lbfgsb3.obj$prm[2])
    alldat$Depth[i] <- round(lbfgsb3.obj$prm[3])
    alldat$Error[i] <- lbfgsb3.obj$f
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/ADA/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

