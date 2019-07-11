#!/usr/bin/env Rscript
library("R.utils")
library(optimx)
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


# Rcgminization grid

name <- paste("Rcgmin.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 5))
colnames(alldat) <- c("Optimizer", "Nu", "Iter", "Depth", "Error", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  Rcgmin.obj <- Rcgmin(par = c(nstart, istart, dstart), fn = ada.opt, 
                       lower = c(nmin, imin, dmin), upper = c(nmax, imax, dmax))
  t2 <- Sys.time()
  if(!is.null(Rcgmin.obj)) {
    alldat$Nu[i] <- Rcgmin.obj$par[1]
    alldat$Iter[i] <- round(Rcgmin.obj$par[2])
    alldat$Depth[i] <- round(Rcgmin.obj$par[3])
    alldat$Error[i] <- Rcgmin.obj$value
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/ADA/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)


