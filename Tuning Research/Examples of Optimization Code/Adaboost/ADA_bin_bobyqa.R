#!/usr/bin/env Rscript
library("R.utils")
library(minqa)
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

# optimization grid

name <- paste("bobyqa.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 6))
colnames(alldat) <- c("Optimizer", "Trees", "Depth", "Nmin", "Shrinkage", "Error", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  bobyqa.obj <- bobyqa(par = c(tstart, dstart, nstart, sstart), fn = ada.opt, 
                       lower = c(tmin, dmin, nmin, smin), 
                       upper = c(tmax, dmax, nmax, smax))
  t2 <- Sys.time()
  if(!is.null(bobyqa.obj)) {
    alldat$Trees[i] <- bobyqa.obj$par[1]
    alldat$Depth[i] <- round(bobyqa.obj$par[2])
    alldat$Nmin[i] <- round(bobyqa.obj$par[3])
    alldat$Shrinkage[i] <- bobyqa.obj$par[4]
    alldat$Error[i] <- bobyqa.obj$fval
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/GBM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)


