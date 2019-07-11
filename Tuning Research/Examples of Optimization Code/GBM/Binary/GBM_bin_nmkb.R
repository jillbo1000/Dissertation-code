#!/usr/bin/env Rscript
library("R.utils")
library(dfoptim)
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

name <- paste("nmkb.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 6))
colnames(alldat) <- c("Optimizer", "Trees", "Depth", "Nmin", "Shrinkage", "Error", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  nmkb.obj <- nmkb(par = c(tstart, dstart, nstart, sstart), fn = gbm.opt, 
                   lower = c(tmin, dmin, nmin, smin), 
                   upper = c(tmax, dmax, nmax, smax))
  t2 <- Sys.time()
  if(!is.null(nmkb.obj)) {
    alldat$Trees[i] <- nmkb.obj$par[1]
    alldat$Depth[i] <- round(nmkb.obj$par[2])
    alldat$Nmin[i] <- round(nmkb.obj$par[3])
    alldat$Shrinkage[i] <- nmkb.obj$par[4]
    alldat$Error[i] <- nmkb.obj$value
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/GBM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)


