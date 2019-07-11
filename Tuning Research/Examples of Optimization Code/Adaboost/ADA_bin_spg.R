#!/usr/bin/env Rscript
library("R.utils")
library(BB)
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

name <- paste("spg.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 5))
colnames(alldat) <- c("Optimizer", "Nu", "Iter", "Depth", "Error", "Time")

for(i in 1:10) {
  t1 <- Sys.time()
  spg.obj <- spg(par = c(nstart, istart, dstart), fn = ada.opt, 
                 lower = c(nmin, imin, dmin), upper = c(nmax, imax, dmax))
  t2 <- Sys.time()
  if(!is.null(spg.obj)) {
    alldat$Nu[i] <- spg.obj$par[1]
    alldat$Iter[i] <- round(spg.obj$par[2])
    alldat$Depth[i] <- round(spg.obj$par[3])
    alldat$Error[i] <- spg.obj$value
  }
  alldat$Time[i] <- as.numeric(t2 - t1, units = "secs")
}


f1 <- "/scratch/general/lustre/u6007925/Grid/ADA/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

# fsum <- paste(f1, data_name, "/", data_name, "_opt_summary.csv", sep = "")
# 
# write.csv(summary.opt, fsum, row.names = FALSE, col.names = FALSE, append = TRUE)

