#!/usr/bin/env Rscript
library("R.utils")
library(optimx)
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

# Load the data and the optimization functions

source("/scratch/general/lustre/u6007925/Grid/Functions/SVM_opt_funs.R")


# optimization grid

name <- paste("Rcgmin.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 4))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  Rcgmin.obj <- Rcgmin(par = c(cstart, gstart), fn = svm.opt, 
                       lower = c(cmin, gmin), upper = c(cmax, gmax))
  t2 <- Sys.time()
  if(!is.null(Rcgmin.obj)) {
    alldat$Cost[i] = Rcgmin.obj$par[1]
    alldat$Gamma[i] = Rcgmin.obj$par[2]
    alldat$Error[i] = Rcgmin.obj$value * 0.01
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/SVM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)

