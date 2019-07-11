#!/usr/bin/env Rscript
library("R.utils")
library(optimx)
library(e1071)

num <- cmdArgs()
data_name <- as.character(num[[1]])
file_num <- as.integer(num[[2]])
seed <- as.integer(num[[3]])
cross <- as.integer(num[[4]])
cmin <- as.integer(num[[5]])
cmax <- as.integer(num[[6]])
gmin <- -1 * as.integer(num[[7]])
gmax <- as.integer(num[[8]])
cstart <- as.integer(num[[9]])
gstart <- -1 * as.integer(num[[10]])

# Load the data

set.seed(seed)

f1 <- "/scratch/general/lustre/u6007925/Grid/Data/"
f2 <- "_Data.R"

source("/scratch/general/lustre/u6007925/Grid/Functions/SVM_opt_funs.R")
source(paste(f1, data_name, f2, sep = ""))

dat <- switch(data_name, 
              "BreastCancer" = bc, 
              "Ionosphere" = Ionosphere, 
              "Lichen" = lichen, 
              "Mullein" = mullein, 
              "Pima" = pid,
              "Sonar" = Sonar)

colnames(dat)[1] <- "y"

# Function that is to be optimized

# Optimization grid

name <- paste("hjn.", file_num, sep = "")

alldat <- cbind.data.frame(rep(name, 10), matrix(NA, nrow = 10, ncol = 4))
colnames(alldat) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  hjn.obj <- hjn(par = c(cstart, gstart), fn = svm.optN, 
                 lower = c(cmin, gmin), upper = c(cmax, gmax))
  t2 <- Sys.time()
  if(!is.null(hjn.obj)) {
    alldat$Cost[i] = hjn.obj$par[1]
    alldat$Gamma[i] = hjn.obj$par[2]
    alldat$Error[i] = hjn.obj$value
  }
  alldat$Time[i] = as.numeric(t2 - t1, units = "secs")
}

f1 <- "/scratch/general/lustre/u6007925/Grid/SVM/Binary/"

fdat <- paste(f1, data_name, "/", data_name, "_", name, ".csv", sep = "")

write.csv(alldat, fdat, row.names = FALSE)


