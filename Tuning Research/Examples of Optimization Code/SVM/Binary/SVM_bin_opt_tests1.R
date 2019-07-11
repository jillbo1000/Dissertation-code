#!/usr/bin/env Rscript
library("R.utils")
library(dfoptim)
library(BB)
library(minqa)
library(lbfgsb3)
library(optimx)
library(GA)
library(e1071)

num <- cmdArgs()
data_name <- as.character(num[[1]])
seed <- as.integer(num[[2]])
cross <- as.integer(num[[3]])
cmin <- as.integer(num[[4]])
cmax <- as.integer(num[[5]])
gmin <- as.integer(num[[6]])
gmax <- as.integer(num[[7]])
# file_num <- as.integer(num[[2]])
# range1 <- as.integer(num[[3]])
# range2 <- as.integer(num[[4]])
# num
# data_name
# file_num
# range1
# range2

# Load the data

set.seed(seed)

f1 <- "/scratch/general/lustre/u6007925/Grid/Data/"
f2 <- "_Data.R"

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

svm.opt <- function(params){
  pr <- NULL
  try(pr <- svm(dat[, -1], dat[, 1], cost = params[1], gamma = 2^params[2], cross = cross))
  if(!is.null(pr)){
    err <- 100 - pr$tot.accuracy
  } else {
    err <- NA
  }
  err  
}


# optimization grid

packages <- c("dfoptim", "dfoptim", "BB", "minqa", "stats", "lbfgsb3", "optimx", "optimx", "GA")
optimizers <- c("nmkb", "hjkb", "spg", "bobyqa", "optim with L-BFGS-B", "lbfgsb3", "Rcgmin", "hjn", "GA")
method <- c("Nelder-Mead", "Hooke-Jeeves", "Spectral projected gradient", 
            "Powell", "Quasi-Newton", "Nocedal-Morales", 
            "Nonlinear conjugate gradient with Dai/Yuan update and restart", 
            "Hooke and Jeeves", "Genetic algorithm")

cbind(packages, optimizers, method)

m.nmkb1 <- cbind.data.frame(rep("nmkb1", 10), matrix(NA, nrow = 10, ncol = 4))
colnames(m.nmkb1) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  nmkb.obj <- try(nmkb(par = c(32, -4), fn = svm.opt, lower = c(1, -10), upper = c(1024, 10)))
  t2 <- Sys.time()
  if(!is.null(nmkb.obj)) {
    m.nmkb1$Cost[i] = nmkb.obj$par[1]
    m.nmkb1$Gamma[i] = nmkb.obj$par[2]
    m.nmkb1$Error[i] = nmkb.obj$value * 0.01
  }
  m.nmkb1$Time[i] = as.numeric(t2 - t1, units = "secs")
}

summary.opt <- cbind.data.frame(name, clims, glims, cstart, gstart, 
                                mean(alldat$Time), sd(alldat$Time), 
                                mean(alldat$Time) + 1.96 * sd(alldat$Time) / sqrt(nrow(alldat)),
                                mean(alldat$Error), sd(alldat$Error), 
                                mean(alldat$Error) + 1.96 * sd(alldat$Error) / sqrt(nrow(alldat)))

colnames(summary.opt) <- c("Optimizer", "Cost Range", "Gamma Range", "Start Cost", 
                           "Start Gamma", "Mean Time (S)", "SD Time", "Time UCL",
                           "Mean Error", "SD Error", "Error UCL")

m.nmkb2 <- cbind.data.frame(rep("nmkb2", 10), matrix(NA, nrow = 10, ncol = 4))
colnames(m.nmkb2) = c("Optimizer", "Cost", "Gamma", "Time", "Error")

for(i in 1:10) {
  t1 <- Sys.time()
  nmkb.obj <- try(nmkb(par = c(32, -4), fn = svm.opt, lower = c(1, -10), upper = c(1024, 5)))
  t2 <- Sys.time()
  if(!is.null(nmkb.obj)) {
    m.nmkb2$Cost[i] = nmkb.obj$par[1]
    m.nmkb2$Gamma[i] = nmkb.obj$par[2]
    m.nmkb2$Error[i] = nmkb.obj$value * 0.01
  }
  m.nmkb2$Time[i] = as.numeric(t2 - t1, units = "secs")
}

tmp.summary <- cbind.data.frame("nmkb2", "[1, 1024]", "[2^(-5), 2^5]", 32, -4, 
                                mean(m.nmkb2$Time), sd(m.nmkb2$Time), 
                                mean(m.nmkb2$Error), sd(m.nmkb2$Error), 
                                mean(m.nmkb2$Error) + 1.96 * sd(m.nmkb2$Error) / sqrt(nrow(m.nmkb2)))

colnames(tmp.summary) <- c("Optimizer", "Cost Range", "Gamma Range", "Start Cost", 
                           "Start Gamma", "Mean Time (S)", "SD Time", "Mean Error", 
                           "SD Error", "Error UCL")

summary.opt <- rbind(summary.opt, tmp.summary)
summary.opt 


