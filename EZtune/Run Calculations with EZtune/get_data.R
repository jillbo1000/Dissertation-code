
f1 <- "/scratch/general/lustre/u6007925/Grid/Data/"
f2 <- "_Data.R"

source(paste(f1, data_name, f2, sep = ""))

dat <- switch(data_name,
              "BreastCancer" = bc,
              "Ionosphere" = Ionosphere,
              "Lichen" = lichen,
              "Mullein" = mullein,
              "Pima" = pid,
              "Sonar" = Sonar,
              "Abalone" = abalone,
              "BostonHousing" = bh,
              "CO2" = CO2,
              "Crime" = crime,
              "OhioHousing" = oh,
              "Union" = union,
              "Wage" = wage)

colnames(dat)[1] <- "y"

x <- dat[, -1]
y <- dat[, 1]

convert <- function(value) {
  if(value == "NULL") {
    value <- NULL
  } else if(value == "TRUE" | value == "FALSE") {
    value <- as.logical(value)
  } else {
    value <- as.numeric(value)
  }
  value
}

parameters <- function(y, method) {
  if(method == "gbm") {
    param <- matrix(0, nrow = 10, ncol = 4)
    colnames(param) <- c("n.trees", "interaction.depth", "n.minobsinnode", "shrinkage")
  } else if(method == "ada") {
    param <- matrix(0, nrow = 10, ncol = 3)
    colnames(param) <- c("iter", "nu", "maxdepth")
  } else if(method == "svm" & (length(unique(y)) == 2)) {
    param <- matrix(0, nrow = 10, ncol = 2)
    colnames(param) <- c("cost", "gamma")
  } else {
    param <- matrix(0, nrow = 10, ncol = 3)
    colnames(param) <- c("cost", "gamma", "epsilon")
  }
  as.data.frame(param)
}

param_fill <- function(y, method, tmp) {
  if(method == "gbm") {
    p_tmp <- c(tmp$n.trees, tmp$interaction.depth, tmp$n.minobsinnode, tmp$shrinkage)
  } else if(method == "ada") {
    p_tmp <- c(tmp$iter, tmp$nu, tmp$maxdepth)
  } else if(method == "svm" & (length(unique(y)) == 2)) {
    p_tmp <- c(tmp$cost, tmp$gamma)
  } else {
    p_tmp <- c(tmp$cost, tmp$gamma, tmp$epsilon)
  }
  p_tmp
}




