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
              "Sonar" = Sonar, 
              "Abalone" = abalone, 
              "BostonHousing" = bh, 
              "CO2" = CO2, 
              "Crime" = crime, 
              "OhioHousing" = oh, 
              "Union" = union, 
              "Wage" = wage)

colnames(dat)[1] <- "y"


svm.opt <- function(params){
  pr <- NULL
  try(pr <- svm(y ~ ., data = dat, cost = params[1], gamma = 2^params[2], 
                epsilon = params[3], cross = cross))
  if(!is.null(pr)){
    mse <- pr$tot.MSE
  } else {
    mse <- 1e+150
  }
  mse  
}


sample.pred <- function(x, y, n, cost, gamma, epsilon) {
  dat <- cbind(y, x)
  dat <- dat[sample(nrow(dat)), ]
  train <- dat[c(1:n), ]
  test <- dat[-c(1:n), ]
  svm.t <- svm(y ~ ., data = train, cost = cost, gamma = gamma, epsilon = epsilon)
  pred <- predict(svm.t, newdata = test[, -1])
  mean((pred - test$y)^2)
}


svm.optN <- function(params){
  pr <- NULL
  try(pr <- sample.pred(dat[, -1], dat[, 1], 
                        n = cross, cost = params[1], gamma = 2^params[2], epsilon = params[3]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}



