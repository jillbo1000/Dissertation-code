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

# Function that is to be optimized for regression

cv.pred <- function(x, y, fold, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  xval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:fold, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, fold)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm(y ~ ., distribution = "gaussian",
                 interaction.depth = id, n.trees = tr,
                 shrinkage = shr, data = train, n.minobsinnode = nmin)
    xval[xvs == i, 1] <- predict.gbm(gbm.t, newdata = test, type="response",
                                     n.trees = tr)
  }
  mse <- mean((xval[, 1] - xval[, 2])^2)
  mse
}

gbm.opt <- function(param){
  pr <- NULL
  try(pr <- cv.pred(dat[, -1], dat[, 1], fold = cross, tr = round(param[1]), 
                    id = round(param[2]), nmin = round(param[3]), 
                    shr = param[4]))
  if(!is.null(pr)){
    mse <- pr
  } else {
    mse <- 1e+300
  }
  mse  
}

sample.pred <- function(x, y, n, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  dat <- dat[sample(nrow(dat)), ]
  train <- dat[c(1:n), ]
  test <- dat[-c(1:n), ]
  gbm.t <- gbm(y ~ ., distribution = "gaussian",
               interaction.depth = id, n.trees = tr,
               shrinkage = shr, data = train, n.minobsinnode = nmin)
  pred <- predict.gbm(gbm.t, newdata = test, type="response", n.trees = tr)
  mean((pred - test$y)^2)
}


gbm.optN <- function(params){
  pr <- NULL
  try(pr <- sample.pred(dat[, -1], dat[, 1], 
                        n = cross, tr = params[1], id = params[2], nmin = params[3], shr = params[4]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}

