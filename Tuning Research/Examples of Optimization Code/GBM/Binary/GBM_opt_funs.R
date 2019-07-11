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
cv.pred <- function(x, y, fold, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  xval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:fold, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, fold)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    gbm.t <- gbm(y ~ ., distribution = "bernoulli",
                 interaction.depth = id, n.trees = tr,
                 shrinkage = shr, data = train, n.minobsinnode = nmin)
    xval[xvs == i, 1] <- predict.gbm(gbm.t, newdata = test, type="response",
                                     n.trees = tr)
    cv.acc[i] <- mean(round(xval[xvs == i, 1]) == xval[xvs == i, 2])
  }
  err <- mean(round(xval[, 1]) != xval[, 2])
  err
}

gbm.opt <- function(param){
  pr <- NULL
  try(pr <- cv.pred(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1, 
                    fold = cross, tr = round(param[1]), id = round(param[2]), 
                    nmin = round(param[3]), shr = param[4]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}

num.pred <- function(x, y, num, tr, id, nmin, shr) {
  dat <- cbind(y, x)
  dat <- dat[sample(1:nrow(dat)), ]
  train <- dat[1:num, ]
  test <- dat[-c(1:num), ]
  gbm.t <- gbm(y ~ ., distribution = "bernoulli",
               interaction.depth = id, n.trees = tr,
               shrinkage = shr, data = train, n.minobsinnode = nmin)
  pred <- predict.gbm(gbm.t, newdata = test, type="response",
                      n.trees = tr)
  err <- mean(round(pred) != test$y)
  err
}

gbm.optN <- function(param){
  pr <- NULL
  try(pr <- num.pred(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1, 
                     num = cross, tr = round(param[1]), id = round(param[2]), 
                     nmin = round(param[3]), shr = param[4]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}


