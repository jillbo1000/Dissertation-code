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

# CV Function that is to be optimized
cv.pred <- function(x, y, fold, nu, iter, maxd) {
  dat <- cbind(y, x)
  xval <- cbind(rep(0, nrow(x)), y)
  xvs <- rep(1:fold, length = nrow(x))
  xvs <- sample(xvs)
  cv.acc <- rep(0, fold)
  for(i in 1:fold) {
    train <- dat[xvs != i, ]
    test <- dat[xvs == i, ]
    ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", iter = round(iter),
                      nu = nu, data = train, control = rpart.control(maxdepth = round(maxd)))
    xval[xvs == i, 1] <- stats::predict(ada.t, newdata = test, type = "prob")[,2]
    cv.acc[i] <- mean(round(xval[xvs == i, 1]) == xval[xvs == i, 2])
  }
  mean(round(xval[, 1]) != xval[, 2])
}

ada.opt <- function(param){
  pr <- NULL
  try(pr <- cv.pred(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1, 
                    fold = cross, nu = param[1], iter = param[2], 
                    maxd = param[3]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}


# Sampling Function that is to be optimized
sample.pred <- function(x, y, n, nu, iter, maxd) {
  dat <- cbind(y, x)
  dat <- dat[sample(nrow(dat)), ]
  train <- dat[c(1:n), ]
  test <- dat[-c(1:n), ]
  ada.t <- ada::ada(as.factor(y) ~ ., loss = "exponential", iter = round(iter),
                    nu = nu, data = train, control = rpart.control(maxdepth = round(maxd)))
  pred <- round(stats::predict(ada.t, newdata = test, type = "prob")[,2])
  mean(pred != test$y)
}

ada.optN <- function(param){
  pr <- NULL
  try(pr <- sample.pred(dat[, -1], as.numeric(as.factor(dat[, 1])) - 1, 
                        n = n, nu = param[1], iter = param[2], 
                        maxd = param[3]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}

