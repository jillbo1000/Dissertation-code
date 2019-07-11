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

svm.opt <- function(params){
  pr <- NULL
  try(pr <- svm(as.factor(y) ~ ., data = dat, cost = params[1], gamma = 2^params[2], cross = cross))
  if(!is.null(pr)){
    acc <- pr$tot.accuracy
  } else {
    acc <- 0
  }
  100 - acc  
}


sample.pred <- function(x, y, n, cost, gamma) {
  dat <- cbind(y, x)
  dat <- dat[sample(nrow(dat)), ]
  train <- dat[c(1:n), ]
  test <- dat[-c(1:n), ]
  svm.t <- svm(as.factor(y) ~ ., data = train, cost = cost, gamma = gamma)
  pred <- predict(svm.t, newdata = test[, -1])
  mean(as.numeric(as.character(pred)) != test$y)
}


svm.optN <- function(params){
  pr <- NULL
  try(pr <- sample.pred(dat[, -1], dat[, 1], 
                        n = cross, cost = params[1], gamma = 2^params[2]))
  if(!is.null(pr)){
    err <- pr
  } else {
    err <- 1
  }
  err  
}

