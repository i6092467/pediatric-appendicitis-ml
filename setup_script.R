# This script sets up necessary utility functions and packages. Run it before any of the other scripts!

########################## Utility functions

computeArea <- function(x.values, y.values) {
  a <- 0
  for (i in 1 : (length(x.values) - 1)) {
    x1 <- x.values[i]
    x2 <- x.values[i + 1]
    y1 <- y.values[i]
    y2 <- y.values[i + 1]
    a <- a + (y1 + y2) * (x2 - x1) / 2
  }
  
  return(a)
}

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p, dep = TRUE)
  }
  require(p, character.only = TRUE)
}

makeTransparent <- function(someColor, alpha = 100) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata) {
    rgb(red = curcoldata[1], green = curcoldata[2],
        blue = curcoldata[3], alpha = alpha, maxColorValue = 255)})
}

rfVarSelection <- function(formula, data, q, B = 100){
  freqs_b <- numeric(ncol(data))
  names(freqs_b) <- colnames(data)
  idx <- 1 : nrow(data)
  for (b in 1 : B) {
    idx_b <- sample(idx, size=length(idx), replace = TRUE)
    data_b <- data[idx_b, ]
    rf_b <- randomForest(formula, data = data_b, importance = TRUE)
    imps_b <- (rf_b$importance[, 1] + rf_b$importance[, 2]) / 2
    sel_b <- which(names(freqs_b) %in% names(tail(sort(imps_b), q)))
    freqs_b[sel_b] <- freqs_b[sel_b] + 1 
  }
  freqs_b <- freqs_b / B
  return(freqs_b)
}

summaryStats <- function(data, y, adjust.method = "hommel", seed = 42) {
  # NB: We assume that grouping variable y is binary
  set.seed(seed)
  pvals <- numeric(ncol(data))
  names(pvals) <- colnames(data)
  stats <- numeric(ncol(data))
  names(stats) <- colnames(data)
  
  for (j in 1 : ncol(data)) {
    var.j <- data[, j]
    x1 <- var.j[as.numeric(y) == 1]
    x2 <- var.j[as.numeric(y) == 2]
    
    if (is.numeric(data[, j])) {
      test.summary <- wilcox.test(x = x1, y = x2)
    } else if (is.factor(data[, j]))  {
      tab <- table(var.j, y)
      test.summary <- chisq.test(tab)
    }
    pvals[j] <- test.summary$p.value
    stats[j] <- unname(test.summary$statistic)
  }
  pvals <- p.adjust(p = pvals, method = adjust.method)
  ans <- list()
  ans$pvals <- pvals
  ans$stats <- stats
  return(ans)
}


########################## Packages

usePackage("readxl")
usePackage("mice")
usePackage("VIM")
usePackage("ROCR")
usePackage("caret")
usePackage("randomForest")
usePackage("randomForestSRC")
usePackage("gbm")
usePackage("cluster")