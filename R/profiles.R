profiles <- function(x, y = NULL, column = 1, condition = c(">"), prob = .75, method = "EAP", full.scores = TRUE, full.scores.SE = FALSE, QMC = TRUE){
  if(!isS4(x)) warning("x must be S4 mirt object from mirt package")
  m <- as.data.frame(fscores(x, method = method, full.scores= full.scores, full.scores.SE = full.scores.SE, QMC=QMC), drop = FALSE)
  if (!is.null(y)){
    if (isS4(y)){
      if(column > 3) warning("Column argument set to high, must be 3 or less for S4 objects")
      y <- as.data.frame(fscores(y, method = method, full.scores= full.scores, full.scores.SE = full.scores.SE, QMC=QMC), drop = FALSE)
      y <- as.matrix(y[,column, drop = FALSE])
    } else {
      if(!is.data.frame(y) && !is.matrix(y)) stop("Input object is not of type data frame or matrix")
      y <- as.matrix(y[,column, drop = FALSE])
    }
    if (condition == ">"){
      m$dummy <- ifelse(y[,1] > quantile(y, prob = prob),1,0)
    }
    else if (condition == "<"){
      m$dummy <- ifelse(y[,1] < quantile(y, prob = prob),1,0)
    }
    else if (condition == ">="){
      m$dummy <- ifelse(y[,1] >= quantile(y, prob = prob),1,0)
    }
    else if (condition == "<="){
      m$dummy <- ifelse(y[,1] <= quantile(y, prob = prob),1,0)
    }
  }
  if (is.null(y)){
    if (condition == ">"){
      m$dummy <- ifelse(m[,column[1]] > quantile(m[,column], prob = prob),1,0)
    }
    else if (condition == "<"){
      m$dummy <- ifelse(m[,column[1]] < quantile(m[,column], prob = prob),1,0)
    }
    else  if (condition == ">="){
      m$dummy <- ifelse(m[,column[1]] >= quantile(m[,column], prob = prob),1,0)
    }
    else  if (condition == "<="){
      m$dummy <- ifelse(m[,column[1]] <= quantile(m[,column], prob = prob),1,0)
    }
  }
  m[m$dummy == 1, ]
}
