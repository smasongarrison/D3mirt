modid <- function(x, head = TRUE){
  if(!is.data.frame(x) && !is.matrix(x)) stop("Input object is not of type data frame or matrix")
  f <- NULL
  for (i in seq(ncol(x))){
    v <- data.frame(x[,i, drop = FALSE])
    colnames(v) <- paste("F", i, sep = "")
    ABS <- data.frame(rowSums(abs(x[,-i, drop = FALSE])))
    colnames(ABS) <- paste("ABS", i, sep = "")
    a <- data.frame(cbind(v,ABS))
    a <- a[order(a$ABS),]
    if (head == TRUE){
      a <- head(a)
    }
    f[[i]] <- a
  }
  print(f)
}
