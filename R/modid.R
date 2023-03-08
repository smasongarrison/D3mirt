#' Model identification
#'
#' @param x A data frame or matrix, ≥ 4 columns, with factor loadings and difficulty parameters estimated with the graded response model as item model.
#' @param head Logical, should output contain first five rows per factor only? Default is TRUE.
#'
#' @return The output consists of lists, one per factor from the fitted factor model, containing one column for the factor loadings from each item on the factor interest, and one column with absolute sum scores calculated from remaining factor loadings in the model. Each list is sorted with the lowest absolute sum score highest, and the user is advised to select two items from two different factors in the output that best meet the assumption of orthogonality, i.e., preferably items with absolute sum scores of < .10 and having factor loading > .80. If these conditions cannot be meet the user is advised to proceed with caution since the loading structure implies that an orthogonal structure may not be empirically attainable.
#' @export
#'
#' @examples
#' library(psych)
#' f <- fa(x, nfactors = 3, rotate = "oblimin", residuals = FALSE, SMC=FALSE)
#' z <- data.frame(f$loadings[,])
#' modid(z)
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
