#' Print Function for S3 Objects of Class `D3mirt`
#'
#' @description The `print.D3mirt()` function returns a compressed output from the [D3mirt::D3mirt()] function.
#' @param x S3 object of class `D3mirt`.
#' @param ... Additional arguments.
#'
#' @return Model summary containing class, numbber of items, levels of difficulty, the number construct vectors and their respective items.
#' @author Erik Forsberg
#'
#' @examples
#' \donttest{
#' # Load data
#' data("anes0809offwaves")
#' x <- anes0809offwaves
#' x <- x[,3:22] # Remove columns for age and gender
#'
#' # Fit a three-dimensional graded response model with orthogonal factors
#' spec <- '  F1 = 1-20
#'            F2 = 1-20
#'            F3 = 1-20
#'
#'            START=(W7Q3,a2,0)
#'            START=(W7Q3,a3,0)
#'
#'            START=(W7Q20,a3,0)
#'
#'            FIXED=(W7Q3,a2)
#'            FIXED=(W7Q3,a3)
#'
#'            FIXED=(W7Q20,a3) '
#'
#'
#' mod.1 <- mirt::mirt(x,
#'                    spec,
#'                    itemtype = 'graded',
#'                    SE = TRUE,
#'                    method = 'QMCEM')
#'
#'
#' # Call D3mirt() and create list of constructs
#' c <- list(list(1,2,3,4,5,6,7,8,9,10),
#'           list(11,12,13,14),
#'           list(15,17,18,19,20))
#' g <- D3mirt(mod.1, c)
#'
#' # Print model summery
#' print(g)
#' }
#' @export
print.D3mirt <- function(x, ...){
  tab1 <- as.data.frame(x$loadings)
  tab2 <- as.data.frame(x$diff)
  tab1 <- as.data.frame(cbind(tab1, tab2))
  if (!is.null(x$c.dir.cos)){
    cat(paste("\nD3mirt:", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
    cat(paste("Constructs:\n"))
    for (i in seq_along(x$c)){
      n <- unlist(x$c[i])
      z <- sapply(n, function (x){
        q <- as.character(rownames(tab1[x,]))
      })
      cat(paste("Vector ", i, ": ", paste(z, collapse=", "), "\n", sep = ""))
    }
  } else {
    cat(paste("\nD3mirt:", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
  }
}
