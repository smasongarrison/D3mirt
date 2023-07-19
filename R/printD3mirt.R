#' Print Function for S3 Objects of Class `D3mirt`
#'
#' @description The `print.D3mirt()` function returns a compressed output from the [D3mirt::D3mirt()] function.
#' @param x S3 object of class `D3mirt`.
#' @param ... Additional arguments.
#'
#' @return Model summary containing class, number of items, levels of difficulty, the number of construct vectors and their respective items.
#' @author Erik Forsberg
#'
#' @examples
#' \dontrun{
#' # Call D3mirt()
#' # The first argument can be data frame with model parameters
#' # or an S4 object of class 'SingleGroupClass' exported from mirt::mirt
#' g <- D3mirt(mod1)
#'
#' # Print model summary
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
    cat(paste("\n"))
  } else {
    cat(paste("\nD3mirt:", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
  }
}
