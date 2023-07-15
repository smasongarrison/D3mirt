#' Print Function for S3 Objects of Class `modid`
#'
#' @description The `print.D3mirt()` function returns a compressed output from the [D3mirt::modid()] function.
#' @param x S3 object of class `modid`.
#' @param ... Additional arguments.
#'
#' @return Printed message reporting the number of factors and the suggested model identification items.
#'
#' @examples
#' \donttest{
#' # Load data
#' data("anes0809offwaves")
#' x <- anes0809offwaves
#' x <- x[,3:22] # Remove columns for age and gender
#'
#' # Identify the DMIRT model using a three-factor EFA
#' g <- modid(x)
#'
#' #' # Optional: Load the EFA data for this example directly from the package file
#' # load(system.file("efa.Rdata", package = "D3mirt"))
#'
#' # Print model summary
#' print(g)
#' }
#' @export
print.modid <- function(x, ...){
  f <- NULL
  for (i in seq_along(x$id)){
    item <- x$id[[i]]
    f[[i]] <- item[1,]
  }
  cat(paste("\nmodid:",  nrow(x$loadings), "items and", length(x$ss.loadings), "factors\n\n"))
  cat(paste("Model identification items:\n"))
  for (i in seq_along(x$id)){
    cat(paste("Item", i, rownames(f[[i]])), sep = " ", "\n")
  }
}
