#' Print Method for S3 Objects of Class `modid`
#'
#' @description The print method for the [D3mirt::modid()] function.
#' @param x A S3 object of class `modid`.
#' @param ... Additional arguments.
#'
#' @return A printed message reporting the number of factors and the suggested model identification items.
#'
#' @examples
#' \dontrun{
#' # Identify the DMIRT model using a three-factor EFA with modid()
#' # x can be a data frame with item data or item factor loadings.
#' # In the case of the latter, set argument 'efa' to 'FALSE'
#' g <- modid(x)
#'
#' # Print model identification summary
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
