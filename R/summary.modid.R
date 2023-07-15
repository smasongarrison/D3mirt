#' Summary Function for S3 Objects of Class `modid`
#'
#' @description The `summary.modid()` function returns a compressed output from the [D3mirt::modid()] function.
#' @param object S3 object of class `modid`.
#' @param ... Additional arguments.
#' @param digits The number of digits shown per estimate. The default is `digits = 4`.
#'
#' @return Model identification items (one less than the number of factors), factor loadings and absolute sum score for items, and squared factor loadings for the model identification items.
#' @author Erik Forsberg
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
#' # Optional: Load the EFA data for this example directly from the package file
#' # load(system.file("efa.Rdata", package = "D3mirt"))
#'
#' # Call to summary
#' summary(g)
#' }
#' @export
summary.modid <- function(object, ..., digits = 4){
  tab1 <- as.data.frame(object$ss.loadings)
  colnames(tab1) <- "SS Loadings"
  tab2 <- as.data.frame(object$loadings)
  f <- NULL
  for (i in seq_along(object$id)){
    item <- object$id[[i]]
    f[[i]] <- item[1,]
  }
  cat(paste("\nmodid:",  nrow(object$loadings), "items and", length(object$ss.loadings), "factors\n\n"))
  cat(paste("Model identification items:\n"))
  for (i in seq_along(object$id)){
    cat(paste("Item", i, rownames(f[[i]])), sep = " ", "\n")
  }
  for (i in seq_along(object$id)){
    cat(paste("\n"))
    x <- data.frame(object$id[i])
    x <- round(x, 4)
    print(x)
  }
  cat(paste("\n"))
  print(round(tab1, digits))
  cat(paste("\n"))
  print(round(tab2, digits))
}
