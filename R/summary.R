#' D3mirt Summary Function
#'
#' @description The `summary.dmirt()` function presents a compressed output from the [D3mirt:dmirt()] S3 object.
#' @param object S3 object of class `dmirt`.
#' @param ... Additional arguments to be passed to methods.
#' @param digits User can adjust the number of digits shown per estimate. Default is `digits = 4`.
#'
#' @return Lists containing \emph{a} and \emph{d} parameters, multidimensional discrimination (MDISC), multidimensional item difficulty (MDIFF), direction cosines, and degrees for vector angles for items and constructs.
#' @export
#'
#' @author Erik Forsberg
#'
#' @examples
#' \dontrun{
#' # Preparation
#' #' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- dmirt(d, c)
#'
#' # Call summary
#' summary(g)
#' }
summary.dmirt <- function(object, ..., digits = 4){
  tab1 <- as.data.frame(object$loadings, drop = FALSE)
  tab2 <- as.data.frame(object$mdiff, drop = FALSE)
  tab3 <- as.data.frame(object$mdisc, drop = FALSE)
  tab1 <- as.data.frame(cbind(tab1, tab2, tab3), drop = FALSE)
  tab4 <- as.data.frame(cbind(object$dir.cos, object$degrees), drop = FALSE)
  if (!is.null(object$c.dir.cos)){
    tab5 <- as.data.frame(cbind(object$c.dir.cos, object$c.degrees), drop = FALSE)
    c <- object$c
    items <- NULL
    for (i in seq_along(c)){
      l <- unlist(c[i])
      items <- list(l)
      for (i in seq_along(l)){
        n <- l[i]
        m <- rownames(tab1[n,])
        items <- list(m)
      }
    }
  }
  if (!is.null(object$c.dir.cos)){
    sum <- list(dmirt.est = round(tab1,digits), dmirt.angles = round(tab4, digits), construct.angles = round(tab5, digits))
  } else{
    sum <- list(dmirt.est = round(tab1,digits), dmirt.angles = round(tab4, digits))
  }
  if (!is.null(object$c.dir.cos)){
    cat(paste("\n3Dmirt object with", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
    for (i in seq_along(c)){
      n <- c[i]
      cat(paste("Construct vector", i, "contains items", paste(c[[i]], collapse = ", "), "\n\n"))
    }
  } else {
    cat(paste("\n3Dmirt object with", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
  }
  sum
}
