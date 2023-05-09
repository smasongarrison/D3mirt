#' Summary Function for `D3mirt()`
#'
#' @description The `summary.D3mirt()` function presents a compressed output from the [D3mirt::D3mirt()] S3 object.
#' @param object S3 object of class `D3mirt`.
#' @param ... Additional arguments.
#' @param digits User can adjust the number of digits shown per estimate. The default is `digits = 4`.
#'
#' @return Tables containing \emph{a} and \emph{d} parameters, multidimensional discrimination (MDISC), multidimensional item difficulty (MDIFF), direction cosines, and degrees for vector angles for items.
#' If constructs were used in the estimation process, the summary function will also show tables for direction cosines, and degrees for construct vectors as well as directional discrimination (DDISC) parameters.
#' @author Erik Forsberg
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
#' mod1 <- mirt::mirt(x,
#'                    spec,
#'                    itemtype = 'graded',
#'                    SE = TRUE,
#'                    method = 'QMCEM')
#'
#' # Assign a data frame with factor loadings (located in the first three columns in mod1),
#' # and difficulty parameters (columns 4-8 in mod1) with mirt::coef and $'items'[,1:8]))
#' d <- data.frame(mirt::coef(mod1,
#'                            simplify=TRUE)$'items'[,1:8])
#'
#' # Call D3mirt() and create list of constructs
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- D3mirt(d, c)
#'
#' # Call to summary
#' summary(g)
#' }
#' @export
summary.D3mirt <- function(object, ..., digits = 4){
  tab1 <- as.data.frame(object$loadings)
  tab2 <- as.data.frame(object$diff)
  tab1 <- as.data.frame(cbind(tab1, tab2))
  tab3 <- as.data.frame(object$mdiff)
  tab4 <- as.data.frame(object$mdisc)
  tab4 <- as.data.frame(cbind(tab4, tab3))
  tab5 <- as.data.frame(cbind(object$dir.cos, object$spherical))
  if (!is.null(object$c.dir.cos)){
    tab6 <- as.data.frame(cbind(object$c.dir.cos, object$c.spherical))
    tab7 <- as.data.frame(cbind(object$ddisc))
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
    sum <- list(model.est = round(tab1,digits), dmirt.est = round(tab4,digits), dmirt.angles = round(tab5, digits), construct.angles = round(tab6, digits), ddisc = round(tab7, digits))
  } else{
    sum <- list(model.est = round(tab1,digits), dmirt.est = round(tab4,digits), dmirt.angles = round(tab5, digits))
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
