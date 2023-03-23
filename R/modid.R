#' D3mirt Model Identification
#'
#' @description The function `modid()` can assist with the model identification of the D3mirt object.
#' The model identification procedure in descriptive multidimensional item repose theory consists of the user selecting two items from the item set or scale that best meet the assumption of orthogonality (Reckase, 2009).
#'  These items are then used for setting the orthogonal axes when fitting the graded response model with [`mirt::mirt`] (Chalmers, 2012).
#'
#' @param x A data frame with factor loadings
#' @param factor.order Optional. User can override the automatic sorting of factors by sum of squares loadings in data frame x by manually setting factor order with integer values, e.g., `c(2,1,3)`to use the second factor (or column) in data frame x first, first factor (or column) in x second, and the third factor (or column) is left untouched.
#' Default is `factor.order = NULL`.
#' @param head The number of highest loading items to use in the item selection algorithm. Indicated with integer a value. Default is `head = 5`.
#'
#' @return Lists of factors loadings and absolute sum scores (denoted with `modid`), sorted by the latter, and lastly a frame with raw factor loadings (denoted with `loadings`).
#'
#'
#' @details Before performing descriptive item response theory analysis it is necessary to investigate if the item score patterns can be used to create empirically valid orthogonal axes in the model.
#' For a three-dimensional model, this entails that at least two items must be selected.
#' The first item should not show any systematic relation to the remaining two axes, and the second item should not be systematically related to the third axis.
#' In praxis, this implies that item one, orthogonal to the y and z-axis, will identify the x-axis. Item two, orthogonal to the z-axis, will identify the y-axis, while the z-axis remains free.
#'
#' # Step 1: Explore Data Structure
#' Thus, to begin the factor structure must be explored with exploratory exploratory factor analysis (EFA).
#' Because `D3mirt` analysis is based on item response theory, it is recommended to use multidimensional item response theory EFA methods, such as the EFA option in [mirt::mirt] (Chalmers, 2012) assuming three factors.
#' However, it is also possible to use classical test theory EFA, such as [psych::fa()] (Revelle, 2022).
#' Preferably, the EFA method and rotation option should be carefully chosen based on theory or otherwise statistically reasonable.
#'
#' # Step 2: Item Selection
#' Next, the `modid()` takes the factor structure assigned to a data frame, and outputs lists, one list per factor from the EFA.
#' These lists contain one column for the loadings from each item on the factor of interest, and one column with absolute sum scores for each item calculated from the remaining factor loadings in the model.
#' Each list is sorted with the lowest absolute sum score highest up.
#' Accordingly, the top items are the items that best meet the assumption of orthogonality in the given item set or scale in the EFA model.
#' Regarding item selection, the model identification items should preferably have: (a) an absolute sum score of < .10, and (b) the highest factor loading score of the all items in the set on the factor of interest.
#'
#'
#' # Limitations
#' If the conditions just mention cannot be met, the user is advised to proceed with caution since the loading scores imply that an orthogonal structure may not be empirically attainable.
#' However, it is also important to recognize that the `modid` function gives suggestions to the model identification and there could be situations where the researcher should consider other methods.
#' It also important to investigate more than one model identification and to compare different solutions in light of both psychometric analytical results and theory.
#'
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Revelle W (2022). \emph{psych: Procedures for Psychological, Psychometric, and Personality Research}. Northwestern University, Evanston, Illinois. R package version 2.2.9, https://CRAN.R-project.org/package=psych.
#'
#' @examples
#' \dontrun{
#' # Preparation: Fit a three-factor EFA model
#' # With mirt package
#' library(mirt)
#' f <- mirt(x, 3)
#'
#' # Assign data frame with factor loadings with varimax rotation
#' g <- summary(f, rotate= 'varimax')
#' h <- data.frame(g$rotF)
#' modid(h, head = 10)
#'
#' # Override factor order
#'
#' # With psych package and oblimin rotation
#' library(psych)
#' f <- fa(x, nfactors = 3, rotate = "oblimin", residuals = FALSE, SMC = FALSE)
#'
#' # Assign data frame with factor loadings
#' g <- data.frame(f$loadings[,])
#' modid(z)
#' }
#' @export
modid <- function(x, factor.order = NULL, head = 5){
  if(!is.data.frame(x) && !is.matrix(x)) stop("Input object is not of type data frame or matrix")
  if (head>nrow(x)) stop("Head argument is higher than number of rows in the data frame")
  if(!head== round(head)) stop("Number of heads must be indicated with integer values")
  if (is.null(factor.order)){
    y <- x[,order(colSums(x^2), decreasing = TRUE)]
  } else {
    if(!factor.order== round(factor.order)) stop("Factor order must be indicated with integer values")
    if (any(duplicated(factor.order))) stop("The factor order argument has duplicate elements")
    if(any(!factor.order <= ncol(x))) stop("The factor argument has at least one factor indicator that is higher than the total number of factors")
    y <- x[, factor.order]
  }
  f <- NULL
  for (i in seq(ncol(y)-1)){
    v <- data.frame(y[,i, drop = FALSE])
    colnames(v) <- paste("Item", i, sep = " ")
    ABS <- data.frame(rowSums(abs(y[,-(1:i), drop = FALSE])))
    colnames(ABS) <- paste("ABS")
    a <- data.frame(cbind(v,ABS))
    a <- a[order(a[,1], decreasing = TRUE),]
    a <- a[1:head,]
    a <- a[order(a[,2]),]
    f[[i]] <- a
  }
  if (is.null(factor.order)){
    ss <- sort(colSums(x^2), decreasing = TRUE)
  } else {
    ss <- colSums((x^2)[, factor.order])
  }
  id <- list(modid = f, loadings = y, ss.loadings = ss)
  print(id)
}
