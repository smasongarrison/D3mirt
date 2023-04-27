#' D3mirt Model Identification
#'
#' @description `modid()` performs model identification for descriptive multidimensional item response theory (DMIRT) models by indicating what items, from a set or scale, that maximize interpretation of the DMIRT model.
#'
#' @param x A data frame with factor loadings
#' @param lower The lower bound for item inclusion based on item factor loadings. The Lower bound is the standard deviation on the scaled factor loadings. Default is `lower = 0.5`.
#' @param upper The upper bound for filtering of absolute sum scores less than or equal to the indicated value. Default is `upper = .10`
#' @param fac.order Optional. Users can override the automatic sorting of factors by manually indicating factor order with integer values, e.g., `c(2,1,3)` to use the second factor (or column) in data frame x first, first factor (or column) in x second, and the third factor (or column) is left untouched.
#' Default is `fac.order = NULL`.
#'
#' @importFrom mirt mirt
#'
#' @details Before performing DMIRT analysis it is necessary to identify the compensatory model (Reckase, 2009).
#' For a three-dimensional model, this entails that two items must be chosen.
#' If improper items are chosen the model will be hard to interpret in a meaningful way.
#' The `modid()` function was designed to maximize the former analytically.
#' Briefly, the function first order factors by the sum of squares and from this select the strongest loading items that meet the statistical assumptions described above.
#' This orders the model so that the strongest loading item, from the strongest factor, always aligns perfectly with the $x$-axis and that the other items follow thereon.
#'
#' Of the two items that must be chosen for `D3mirt`analysis, the first item is constrained not to load on the second and third axes (y and z),
#' while the second item is only constrained not to load on the third axis (z).
#' This will create an orthogonal structure with the first item fixed on the x-axis.
#' The model identification process is briefly explained below.
#'
#' ## Step 1: Explore Data Structure
#' To begin the factor structure must be explored with exploratory factor analysis (EFA).
#' However, because `D3mirt` analysis is based on item response theory and the 2PL or graded response item model (Samejima, 1969), it is item response theory EFA methods,
#' such as the EFA option in [mirt::mirt] (Chalmers, 2012) with `ìtemtype = 'graded'`, or `2PL` and number of factors set to 3.
#'
#' ## Step 2: Item Selection
#' The `modid()` takes in the factor solution from the EFA, assigned to a data frame \emph{x}, and outputs lists (denoted \emph{id}) with suggestions of items (\emph{item.1....item.n}) to use for the model identification.
#' These lists contain one column for the highest factor loadings (equal to or higher than the limit set by the lower bound) from each item on the factor of interest, and one column with absolute sum scores for each item calculated from the remaining factor loadings in the model.
#' Each list is sorted with the lowest absolute sum score highest up.
#' Accordingly, the top items in each list are the items that best meet the assumption of orthogonality in the analysis.
#' Therefore, for a three-dimensional model, all else equal, the item highest up in the first list should be used to identify the x-axis, and the item highest up in the second list should be used to identify the y-axis, and so on.
#'
#' ## Criteria
#' Optimized model identification items should preferably (a) have an absolute sum score of less than or equal to .10 and (b) with maximized factor loading on the factor of interest.
#' Of these two criteria, (a) should be given the strongest weight in the selection decision.
#' If these conditions cannot be met, the user is advised to proceed with caution since the loading scores imply that an adequate orthogonal structure may not be empirically attainable.
#' If problems occur, try the following:
#'
#' \enumerate{
#' \item Change the rotation method in the EFA, e.g., to change from \emph{oblimin} to \emph{varimax}.
#' \item Adjust the `lower` bound in `modid()`.
#' \item Override factor order with `fac.order`.
#' \item Adjust the `upper` bound in `modid()`.
#' }
#'
#' The latter (point 4) should, however, only be used as a last resort.
#' This is because the upper bound sets the upper limit for item inclusion.
#' Adjusting this limit too high means that the necessary statistical requirements are compromised.
#' The lower limit (point 2), however, only increases the size of the item pool used for the item selection (see the section below on the model identification procedure).
#' It is, therefore, recommended to adjust the lower limit up and down to see if the output differs, and from that make the final decision.
#'
#' The user also has the option of overriding the automatic sorting of factor order (point 4) with the argument `fac.order` (see examples section).
#' This can be useful when there is only a very small difference between the squared factor loadings that in turn can
#' causes problems (often only observable at later stages) when trying to find the best items for the model identification.
#'
#' Note, the `modid()` function is not limited to three-dimensional analysis and can be used on any number of factors.
#' Moreover, although based on suggestions on model identification given by Reckase (2009) for this type of analysis, the function offers some expansions that introduce more precision.
#' The latter foremost consist in incorporating sum of squares in the item selection process (unless the user has not specified otherwise).
#' Experience tells that this is good practice that often leads to better results compared to other options.
#' However, it is important to recognize that the `modid()` function only gives suggestions to the model specification, and there could be situations where the researcher should consider other methods.
#'
#' @return S3 object of class `modid` with lists of items and absolute sum scores, sorted by the latter, and sum of squared factor loadings and frame with raw factor loadings with columns ordered on explained variance (high to low) or according to user settings.
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Reckase, M. D., & McKinley, R. L. (1991). The Discriminating Power of Items That Measure More Than One Dimension. \emph{Applied Psychological Measurement, 15}(4), 361-373–373. https://doi-org.ezp.sub.su.se/10.1177/014662169101500407
#' @references Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. \emph{Psychometrika 34}, 1–97. https://doi.org/10.1007/BF03372160
#'
#' @examples
#' \donttest{
#' # Load data
#' data("anes0809offwaves")
#' x <- anes0809offwaves
#' x <- x[,3:22] # Remove columns for age and gender
#'
#' # Fit a three-factor EFA model with the mirt package
#' e <- mirt::mirt(x, 3, itemtype = 'graded')
#'
#' # Assign data frame with factor loadings with oblimin rotation
#' f <- summary(e, rotate= 'oblimin')
#' h <- data.frame(f$rotF)
#'
#' # Call to modid()
#' modid(h)
#'
#' # Call to modid with increased lower and higher bound
#' modid(h, lower = 1, upper = .12 )
#'
#' # Override factor order by reversing columns in the original data frame
#' modid(h, fac.order = c(3,2,1))
#' }
#' @export
modid <- function(x, lower = 0.5, upper = .10, fac.order = NULL){
  if (is.null(fac.order)){
    y <- x[,order(colSums(x^2), decreasing = TRUE)]
  } else {
   if(any(!fac.order == round(fac.order))) stop("Factor order must be indicated with integer values")
    if (any(duplicated(fac.order))) stop("The factor order argument has duplicate elements")
    if (!length(fac.order) == ncol(x)) stop("The number of factor indicators and the number of factors do not match")
    if(any(!fac.order <= ncol(x))) stop("The factor argument has at least one factor indicator that is higher than the total number of factors")
    y <- x[, fac.order]
  }
  f <- NULL
  for (i in seq(ncol(y)-1)){
    v <- data.frame(y[,i, drop = FALSE])
    colnames(v) <- paste("Item", i, sep = " ")
    ABS <- data.frame(rowSums(abs(y[,-(1:i), drop = FALSE])))
    colnames(ABS) <- paste("ABS")
    a <- data.frame(cbind(v,ABS))
    a <- a[order(a[,1], decreasing = TRUE),]
    b <- NULL
    j <- 0
    k <- 1
    while (j < lower){
      b <- rbind(b,a[k,])
      s <- scale(a, center= TRUE, scale=TRUE)
      j <- (s[1,1]-s[k,1])
      k <- k + 1
    }
    b <- b[(b[,2]) <= upper ,]
    if (nrow(b)==0) stop ("Model identification failed, try changing factor rotation method or adjusting lower or upper bound")
    b <- b[order(b[,2]),]
    f[[i]] <- b
  }
  if (is.null(fac.order)){
    ss <- sort(colSums(x^2), decreasing = TRUE)
  } else {
    ss <- colSums((x^2)[, fac.order])
  }
  modid <- list(id = f, ss.loadings = ss, loadings = y)
  class(modid) <- "modid"
  modid
}
