#' D3mirt Model Identification
#'
#' @description `modid()` performs model identification for descriptive multidimensional item response theory (DMIRT) models by indicating what items, from a set or scale, to use to maximize the utility of the DMIRT model.
#'
#' @param x A data frame with item data or item factor loadings that fit the multidimensional graded response model (MGRM) or the multidimensional 2-parameter logistic model (M2PL).
#' @param efa Logical, if the data should be explored with exploratory factor analysis. The default is `efa = TRUE`.
#' @param factors The number of factors for the exploratory factor analysis. The default is `factors = 3`.
#' @param lower The lower bound for the item pool is calculated using the standard deviation of scaled item factor loadings. The default is `lower = 0.5`.
#' @param upper The upper bound for filtering of absolute sum scores less than or equal to the indicated value. The default is `upper = .10`
#' @param fac.order Optional. Users can override the automatic sorting of factors by manually indicating factor order with integer values, e.g., `c(2,1,3)` to use the second factor (or column) in data frame x first, first factor (or column) in x second, and the third factor (or column) is left untouched.
#' The default is `fac.order = NULL`.
#' @param itemtype The item model for the exploratory factor analysis. Note, only item type 'graded' (for the MGRM) or '2PL' (for the M2PL) are allowed. The default is `itemtype = "graded"`. See [mirt::mirt] (Chalmers, 2012) for more on item models.
#' @param method A string indicating what integration algorithm to use. The default is `method = 'QMCEM'`. See [mirt::mirt] (Chalmers, 2012) for more on methods.
#' @param rotate A string indicating what rotation method to use for the exploratory factor analysis. The default is `rotate = "oblimin"`. See [mirt::mirt] (Chalmers, 2012) for more on rotations.
#' @param ... Any additional arguments passed to mirt().
#' @importFrom mirt mirt
#'
#' @details Before performing DMIRT analysis it is necessary to identify the compensatory model (Reckase, 2009).
#' For a three-dimensional model, this entails that two items must be fixed.
#' The first item should not load on the second and third axes (y and z), while the second item should not load on the third axis (z).
#' If this can be empirically achieved, it will be possible to create the orthogonal structure necessary for the analysis.
#'
#' The `modid()` function can help by suggesting what items to use for this purpose.
#' Briefly, this is done by `modid()` by first performing an EFA, then ordering factors by the sum of squares, and from this select the strongest loading items that meet the statistical assumptions described above.
#' This orders the entire model so that the strongest loading item, from the strongest factor, always aligns with the x-axis and the other items follow thereon.
#'
#' Because `D3mirt` analysis is based on the M2PL and the MGRM it is recommended to use multidimensional item response theory EFA methods, such as the EFA option in [mirt::mirt] (Chalmers, 2012) with `ìtemtype = 'graded'` or `'2PL'`, so that the EFA is performed with the proper item model.
#' The `mirt()` function is integrated into the `modid()` function so that the user needs only to provide the data frame containing empirical item data in the first argument of the function.
#' Accordingly, in the default mode (`efa = TRUE`) using raw item data, the function performs an EFA, with three factors as default (`factors = 3`), and then finishes with the model identification.
#'
#' However, it is also possible to use the `modid()` function without performing the EFA by setting `efa = FALSE`, if, for instance, a factor loading data frame is already available.
#' This allows the function to jump directly to the model identification step.
#'
#' Note, the EFA is only used to find model identification items that meet the necessary DMIRT model specification requirements.
#' The EFA model itself is discarded after this step in the procedure and the user can, therefore, try different types of rotation methods and compare model identification results.
#'
#' Running the function prints the number of items and factors together with the suggested model identification items to the console.
#' The output consists of an S3 object of class `modid` containing lists with model identification items, order of factor strength (based on sum of squares), and item factor loadings.
#' The summary function is used to inspect the results.
#' The latter includes data frames that hold all the model identification items (`Item.1...Item.n`) selected by `modid()` together with the items absolute sum score (`ABS`), one frame for sum of squares for factors sorted in descending order, and one frame for item factor loadings.
#' The order of the factors follows the model identification items so that item 1 comes from the strongest factor, item 2 from the second strongest, and so on.
#'
#' The absolute sum scores indicate statistical fit to the structural assumptions of the DMIRT model and the items are, therefore, sorted with the lowest absolute sum score highest up.
#' The top items are the items that best meet the necessary statistical requirements for the model identification.
#' For a three-dimensional model this implies that the item highest up in the first data frame should be used to identify the x-axis, and the item highest up in the second data frame should be used to identify the y-axis, and so on.
#'
#' When fitting the model, the first item is constrained not to load on the second and third axes (y and z), while the second item is only constrained not to load on the third axis (z).
#' This will identify an orthogonal three-dimensional structure with the first item fixed on the x-axis.
#' For more details on the model identification process, please see the package vignette.
#'
#' ## Criteria
#' Model identification items should preferably (a) have an absolute sum score of less than or equal to .10 and (b) have the highest factor loading scores on the factor of interest.
#' Of these two criteria, (a) should be given the strongest weight in the selection decision.
#' If these conditions cannot be met, the user is advised to proceed with caution since the loading scores, therefore, imply that an adequate orthogonal structure may not be empirically attainable.
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
#' The lower limit (point 2), however, only increases the size of the item pool used for the item selection.
#' It is, therefore, recommended to adjust the lower limit up and down to see if the output differs, and from that make the final decision.
#'
#' The user also has the option of overriding the automatic sorting of factor order (point 4) with the argument `fac.order` (see examples section).
#' This can be useful when there is only a very small difference between the squared factor loadings that in turn can
#' causes problems (often only observable at later stages) when trying to find the best items for the model identification.
#'
#' Note, the `modid()` function is not limited to three-dimensional analysis and can be used on any number of factors.
#' Moreover, although based on suggestions on model identification given by Reckase (2009) for this type of analysis, the function offers some expansions that introduce more precision.
#' The latter foremost consists in incorporating sum of squares in the item selection process (unless the user has not specified otherwise).
#' Experience tells that this often leads to better results compared to other options.
#' However, it is important to recognize that the `modid()` function only gives suggestions to the model specification, and there could be situations where the researcher should consider other methods.
#'
#' @return S3 object of class `modid` with lists of items and absolute sum scores, sorted by the latter, and sum of squared factor loadings and frame with raw factor loadings with columns ordered on explained variance (high to low) or according to user settings.
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Reckase, M. D., & McKinley, R. L. (1991). The Discriminating Power of Items That Measure More Than One Dimension. \emph{Applied Psychological Measurement, 15}(4), 361-373. https://doi-org.ezp.sub.su.se/10.1177/014662169101500407
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
#' # Optional: Load the EFA data for this example directly from the package file
#' # load(system.file("extdata/efa.Rdata", package = "D3mirt"))
#'
#' # Call to summary
#' summary(g)
#'
#' # Call to modid with increased lower and upper bound
#' modid(x, lower = 1, upper = 1 )
#'
#' # Override factor order by reversing columns in the original data frame
#' modid(x, fac.order = c(3,2,1))
#' }
#' @export
modid <- function(x, efa = TRUE, factors = 3, lower = 0.5, upper = .10, fac.order = NULL, itemtype = "graded", method = 'EM', rotate = "oblimin", ...){
  if (efa == TRUE){
  if (!(itemtype %in% c("graded", "2PL"))) stop ("The item model must be of type graded or 2PL")
  x <- as.matrix(x)
  if(any(!x == round(x))) stop("Set efa to FALSE if the data frame contains factor loadings")
  e <- mirt::mirt(x, model = factors, itemtype = itemtype, method = method, ...)
  f <- mirt::summary(e, rotate = rotate, verbose = FALSE)
  x <- data.frame(f$rotF)
  }
  if (is.null(fac.order)){
    y <- x[,order(colSums(x^2), decreasing = TRUE)]
  } else {
    if(any(!fac.order == round(fac.order))) stop("The factor order must be indicated with integer values")
    if (any(duplicated(fac.order))) stop("The factor order argument has duplicate elements")
    if (!length(fac.order) == ncol(x)) stop("The number of factor indicators and the number of factors do not match")
    if(any(!fac.order <= ncol(x))) stop("The factor argument has at least one factor indicator that is higher than the total number of factors")
    y <- x[, fac.order]
  }
  f <- NULL
  for (i in seq(ncol(y)-1)){
    v <- data.frame(y[,i, drop = FALSE])
    colnames(v) <- paste("Item", i, sep = " ")
    ABS <- data.frame(rowSums(abs(y[, -(1:i), drop = FALSE])))
    colnames(ABS) <- paste("ABS")
    a <- data.frame(cbind(v, ABS))
    a <- a[order(a[, 1], decreasing = TRUE),]
    s <- scale(a[, 1, drop = FALSE], center= TRUE, scale=TRUE)
    s <- subset(s, s >= (s[1, 1] - lower))
    b <- a[1:nrow(s), ]
    b <- b[(b[, 2]) <= upper, ]
    if (nrow(b) == 0) stop ("The model identification failed, try changing factor rotation method or adjusting lower or upper bound")
    c <- b[order(b[, 2]), ]
    f[[i]] <- c
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
