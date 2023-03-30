#' DMIRT Model Identification
#'
#' @description `modid()` assists with the model identification of the D3mirt object by indicating what items, from a set or scale, to use for identifying the model to use in the D3mirt analysis.
#'
#' @param x A data frame with factor loadings
#' @param lower The lower bound for item inclusion based on item factor loadings. Default is `lower = 0.5`.
#' @param upper The upper bound for filtering of absolute sum scores less than or equal to the indicated value. Default is `upper = .10`
#' @param fac.order Optional. Users can override the automatic sorting of factors by manually indicating factor order with integer values, e.g., `c(2,1,3)` to use the second factor (or column) in data frame x first, first factor (or column) in x second, and the third factor (or column) is left untouched.
#' Default is `factor.order = NULL`.
#'
#' @details Before performing descriptive item response theory analysis it is necessary to identify the measurement model (Reckase, 2009), that will be used to specify the multidimensional graded response model with [`mirt::mirt`] (Chalmers, 2012; see [D3mirt::D3mirt] documentation for more details on model specification).
#' For a three-dimensional model, this entails that two items must be chosen.
#' If improper items are chosen the model will be hard to interpret in a meaningful way.
#' The `modid` function maximize the former analytically by first order factors by sum of squares and then select the most optimal items.
#' This help order the model so that the strongest loading items on the strongest factor always align with the x-axis.
#'
#' Of the two items that must be chosen for `D3mirt`analysis, the first item is constrained not to load on the second and third axes (y and z), while the second item is only constrained not to load on the third axis (z).
#' This will create an orthogonal structure with the first item locked in parallel on the x-axis.
#' The model identification process is briefly explained below.
#'
#' ## Step 1: Explore Data Structure
#' To begin the factor structure must be explored with exploratory factor analysis (EFA).
#' Because `D3mirt` analysis is based on item response theory, it is recommended to use multidimensional item response theory EFA methods, such as the EFA option in [mirt::mirt] (Chalmers, 2012), for increased psychometric precision.
#' However, it is also possible to try classical test theory EFA, such as [psych::fa()] (Revelle, 2022).
#' Regarding rotation method, EFA method and rotation should be carefully chosen based on theory or otherwise statistically reasonable.
#' However, it is beneficial to try and compare several rotation options.
#' For,ost, an EFA solution is in adequate if it cannot fit the orthogonal constraints described above.
#'
#' ## Step 2: Item Selection
#' The `modid()` takes in the factor solution from the EFA, assigned to a data frame \emph{x}, and outputs lists (denoted \emph{items}) with suggestions of items to use for the model identification.
#' These lists contain one column for the loadings from each item on the factor of interest, and one column with absolute sum scores for each item calculated from the remaining factor loadings in the model.
#' Each list is sorted with the lowest absolute sum score highest up.
#' Accordingly, the top items in each list are the items that best meet the assumption of orthogonality in the analysis.
#' Therefore, for a three-dimensional model, all else equal, the item highest up in the first list should be used to identify the x-axis, and the item highest up in the second list should be used to identify the y-axis, and so on
#'
#'
#' ## The Model Identification Procedure
#' The `modid()` function uses an iterating model identification procedure that can be user adjusted.
#' In brief, in automatic mode, `modid()` starts by first calculating the ss loadings on all factors \emph{F} in the data frame x and then rearrange the columns in \emph{x}, in decreasing order following the level of strength of the ss loadings.
#' Next, the function creates a list containing factor loadings on the first factor, \emph{f1}, and absolute sum scores of the factor loadings in the remaining factors, i.e., \emph{F-f1}, row-wise.
#' The list is then rearrange in decreasing order based on factor loading strength on \emph{f1}.
#' Items are selected by scaling f1, and using a standard deviation of 0.5 (can be adjusted with the `lower` argument.) as the lower bound criteria for inclusion.
#' That is, starting form the top, rows with raw factor scores and absolut sum scores are extracted until the lower bound is reached.
#' This allows the function to extract more rows in the case empirical factor loadings are similar in strength.
#' The result is recorded as a nested list before the function starts over with the next factor, f2, and so on.
#'
#' For every iteration, the algorithm jumps to the next factor in the EFA model, rearrange rows and extract the strongest loading items.
#' However the absolute sum score is always assessed on the number of factors less than the total number of factors following the order of iteration,
#' That is, iteration 1 use factor loadings from all factors \emph{F-f1}, iteration 2 \emph{F-(f1+f2)}, iteration 3 \emph{F-(f1+f2+f3)}, and so on, when calculating the absolute sum scores.
#'
#'
#' ## Criteria
#' Optimized model identification items should preferably (a) have an absolute sum score of <= .10 and (b) with maximized factor loading on the factor of interest.
#' Of these two criteria, (a) should be given the strongest weight in the selection decision.
#' If these conditions cannot be met, the user is advised to proceed with caution since the loading scores imply that an adequate orthogonal structure may not be empirically attainable.
#' If problems occur, try change the rotation method for the EFA first hand.
#' If this does not help, proceed by increasing the lower bound since this will allow the function to include weaker loading items in the analysis.
#' The upper bound (set with argument `upper`) should not be increased > .1, unless the assumption of orthogonality can be compromised.
#'
#' The user also has the option of overriding the automatic sorting of factor order according to ss loadings, with the argument `fac.order` (see examples section).
#' This can, for instance, be useful in cases where the ss loadings are very similar in strength in the EFA model.
#'
#' ## Limitations
#' The `modid()` function is not limited to three-dimensional analysis and can be used on any number of factors.
#' Although based on suggestions on model identification given by Reckase (2009) for this type of analysis, the function offers some expansions that introduce more precision.
#' The latter foremost consist in incorporating sum of squares in the item selection process (unless the user has not specified otherwise).
#' Experience tells that this is good practice that often leads to better results compared to other options.
#' However, it is important to recognize that the model identification procedure only gives suggestions to the model specification, and there could be situations where the researcher should consider other methods.
#'
#' @return S3 object of class `modid` with lists of items and absolute sum scores, sorted by the latter, and sum of squared factor loadings and frame with raw factor loadings with columns ordered on explained variance (high to low) or according to user settings.
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
#' f <- mirt::mirt(x, 3)
#'
#' # Assign data frame with factor loadings with varimax rotation
#' g <- summary(f, rotate= 'varimax')
#' h <- data.frame(g$rotF)
#'
#' # With psych package and oblimin rotation
#' library(psych)
#' f <- psych::fa(x, nfactors = 3, rotate = "oblimin", residuals = FALSE, SMC = FALSE)
#'
#' # Assign data frame with factor loadings
#' g <- data.frame(f$loadings[,])
#'
#' # Call to modid
#' modid(h)
#'
#' # Call to modid with increased lower bound and decreased upper bound
#' modid(h, lower = 1, upper = .08 )
#'
#' # Override factor order by reversing columns in the original data frame
#'  modid(g, fac.order = c(3,2,1))
#' }
#' @export
modid <- function(x, lower = 0.5, upper = .10, fac.order = NULL){
  if (is.null(fac.order)){
    y <- x[,order(colSums(x^2), decreasing = TRUE)]
  } else {
   if(any(!fac.order == round(fac.order))) stop("Factor order must be indicated with integer values")
    if (any(duplicated(fac.order))) stop("The factor order argument has duplicate elements")
    if (!length(fac.order) == length(x)) stop("The factor order argument has too many indicators")
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
    if (nrow(b)==0) stop ("Model identification failed, try adjusting lower or upper bound")
    b <- b[order(b[,2]),]
    f[[i]] <- b
  }
  if (is.null(fac.order)){
    ss <- sort(colSums(x^2), decreasing = TRUE)
  } else {
    ss <- colSums((x^2)[, fac.order])
  }
  modid <- list(items = f, ss.loadings = ss, loadings = y)
  class(modid) <- "modid"
  modid
}
