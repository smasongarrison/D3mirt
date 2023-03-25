#' D3mirt Model Identification
#'
#' @description `modid()` assists with the model identification of the D3mirt object by indicating what items, from a set or scale, to use for identifying the model to use in the D3mirt analysis.
#'
#' @param x A data frame with factor loadings
#' @param lower The the lower bound for item inclusion based on item factor loadings. Default is `lower = 0.8`.
#' @param upper Value for upper bound for the absolute value criteria. Default is `upper = .10`
#' @param fac.order Optional. Users can override the automatic sorting of factors by manually indicating factor order with integer values, e.g., `c(2,1,3)` to use the second factor (or column) in data frame x first, first factor (or column) in x second, and the third factor (or column) is left untouched.
#' Default is `factor.order = NULL`.
#'
#' @details Before performing descriptive item response theory analysis it is necessary to investigate if the item score patterns fit the `D3mirt` method.
#' For a three-dimensional model, this means that two items must be chosen that can be used to identify the model.
#' More specifically, the first item should not show any systematic relation to the remaining two axes (y and z), and the second item should not be systematically related to the third axis (z), following Reckase's (2009) original specifications.
#' These items are then used to specify the multidimensional graded response model with [`mirt::mirt`] (Chalmers, 2012; see [D3mirt::D3mirt] documentation for more details on model specification).
#'
#' ## Step 1: Explore Data Structure
#' To begin the factor structure must be explored with exploratory factor analysis (EFA).
#' Because `D3mirt` analysis is based on item response theory, it is recommended to use multidimensional item response theory EFA methods, such as the EFA option in [mirt::mirt] (Chalmers, 2012).
#' However, it is also possible to use classical test theory EFA, such as [psych::fa()] (Revelle, 2022).
#' Regardless, the EFA method and rotation should be carefully chosen based on theory or otherwise statistically reasonable.
#'
#' ## Step 2: Item Selection
#' The `modid()` takes in the factor structure from the EFA, assigned to a data frame \emph{x}, and outputs lists (denoted \emph{items}) with suggestions of items to use model identification.
#' These lists contain one column for the loadings from each item on the factor of interest, and one column with absolute sum scores for each item calculated from the remaining factor loadings in the model.
#' Each list is sorted with the lowest absolute sum score highest up.
#'
#'
#' Accordingly, the top items in each list are the items that best meet the assumption of orthogonality in the analysis, given item set or scale in the EFA model.
#' For a three-dimensional model, all else equal, the best item from the first list should be used to identify the x-axis, and the best item in the second list should be used to identify the y-axis.
#' Optimized model identification items should preferably (a) have an absolute sum score of < .10 and (b) a notably strong factor loading on the factor of interest.
#' Of these two criteria, (a) should be given the strongest weight in the selection decision.
#'
#'
#' If these conditions cannot be met, the user is advised to proceed with caution since the loading scores imply that an orthogonal structure may not be empirically attainable.
#' Regardless, because empirical data is noisy it is always necessary to try several options and to compare the outcomes before a final decision is made.
#'
#'
#' ## The Automatic Model Identification Procedure
#' The `modid` function uses an iterating model identification procedure that can be user adjusted.
#' In brief, in automatic mode, `modid` starts by first calculating the ss loadings on all factors \emph{F} in the data frame x and then rearrange the columns in \emph{x}, in decreasing order following the level of strength of the ss loadings.
#' Next, the function calculates the absolute sum scores of the factor loadings in the remaining factors, i.e., \emph{F-f1}, row-wise,
#' and rearrange the data frame based on factor loading strength on the first factor, \emph{f1}.
#' Items are selected by scaling f1, and using a standard deviation of 0.5 as the lower bound criteria.
#' That is, starting form the top, rows with raw factor scores and absolut sum scores are extracted until the lower bound is reached.
#' This allows the function to extract more rows in the case empirical factor loadings are very similar in strength.
#' The lower bound can be adjusted by the user with the `st.d` argument.
#' The result is recorded in a list before the function starts over with the next factor, f2, and so on.
#'
#' For every iteration, the algorithm jumps to the next factor in the EFA model, rearrange and extract the head.
#' However the absolute sum score is always assessed on the number of factors less than the total number of factors, following the order of iteration,
#' That is, iteration 1 use factor loadings from all factors \emph{F-f1}, iteration 2 \emph{F-(f1+f2)}, iteration 3 \emph{F-(f1+f2+f3)}, and so on, when calculating the absolute sum scores.
#'
#' The user has the option of overriding the automatic sorting of factor order according to ss loadings, with the argument `factor.order` (see examples section).
#' This can, for instance, be useful in cases where the ss loadings are very similar in strength in the EFA model, as well as to try to increase the head argument.
#'
#' ## Limitations
#' The `modid` function is not limited to three-dimensional analysis, but can be used on any number of factors.
#' The user must therefore make sure that the input data frame is of correct format and content.
#' Although based on suggestions on model identification given by Reckase (2009) for this type of analysis the function offers an expansion that gives more precision.
#' The latter foremost includes the introduction of automatization in the selection process, and in taking the sum of squares into consideration during item selection (unless the user has not specified otherwise).
#' Experience tells that ordering factors by strength during item selection is good practice that often leads to better results compared to other options.
#' However, it is important to recognize that the automatic model identification procedure only gives suggestions, and there could be situations where the researcher should consider other methods.
#'
#' @return Lists of items and absolute sum scores, sorted by the latter, and sum of squared factor loadings and frame with raw factor loadings with columns ordered by explained variance (high to low) or according to user settings.
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
#'
#' # With psych package and oblimin rotation
#' library(psych)
#' f <- fa(x, nfactors = 3, rotate = "oblimin", residuals = FALSE, SMC = FALSE)
#'
#' # Assign data frame with factor loadings
#' g <- data.frame(f$loadings[,])
#'
#' # call to modid with an increased head argument
#' modid(h, head = 10)
#'
#' # Override factor order by reversing columns in the original data frame
#'  modid(g, factor.order = c(3,2,1))
#' }
#' @export
modid <- function(x, lower = 0.8, upper = .10, fac.order = NULL){
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
