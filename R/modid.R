#' D3mirt Model Identification
#'
#' @description The function `modid()` can assist with the model identification of the D3mirt object.
#' The model identification procedure in descriptive multidimensional item repose theory consists of the user selecting two items from the item set or scale that best meet the assumption of orthogonality (Reckase, 2009).
#'  These items are then used for setting the orthogonal axes when fitting the graded response model with [`mirt::mirt`] (Chalmers, 2012).
#'
#' @param x A data frame with factor loadings
#' @param head Logical, if output should contain first five rows per factor only. Default is `head = TRUE`.
#'
#' @return Lists of factor loadings and absolute sum scores, sorted by the latter.
#'
#' @details Before performing descriptive item response theory analysis it is necessary to investigate if the item score patterns can be used to create empirically valid orthogonal axes in the model.
#' For a three-dimensional model, this entails that at least two items must be selected.
#' The first item should not show any systematic relation to the remaining two axes, and the second item should not be systematically related to the third axis.
#' In praxis, this implies that item one, orthogonal to the y and z-axis, will identify the x-axis. Item two, orthogonal to the z-axis, will identify the y-axis, while the z-axis remains free.
#'
#'
#' Thus, to begin the factor structure must be explored. This can be done using classical test theory exploratory factor methods (EFA), such as [psych::fa()] (Revelle, 2022). assuming three factors.
#' Preferably, the EFA method should be carefully chosen based on theory or otherwise statistically reasonable.
#'
#'
#' Next, the `modid()` takes the factor structure assigned to a data frame, and outputs lists, one list per factor from the EFA.
#' These lists contain one column for the loadings from each item on the factor of interest, and one column with absolute sum scores for each item calculated from the remaining factor loadings in the model.
#' Each list is sorted with the lowest absolute sum score highest up.
#' Accordingly, the top items are the items that best meet the assumption of orthogonality in the given item set or scale.
#'
#'
#' Regarding item selection, model identification items should preferably have an absolute sum score of < .10 and factor loading > .80.
#' If these conditions cannot be met, the user is advised to proceed with caution since the loading scores imply that an orthogonal structure may not be empirically attainable.
#' However, it is also important to recognize that the `modid` function gives suggestions to the model identification and there could be situations where the researcher would need to use other methods.
#'
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Revelle W (2022). \emph{psych: Procedures for Psychological, Psychometric, and Personality Research}. Northwestern University, Evanston, Illinois. R package version 2.2.9, https://CRAN.R-project.org/package=psych.
#'
#' @examples
#' \dontrun{
#' # Preparation: Fit a three-factor EFA model
#' library(psych)
#' f <- fa(x, nfactors = 3, rotate = "oblimin", residuals = FALSE, SMC = FALSE)
#'
#'
#' # Assign data frame with factor loadings from EFA
#' z <- data.frame(f$loadings[,])
#'
#' # Perform identification estimation
#' modid(z)
#' }
#' #' @export
modid <- function(x, head = TRUE){
  if(!is.data.frame(x) && !is.matrix(x)) stop("Input object is not of type data frame or matrix")
  f <- NULL
  for (i in seq(ncol(x))){
    v <- data.frame(x[,i, drop = FALSE])
    colnames(v) <- paste("F", i, sep = "")
    ABS <- data.frame(rowSums(abs(x[,-i, drop = FALSE])))
    colnames(ABS) <- paste("ABS", i, sep = "")
    a <- data.frame(cbind(v,ABS))
    a <- a[order(a$ABS),]
    if (head == TRUE){
      a <- head(a)
    }
    f[[i]] <- a
  }
  print(f)
}
