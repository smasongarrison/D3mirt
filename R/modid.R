#' modid: A Model identification Helper for D3mirt
#'
#' @description The function `modid()` can assist with the model identification step in the D3mirt procedure. This results in the selection of two items from the item set under investigation that are used when fitting the graded response model with the [`mirt::mirt`] function. The S4 object that `mirt` returns is then used in `dmirt()` from the `D3mirt`package that calculates all necessary estimates for the descriptive 3D IRT analysis.
#'
#' @param x A data frame or matrix with factor loadings.
#' @param head Logical, if output should contain first five rows per factor only. Default is `head = TRUE`.
#'
#' @return Lists of factor loadings and absolute sum scores.
#' @export
#'
#' @details To visualize the D3mirt measurement model in 3D space it is necessary to investigate if it is empirically possible to create the necessary orthogonal axis in the model.
#' For a 3D model, this means that at least two items must be selected. The first item should not show any systematic relation to the remaining two axis, and the second item should not be systematically related to the third axis.
#' In praxis, this implies that item one, orthogonal the y and z axis, will identify the x-axis. Next, item two, orthogonal to the z-axis, will identify the y-axis, while the z-axis remains free.
#'
#'
#' Thus, before using `modid()`, the factor structure must be explored. This can be done using classical test theory exploratory factor methods (EFA), such as [`psych::fa()`].
#' Preferably, the EFA method should be carefully chosen.
#' For instance, since the factors in most psychological methods can be assumed to correlate the `oblimin` rotation is a reasonable choice. However, if the factor structure is ambiguous, a 'varimax' rotation could be an option to consider.
#' Regardless, it is important that the model identification process is made transparent and that the choice of EFA for the model identification is reported.
#'
#'
#' Regarding the function output, `modid()` takes a data frame with factor loadings as input and outputs lists, one list per factor from the EFA.
#' These lists contains one column for the loadings from each item on the factor of interest, and one column with absolute sum scores for each item calculated from the remaining factor loadings in the model.
#' Each list is sorted with the lowest absolute sum score highest up. Accordingly, the items located at the top in each list are the items that best meet the assumption of orthogonality in the given set.
#'
#'
#' The user is advised to select two items from two different factors that can be used to identify the measurement model in 3D space. Preferably, this should be items with absolute sum scores of < .10 and with factor loading > .80.
#' If these conditions cannot be meet the user is advised to proceed with caution since the loading scores implies that an orthogonal structure may not be empirically attainable.
#' However, it is also important to recognize that the `modid` function gives suggestion to the model identification and there can be situations where the output from `modid` can not be used.
#'
#' @examples
#' # Preparation: fit a factor model with number of factors set to 3
#' library(psych)
#' f <- fa(x, nfactors = 3, rotate = "oblimin", residuals = FALSE, SMC=FALSE)
#'
#'
#' # Assign data frame with factor loadings
#' z <- data.frame(f$loadings[,])
#'
#' # Perform identification estimation
#' modid(z)
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
