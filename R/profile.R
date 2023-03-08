#' profile: Selection of Respondents for Plotting
#' @description `profile()` is a wrapper function to `fscores()` from the `mirt` package. Respondents factor scores are selected row wise based on the logical condition set by user. Factor scores can then be used to plot respondents as spheres in the 3D `dmirt` object. Please consult documentation on [mirt::fscores()] for more details and available options regarding the `fscores()` function.
#' @param x S4 mirt object.
#' @param y S4 mirt object or data frame with respondents scores.
#' @param column Select what column in the input for y to use for selection of respondent rows. Default is 1.
#' @param condition String input on what logical condition to use for filtering respondents. Options are: ">", "<", ">=", and "<=" and default is set to ">".
#' @param prob Indicate what probability to use as cut off, default is .75.
#' @param method The method used to extract respondent factor scores from the S4 mirt object. Default is "EAP".
#' @param full.scores Logical, should full scores from the `fscores()` function be printed? Default is TRUE.
#' @param full.scores.SE Logical, should standard errors from the `fscores()` function be printed? Default is FALSE.
#' @param QMC Integration method for extracting respondents factor scores from the `fscores()` function. Default is "QMC".
#'
#' @return List of factor scores from the `mirt` S4 object.
#' @export
#'
#' @examples
#' # x is the fitted three dimensional S4 mirt object
#' # y is a data frame containing scores from the same respondents
#' p <- profile(x, y, column = 20, condition = ">", prob = .75)
#' # To plot with the `D3mirt` plot function while hiding item vectors
#' plot(x, profiles = p, hide = TRUE)
profile <- function(x, y = NULL, column = 1, condition = c(">"), prob = .75, method = "EAP", full.scores = TRUE, full.scores.SE = FALSE, QMC = TRUE){
  if(!isS4(x)) warning("x must be S4 mirt object from mirt package")
  m <- as.data.frame(fscores(x, method = method, full.scores= full.scores, full.scores.SE = full.scores.SE, QMC=QMC), drop = FALSE)
  if (!is.null(y)){
    if (isS4(y)){
      if(column > 3) warning("Column argument set to high, must be 3 or less for S4 objects")
      y <- as.data.frame(fscores(y, method = method, full.scores= full.scores, full.scores.SE = full.scores.SE, QMC=QMC), drop = FALSE)
      y <- as.matrix(y[,column, drop = FALSE])
    } else {
      if(!is.data.frame(y) && !is.matrix(y)) stop("Input object is not of type data frame or matrix")
      y <- as.matrix(y[,column, drop = FALSE])
    }
    if (condition == ">"){
      m$dummy <- ifelse(y[,1] > quantile(y, prob = prob),1,0)
    }
    else if (condition == "<"){
      m$dummy <- ifelse(y[,1] < quantile(y, prob = prob),1,0)
    }
    else if (condition == ">="){
      m$dummy <- ifelse(y[,1] >= quantile(y, prob = prob),1,0)
    }
    else if (condition == "<="){
      m$dummy <- ifelse(y[,1] <= quantile(y, prob = prob),1,0)
    }
  }
  if (is.null(y)){
    if (condition == ">"){
      m$dummy <- ifelse(m[,column[1]] > quantile(m[,column], prob = prob),1,0)
    }
    else if (condition == "<"){
      m$dummy <- ifelse(m[,column[1]] < quantile(m[,column], prob = prob),1,0)
    }
    else  if (condition == ">="){
      m$dummy <- ifelse(m[,column[1]] >= quantile(m[,column], prob = prob),1,0)
    }
    else  if (condition == "<="){
      m$dummy <- ifelse(m[,column[1]] <= quantile(m[,column], prob = prob),1,0)
    }
  }
  m[m$dummy == 1, ]
}
