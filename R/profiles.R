#' D3mirt Profile Analysis
#'
#' @description `profile()` is a wrapper function to [mirt::fscores()] (Chalmers, 2012) and use [stats::quantile()] to select respondent by row.
#' @param x A three-dimensional S4 [mirt::mirt] object, fitted using descriptive multidimensional item response theory modeling (Reckase, 2009)
#' @param y Optional. A S4 [mirt::mirt] object of the same type as above, or a data frame with respondent's scores.
#' @param z Optional. A data frame with respondent's scores.
#' @param column Numeric indicating what columns to use regarding the input for y. Default is `column = c(1)`.
#' @param condition String indicating what logical conditions to use for filtering. Options are: `">"`, `"<"`, `">="`, and `"<="`. Default is `condition = ">"`.
#' @param probs Decimal indicating what probability to use as cut-off. Default is `probs = c(.75)`.
#' @param method The method used to extract respondent factor scores with [mirt::fscores()]. Default is `method = EAP`.
#' @param QMC Logical, if Quasi-Monte Carlo integration should be used for extracting respondents' factor scores from [mirt::fscores()]. Default is `QMC = TRUE`.
#'
#'
#' @importFrom stats cov
#'
#' @details The function can be used to extract respondent using [stats::quantile] and logical operators `">"`, `"<"`, `">="`, and `"<="`.
#' The returned object can be used to plot respondents' scores with [D3mirt::plot()], using factor scores as coordinates in the graphical output.
#' However, selection of respondents can be achieved in many different ways and the `profile()` function is therefore very limited in this sense.
#'
#'
#' Selection of respondents for `plotD3mirt` only require that respondents' factor scores from the current model is extracted with `fscores` (for coordinates of spheres),
#' and that respondent row identity in the `fscroes()`output is the same as in the external data frame.
#' Another option is to select respondents from the `fscores() output itself.
#'
#'
#' Please consult the documentation on [mirt::fscores()] for more details and available options regarding the `fscores()` function.
#'
#' @return List of factor scores
#'
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#'
#' @examples
#' \dontrun{
#' # mod1 is the fitted three dimensional S4 mirt object
#' # y is a data frame containing scores from the same respondents
#' # Condition: scores >.75, of all scores in column 20 in data frame y
#' p <- profiles(mod1, y, column = 20, condition = ">", prob = .75)
#'
#'
#' # Plot model with the D3mirt plot function while hiding item vectors
#' plot(x, profiles = p, hide = TRUE)
#' }
#' @export
profiles <- function(x, y = NULL, z = NULL, column = c(1), condition = c(">"), probs = c(.25), method = "EAP", QMC = TRUE){
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop(
      "Package \"mirt\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if(!isS4(x)) stop("x must be S4 mirt object from mirt package")
  m <- data.frame(mirt::fscores(x, method = method, full.scores= TRUE, full.scores.SE = FALSE, QMC = QMC))
  if (!is.null(y) && is.null(z)){
    if (isS4(y)){
      if(column > 3) stop("Column argument set to high, must be 3 or less for S4 objects")
      y <- data.frame(mirt::fscores(y, method = method, full.scores= TRUE, full.scores.SE = FALSE, QMC = QMC))
      y <- as.matrix(y[,column[1], drop = FALSE])
    } else {
      if (column > ncol(y)) stop("Column argument is too high")
      y <- as.matrix(y[,column[1], drop = FALSE])
    }
    if (condition[1] == ">"){
      m$dummy <- ifelse(y[,1] > stats::quantile(y, probs= probs[1]),1,0)
    }
    else if (condition[1] == "<"){
      m$dummy <- ifelse(y[,1] < stats::quantile(y, probs= probs[1]),1,0)
    }
    else if (condition[1] == ">="){
      m$dummy <- ifelse(y[,1] >= stats::quantile(y, probs= probs[1]),1,0)
    }
    else if (condition[1] == "<="){
      m$dummy <- ifelse(y[,1] <= stats::quantile(y, probs= probs[1]),1,0)
    }
  }
  if (!is.null(y) && !is.null(z)){
    if (isS4(y)){
      if(!isS4(y)) stop("y must be S4 mirt object from mirt package")
      y <- data.frame(mirt::fscores(y, method = method, full.scores= TRUE, full.scores.SE = FALSE, QMC=QMC))
      y <- as.matrix(y[,column[1], drop = FALSE])
      z <- as.matrix(z[,column[2], drop = FALSE])
    } else {
      y <- as.matrix(y[,column[1], drop = FALSE])
      z <- as.matrix(z[,column[2], drop = FALSE])
    }
    if (condition[1] == ">" && condition[2] ==">"){
      m$dummy <- ifelse(y[,1] > stats::quantile(y, probs= probs[1]) & z[,1] > stats::quantile(z, probs = probs[2]),1,0)
    }
    else if  (condition[1] == ">" && condition[2] =="<"){
      m$dummy <- ifelse(y[,1] > stats::quantile(y, probs= probs[1]) & z[,1] < stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == ">" && condition[2] ==">="){
      m$dummy <- ifelse(y[,1] > stats::quantile(y, probs= probs[1]) & z[,1] >= stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == ">" && condition[2] =="<="){
      m$dummy <- ifelse(y[,1] > stats::quantile(y, probs= probs[1]) & z[,1] <= stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == "<" && condition[2] ==">"){
      m$dummy <- ifelse(y[,1] < stats::quantile(y, probs= probs[1]) & z[,1] > stats::quantile(z, probs = probs[2]),1,0)
    }
    else if  (condition[1] == "<" && condition[2] =="<"){
      m$dummy <- ifelse(y[,1] < stats::quantile(y, probs= probs[1]) & z[,1] < stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == "<" && condition[2] ==">="){
      m$dummy <- ifelse(y[,1] < stats::quantile(y, probs= probs[1]) & z[,1] >= stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == "<" && condition[2] =="<="){
      m$dummy <- ifelse(y[,1] < stats::quantile(y, probs= probs[1]) & z[,1] <= stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == ">=" && condition[2] ==">"){
      m$dummy <- ifelse(y[,1] >= stats::quantile(y, probs= probs[1]) & z[,1] > stats::quantile(z, probs = probs[2]),1,0)
    }
    else if  (condition[1] == ">=" && condition[2] =="<"){
      m$dummy <- ifelse(y[,1] >= stats::quantile(y, probs= probs[1]) & z[,1] < stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == ">=" && condition[2] ==">="){
      m$dummy <- ifelse(y[,1] >= stats::quantile(y, probs= probs[1]) & z[,1] >= stats::quantile(z, probs= probs[2]),1,0)
    }
    else if (condition[1] == ">=" && condition[2] =="<="){
      m$dummy <- ifelse(y[,1] >= stats::quantile(y, probs= probs[1]) & z[,1] <= stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == "<=" && condition[2] ==">"){
      m$dummy <- ifelse(y[,1] <= stats::quantile(y, probs= probs[1]) & z[,1] > stats::quantile(z, probs = probs[2]),1,0)
    }
    else if  (condition[1] == "<=" && condition[2] =="<"){
      m$dummy <- ifelse(y[,1] <= stats::quantile(y, probs= probs[1]) & z[,1] < stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == "<=" && condition[2] ==">="){
      m$dummy <- ifelse(y[,1] <= stats::quantile(y, probs= probs[1]) & z[,1] >= stats::quantile(z, probs = probs[2]),1,0)
    }
    else if (condition[1] == "<=" && condition[2] =="<="){
      m$dummy <- ifelse(y[,1] <= stats::quantile(y, probs= probs[1]) & z[,1] <= stats::quantile(z, probs = probs[2]),1,0)
    }
  }
  if (is.null(y) && is.null(z)){
    if (condition[1] == ">"){
      m$dummy <- ifelse(m[,column[1]] > stats::quantile(m[,column[1]], probs = probs),1,0)
    }
    else if (condition[1] == "<"){
      m$dummy <- ifelse(m[,column[1]] < stats::quantile(m[,column[1]], probs = probs),1,0)
    }
    else  if (condition[1] == ">="){
      m$dummy <- ifelse(m[,column[1]] >= stats::quantile(m[,column[1]], probs = probs),1,0)
    }
    else  if (condition[1] == "<="){
      m$dummy <- ifelse(m[,column[1]] <= stats::quantile(m[,column[1]], probs = probs),1,0)
    }
  }
  m[m$dummy == 1, ]
}
cov
