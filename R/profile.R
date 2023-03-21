#' D3mirt Profile Analysis
#'
#' @description `profile()` is a wrapper function to [mirt::fscores()] (Chalmers, 2012) and use [stats::quantile()] to select respondent by row.
#' @param x A three-dimensional S4 [mirt::mirt] object, fitted using descriptive multidimensional item response theory modeling (Reckase, 2009)
#' @param y A S4 [mirt::mirt] object of the same type as above, or a data frame with respondent's scores.
#' @param column Numeric indicating what column to use regarding the input for y. Default is `column = 1`.
#' @param condition String indicating what logical condition to use for filtering. Options are: `">"`, `"<"`, `">="`, and `"<="`. Default is `condition = >`.
#' @param prob Decimal indicating what probability to use as cut-off. Default is `prob = .75`.
#' @param method The method used to extract respondent factor scores with [mirt::fscores()]. Default is `method = EAP`.
#' @param QMC Logical, if Quasi-Monte Carlo integration should be used for extracting respondents' factor scores from [mirt::fscores()]. Default is `QMC = TRUE`.
#'
#' @return List of factor scores
#' @export
#' @importFrom stats cov
#'
#' @details The returned object can be used to plot respondents' scores with [D3mirt::plot()], using factor scores as coordinates in the graphical output. Please consult the documentation on [mirt::fscores()] for more details and available options regarding the `fscores()` function.
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
#' p <- profile(mod1, y, column = 20, condition = ">", prob = .75)
#'
#'
#' # Plot model with the D3mirt plot function while hiding item vectors
#' plot(x, profiles = p, hide = TRUE)
#' }
profile <- function(x, y = NULL, column = 1, condition = c(">"), prob = .75, method = "EAP", QMC = TRUE){
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop(
      "Package \"mirt\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if(!isS4(x)) warning("x must be S4 mirt object from mirt package")
  m <- as.data.frame(mirt::fscores(x, method = method, full.scores= TRUE, full.scores.SE = FALSE, QMC = QMC), drop = FALSE)
  if (!is.null(y)){
    if (isS4(y)){
      if(column > 3) warning("Column argument set to high, must be 3 or less for S4 objects")
      y <- as.data.frame(mirt::fscores(y, method = method, full.scores= TRUE, full.scores.SE = FALSE, QMC = QMC), drop = FALSE)
      y <- as.matrix(y[,column, drop = FALSE])
    } else {
      if(!is.data.frame(y) && !is.matrix(y)) stop("Input object is not of type data frame or matrix")
      if (column > ncol(y)) stop("Column argument is too high")
      y <- as.matrix(y[,column, drop = FALSE])
      if (!nrow(y)==nrow(m)) stop("Data frames does not have the same number of rows")
    }
    if (condition == ">"){
      m$dummy <- ifelse(y[,1] > stats::quantile(y, prob = prob),1,0)
    }
    else if (condition == "<"){
      m$dummy <- ifelse(y[,1] < stats::quantile(y, prob = prob),1,0)
    }
    else if (condition == ">="){
      m$dummy <- ifelse(y[,1] >= stats::quantile(y, prob = prob),1,0)
    }
    else if (condition == "<="){
      m$dummy <- ifelse(y[,1] <= stats::quantile(y, prob = prob),1,0)
    }
  }
  if (is.null(y)){
    if (condition == ">"){
      m$dummy <- ifelse(m[,column[1]] > stats::quantile(m[,column], prob = prob),1,0)
    }
    else if (condition == "<"){
      m$dummy <- ifelse(m[,column[1]] < stats::quantile(m[,column], prob = prob),1,0)
    }
    else  if (condition == ">="){
      m$dummy <- ifelse(m[,column[1]] >= stats::quantile(m[,column], prob = prob),1,0)
    }
    else  if (condition == "<="){
      m$dummy <- ifelse(m[,column[1]] <= stats::quantile(m[,column], prob = prob),1,0)
    }
  }
  m[m$dummy == 1, ]
}
