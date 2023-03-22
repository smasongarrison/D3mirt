#' D3mirt S3 Object
#'
#' @description Descriptive multidimensional item response theory modeling (Reckase, 2009, 1985, Reckase & McKinley, 1991), restricted to three dimensions.
#' The `dmirt()` function takes in a data frame of factor slopes (\emph{a}) and difficulty parameters (\emph{d}) from a three-dimensional graded response model (Samejima, 1969),
#' fitted with [mirt::mirt] (Chalmers, 2012). The function returns an S3
#' object containing dmirt estimates that can be graphically displayed with [D3mirt::plot()].
#'
#' @param x Data frame with rows for items and columns for model parameters. The number of columns must be ≥ 4, i.e., three columns for \emph{a} parameters and at least one column for \emph{d} parameters.
#' @param constructs Optional. Nested lists with integers indicating construct. Default is `constructs = NULL`.
#'
#' @return S3 object with lists of \emph{a} and \emph{d} parameters, multidimensional discrimination (MDISC), multidimensional item difficulty (MDIFF), direction cosines and degrees for vector angles, construct lists, and vector coordinates.
#'
#'
#' @details Model parameters for the multidimensional graded response model must be estimated before using `dmirt()` (see examples below) and follow the necessary model specification and limitations.
#' This includes that items must be set to load on all factors in the graded response model and that the number of factors must be three.
#'
#' # Constructs
#' The user has the option of including constructs in the estimation, by creating one or more nested lists that indicate what items belong to what construct (see the examples section).
#' From this, the `dmirt()` function calculates new direction cosines for the constructs by adding and normalizing the direction cosines for the items contained in each construct list.
#' The construct vector arrows can contribute to the analysis by visualizing the average direction for a subset set of items.
#'
#' # Scaling of Vector Arrows
#' Regarding plotting, the `dmirt()` function returns vector coordinates estimated with and without the MDISC as a scalar for arrow length. If the MDISC is not used, all vector arrows are scaled to unit length.
#' This can help reduce clutter in the graphical output.
#'
#' # Model Identification
#' An important part of descriptive multidimensional item response theory modeling is the model identification that is performed before fitting the three-dimensional graded response model.
#' An example based on a 10-item set in which the model has already been identified is given below.
#' For more information on model identification, please read the documentation included under `D3mirt::modid()`.
#'
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Reckase, M. D. (1985). The Difficulty of Test Items That Measure More Than One Ability. \emph{Applied Psychological Measurement, 9}(4), 401-412–412. https://doi-org.ezp.sub.su.se/10.1177/014662168500900409
#' @references Reckase, M. D., & McKinley, R. L. (1991). The Discriminating Power of Items That Measure More Than One Dimension. \emph{Applied Psychological Measurement, 15}(4), 361-373–373. https://doi-org.ezp.sub.su.se/10.1177/014662169101500407
#' @references Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. \emph{Psychometrika 34}, 1–97. https://doi.org/10.1007/BF03372160
#'
#' @examples
#' \dontrun{
#' # Preparation: Fitting a three-dimensional graded response model
#' # Example uses a scale with 10 items in total, named I_01...I_10
#' # In the example below item I_01 and item I_10 have been selected to identify the model
#' # All items in the set are specified to load on all three factors (F1 to F3)
#' The START and FIXED commands are used to locate the orthogonal axis in the model
#' library(mirt)
#' spec <- ' F1 = 1-10
#'           F2 = 1-10
#'           F3 = 1-10
#'
#'           START=(I_05,a2,0)
#'           START=(I_05,a3,0)
#'
#'           START=(I_10,a3,0)
#'
#'           FIXED=(I_05,a2)
#'           FIXED=(I_05,a3)
#'
#'           FIXED=(I_10,a3) '
#'
#'
#' mod1 <- mirt(x, spec, itemtype = 'graded', SE = TRUE, method = 'QMCEM')
#'
#' # Assign data frame with factor loadings (columns 1-3)
#' # and difficulty parameters (columns 4-7) from mod1
#' d <- data.frame(mirt::coef(mod1, simplify=TRUE)$'items'[,1:7])
#'
#' # Estimation with dmirt(), including nested lists for two constructs
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- dmirt(d, c)
#' }
#' @export
D3mirt <- function(x, constructs = NULL){
  # Warning for format
  if(!is.data.frame(x) && !is.matrix(x)) stop("Input object is not of type data frame or matrix")
  if(ncol(x) < 4) stop("Data frame must have at least 4 columns")
  x <- as.matrix(x, drop = FALSE)
  a <- x[,1:3, drop = FALSE]
  ndiff <- ncol(x)-3
  if (ndiff == 1){
    mdiff <- x[,4, drop = FALSE]
  } else {
    mdiff <- x[,(4):(3+ndiff), drop = FALSE]
  }
  mdisc <- sqrt(rowSums(a^2))
  md <- mdisc%*%matrix(rep(1,3), nrow=1, ncol=3)
  dcos <- as.matrix(a/md, ncol = 3)
  deg <- acos(dcos)*(180/pi)
  vector1 <- NULL
  vector2 <- NULL
  for (i in seq_len(ndiff)){
    d <- mdiff[,i]
    distance <- -d/mdisc
    xyz <- distance*dcos
    uvw1 <- mdisc*dcos+xyz
    uvw2 <- dcos+xyz
    vec1 <- do.call(rbind,list(xyz,uvw1))[order(sequence(sapply(list(xyz,uvw1),nrow))),]
    vec2 <- do.call(rbind,list(xyz,uvw2))[order(sequence(sapply(list(xyz,uvw2),nrow))),]
    vector1 <- matrix(rbind(vector1,vec1), ncol = 3)
    vector2 <- matrix(rbind(vector2,vec2), ncol = 3)
  }
  if (!is.null(constructs)){
    if(!is.list(constructs)) stop("Construct object must be of type list")
    if(!any(sapply(constructs, class) == "list")) stop("The constructs must be nested lists")
    con <- NULL
    cdeg <- NULL
    ncos <- NULL
    for (i in seq_along(constructs)){
      l <- unlist(constructs[i])
      cos <- NULL
      cdcos <- NULL
      for (i in seq_along(l)){
        n <- l[i]
        m <- dcos[n,]
        cos <- matrix(rbind(cos,m), ncol = 3)
      }
      cscos <- matrix(colSums(cos), ncol = 3)
      cdcos <- 1/sqrt(rowSums(cscos^2))*cscos
      maxnorm <- (1.1*max(vector1))*cdcos
      minnorm <- (0.6*min(vector1))*cdcos
      con <- matrix(rbind(con,rbind(minnorm, maxnorm)), ncol = 3)
      ncos <- matrix(rbind(ncos,cdcos), ncol = 3)
      cdeg <- matrix(rbind(cdeg,(acos(cdcos)*(180/pi))), ncol = 3)
    }
  }
  a <- as.data.frame(a, drop = FALSE)
  sapply(ncol(a), function(x){
    colnames(a) <- paste("a", 1:x, sep = "")})
  mdisc <- as.data.frame(mdisc, drop = FALSE)
  colnames(mdisc) <- c("MDISC")
  dcos <- as.data.frame(dcos)
  colnames(dcos) <- c("D.Cos X", "D.Cos Y", "D.Cos Z")
  deg <- as.data.frame(deg, drop = FALSE)
  colnames(deg) <- c("Deg.X", "Deg.Y", "Deg.Z")
  mdiff <- as.data.frame(mdiff, drop = FALSE)
  sapply(ncol(mdiff), function(x){
    colnames(mdiff) <- paste("d", 1:x, sep = "")})
  if (ndiff == 1){
    dir.vec <- vector1
    scal.vec <- vector2
  } else {
    dir.vec <- split.data.frame(vector1, cut(seq_len(nrow(vector1)), ndiff))
    scal.vec <- split.data.frame(vector2, cut(seq_len(nrow(vector2)), ndiff))
  }
  if (!is.null(constructs)){
    ncos <- as.data.frame(ncos, drop = FALSE)
    colnames(ncos) <- c("D.Cos X","D.Cos Y", "D.Cos Z")
    cdeg <- as.data.frame(cdeg, drop = FALSE)
    colnames(cdeg) <- c("Deg.X", "Deg.Y", "Deg.Z")
    D3mirt <- list(loadings = a, mdisc = mdisc, dir.cos = dcos, degrees = deg, mdiff = mdiff,
                  dir.vec = dir.vec, scal.vec = scal.vec, c = constructs, c.dir.cos = ncos ,c.degrees = cdeg, c.vec = con)
  } else {
    D3mirt <- list(loadings = a, mdisc = mdisc, dir.cos = dcos, degrees = deg, mdiff = mdiff,
                  dir.vec = dir.vec, scal.vec = scal.vec)
  }
  class(D3mirt) <- "D3mirt"
  D3mirt
}
