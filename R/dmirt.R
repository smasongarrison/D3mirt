#' dmirt: Item Analysis using Descriptive 3D Item Response Theory
#'
#' @description The `dmirt()`function outputs an S3 object that contains all necessary estimates for plotting a three dimensional measurement model.
#' @param x Data frame or matrix
#' @param constructs Nested list or lists with integers indicating what items are part of what construct. Default is `constructs = NULL`.
#'
#' @return S3 object
#' @export
#'
#' @details Input consists of a data frame that holds factor loadings and difficulty parameters for all items in the scale or item set. Importantly, rows must be items and number of columns must be ≥ 4, i.e., three columns containing factor loadings and at least one column for item difficulty. The output include lists of tables for factor loadings, multidimensional discrimination (MDISC), multidimensional item difficulty (MDIFF), direction cosines and degrees for vector angles. The output can then also be visualized in 3D space using the `plot()` function.
#'
#' The user has the option of including constructs in the estimation, by creating one or more nested lists that indicate what items belongs to what construct (see the examples section). From this, the `dmirt()`function calculates construct direction cosines by adding and normalizing the direction cosines for the items contained in each construct list. In so doing, the constructs vector arrows can contribute to the analysis by visualizing the average direction for a subset set of items. However, the length of the construct arrow is arbitrary and can be set by the user by changing the `max.norm` and `min.norm` arguments.
#'
#' An important part of the analytically process is the model identification and the fitting of the multidimensional graded response model with the [`mirt::mirt()`] function. This is done prior using `dmirt()`. An example based on a 10 item set in which the model has already been identified is given below. In short, all items in the set is specified to load on all three factor (F1 to F3). The START and FIXED command are used to create the orthogonal axis by stating that the first item is orthogonal to the the y and z-axis (a2 ans a3 respectively) and that item two is orthogonal only to the z-axis (a3).
#' For more information on model identification please study the documentation included under `modid()`.
#'
#'
#' @examples
#' # Preparation: Fitting a three dimensional graded response model
#'
#' library(mirt)
#' spec <- ' F1 = 1-10
#'           F2 = 1-10
#'           F3 = 1-10
#'
#'           START=(I_05,a2,0)
#'           START=(I_05,a3,0)
#'
#'           START=(I_30,a3,0)
#'
#'           FIXED=(I_05,a2)
#'           FIXED=(I_05,a3)
#'
#'           FIXED=(I_30,a3) '
#'
#'
#' mod1 <- mirt(x, spec, itemtype='graded', SE=T, method = 'QMCEM')
#'
#' # Assign data frame with factor loadings (column 1-3) and difficulty parameters (column 4-7) from mod1
#' d <- data.frame(mirt::coef(mod1, simplify=T)$'items'[,1:7])
#'
#' # Estimation with dmirt(), in this case including nested lists for constructs
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- dmirt(d, c)
dmirt <- function(x, constructs = NULL){
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
  vector <- NULL
  for (i in seq_len(ndiff)){
    d <- mdiff[,i]
    distance <- -d/mdisc
    xyz <- distance*dcos
    uvw <- mdisc*dcos+xyz
    vec <- do.call(rbind,list(xyz,uvw))[order(sequence(sapply(list(xyz,uvw),nrow))),]
    vector <- matrix(rbind(vector,vec), ncol = 3)
  }
  if (!is.null(constructs)){
    if(!is.list(constructs)) stop("Construct object must be of type list")
    if(!any(sapply(constructs, class) == "list")) stop("The constructs argument must formatted as nested lists")
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
      maxnorm <- (1.1*max(vector))*cdcos
      minnorm <- (0.6*min(vector))*cdcos
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
  colnames(deg) <- c("X°", "Y°", "Z°")
  mdiff <- as.data.frame(mdiff, drop = FALSE)
  sapply(ncol(mdiff), function(x){
    colnames(mdiff) <- paste("d", 1:x, sep = "")})
  if (ndiff == 1){
    dir.vec = vector
  } else {
    dir.vec = split.data.frame(vector, cut(seq_len(nrow(vector)), ndiff))
  }
  if (!is.null(constructs)){
    ncos <- as.data.frame(ncos, drop = FALSE)
    colnames(ncos) <- c("D.Cos X","D.Cos Y", "D.Cos Z")
    cdeg <- as.data.frame(cdeg, drop = FALSE)
    colnames(cdeg) <- c("X°", "Y°", "Z°")
    dmirt <- list(loadings = a, mdisc = mdisc, dir.cos = dcos, degrees = deg, mdiff = mdiff,
                  dir.vec = dir.vec, c = constructs, c.dir.cos = ncos ,c.degrees = cdeg, c.vec = con)
  } else {
    dmirt <- list(loadings = a, mdisc = mdisc, dir.cos = dcos, degrees = deg, mdiff = mdiff,
                  dir.vec = dir.vec, ndiff)
  }
  class(dmirt) = "dmirt"
  dmirt
}
