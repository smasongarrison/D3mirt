#' dmirt: Estimation of D3mirt R3 Object
#'
#' @description The `dmirt`function creates an R3 object that contains all necessary estimates for the three dimensional measurement model. Input consists of a data frame that holds factor loadings and difficulty parameters for all items in the scale or set. Importantly, rows must be items and number of columns must be ≥ 4, i.e., three columns for factor loadings and at least one column for item difficulty. The vector arrows that can be plotted with the `plot()` function represents item characteristics the three dimensional theta space. More specifically, item location shows the items three dimensional level of difficulty. If Likert items are used, each item will have multiple level of difficulty and can therefore be said to show the difficulty range of an item. Moreover, the angle of the vector arrows shows the direction of optimal discrimination in the model. In turn, this indicates what traits, one up to three, the item can be said to describe. Lastly, the length of each vector arrows shows the strength of discrimination, in which longer arrows indicate higher discrimination. Short vector arrows are therefore signs of model violations. The user has the option of including constructs in the estimation, by creating one or more nested lists that states what items belongs to what construct (see the examples section). From this, the `dmirt`function extract direction vectors by averaging the direction cosines for the items contained in each construct list. The constructs vector arrows, therefore, can contribute by visualizing the average direction for at larger set of items. However, the lenght of the construct arrow has no meaning and can be set by the user by changing the `max.norm` and `min.norm` arguments.
#' @param x data frame or matrix
#' @param constructs Nested list or lists with integers indicating what items are part of what construct. Default is NULL.
#' @param max.norm Scaling factor for the length of the construct arrow above center in the model. Default is set to 1.1.
#' @param min.norm Scaling factor for the length of the construct arrow below center in the model. Default is set to 0.6.
#'
#' @return R3 object that contain lists with estimates.
#' @export
#'
#' @examples
#' # Fitting three dimensional mirt S4 object with the graded response model as item model
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
#' d <- data.frame(mirt::coef(mod.1, simplify=T)$'items'[,1:7])
#'
#' # The example below use constructs in the `dmirt´estimation
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- dmirt(d, c)
dmirt <- function(x, constructs = NULL, max.norm = 1.1, min.norm = 0.6){
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
      maxnorm <- (max.norm*max(vector))*cdcos
      minnorm <- (min.norm*min(vector))*cdcos
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
