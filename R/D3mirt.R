#' 3D DMIRT Modeling
#'
#' @description Descriptive multidimensional item response theory modeling (DMIRT; Reckase, 2009, 1985, Reckase & McKinley, 1991), for dichotomous and polytomous items in three dimensions.
#'
#' @param x Data frame with rows for items and columns for model parameters. The number of columns must be ≥ 4, i.e., three columns for slope (\emph{a}) parameters and at least one column for difficulty (\emph{d}) parameters.
#' @param constructs Optional. Nested lists with integer values indicating construct. Default is `constructs = NULL`.
#'
#' @details The `D3mirt()` function takes in a data frame of factor slopes and difficulty parameters from a three-dimensional graded response model and returns an S3 object containing estimates that can be graphed as vector arrows with [D3mirt::plotD3mirt].
#'
#' # Model Identification
#' Importantly, model parameters from the multidimensional graded response model must be assessed prior to using `D3mirt()` (for example with [mirt::mirt] (Chalmers, 2012); see examples section below).
#' This includes that the multidimensional graded response model must follow the DMIRT model specification pattern; in brief, all items must be set to load on all factors in the multidimensional model.
#' The `D3mirt`package contains the `modid()` function to help with the model identification process. For more information on model identification, see documentation regarding [D3mirt::modid].
#'
#' # Within Multidimensional Modeling
#' In DMIRT analysis, also called within multidimensional modeling, it is assumed that items in a multidimensional trait space can measure single or multiple latent abilities (Reckase, 2009, 1985, Reckase & McKinley, 1991).
#' The method is said to be descriptive because the estimates describes item characteristics when more than one latent dimension is used in the analysis.
#' Under the assumption of within-dimensionality, the two-parameter graded response model can be used to extract two-parameter multidimensional item characteristics.
#' These include a single multidimensional discrimination (MDISC) parameter and a multidimensional difficulty (MDIFF) index for each item.
#' The MDISC  \eqn{A_i} for item \eqn{i} represents the highest level of discrimination the item \eqn{i} can achieve located in a multidimensional theta space with \eqn{m} number of dimensions and \eqn{a_{ik}} item slope parameters.
#'
#' \deqn{MDISC=A_i=\sqrt{\sum_{k=1}^m a^2_{ik}}}{%
#' MDISC=A_i=\sqrt{\sum_{k=1}^m a^2_{ik}}}
#'
#' Just as in unidimensional modeling, the  \eqn{A_i} indicates the direction, as seen from the origin of the model, to the point of maximum slope of the item response surface.
#' The slope is, similarly to the unidimensional case, assessed as \eqn{A_i/4} (omitted in the equation above).
#'
#' The item angle orientation is set by taking the direction cosines, using linear algebra terms, of \eqn{a_{il}}, i.e., the slope values of item \eqn{i} on coordinate axis \eqn{l}.
#'
#' \deqn{a_{il}= cos^{-1}\left(\frac{a_{il}}{\sqrt{\sum_{k=1}^m a^2_{ik}}}\right)}{%
#' a_{il}= cos^{-1}\left(\frac{a_{il}}{\sqrt{\sum_{k=1}^m a^2_{ik}}}\right)}
#'
#' The resulting direction vector is a characteristic of the item that describes the angular orientation of an item in a multidimensional theta space.
#'
#' The multidimensional version of the difficulty parameter, \eqn{B_i}, for item \eqn{i} is defined as the negative intercept \eqn{d_i} divided by the MDISC.
#'
#' \deqn{MDIFF=B_i= \frac{-d_i}{\sqrt{\sum_{k=1}^m a^2_{ik}}}}{%
#' \frac{-d_i}{\sqrt{\sum_{k=1}^m a^2_{ik}}}}
#'
#' The MDIFF is interpreted similarly as the difficulty parameter in the unidimensional model.
#' That is, higher values indicate that higher levels of theta for a probability of a correct response >.5 are necessary.
#' Likewise, the MDIFF sets the distance from the origin to the point of maximum slope.
#' However, in DMIRT analysis, the MDIFF parameter values become scalars for the location of the direction vectors given by the \eqn{a_{il}} equation.
#' The length of the vector is set by taking the MDIFF times the MDISC so that items with higher discrimination have longer vector arrows.
#'
#' # Limitations
#' The `D3mirt` technique is based on the grade response item model (Samejima, 1969) extended to a multidimensional space.
#' Consequently, the latter entails that `D3mirt` analysis is limited to items that fit the GRM.
#' Moreover, for the `D3mirt`analysis the number of dimensions can be up to three.
#' However, due to the nature of the model specification, the analysis can handle dimensions less than three.
#' This since the third axis, the z-axis, is free while only two items must meet the model identification requirements to locate the first (x-axis) and second axis (y-axis).
#'
#' # User Options
#' ## Constructs
#' The user has the option of including constructs in the estimation, by creating one or more nested lists that indicate what items belong to what construct (see the examples section).
#' From this, the `D3mirt()` function calculates direction cosines for the constructs by adding and normalizing the direction cosines for the items contained in each construct list.
#' The construct vector arrows can contribute to the analysis by visualizing the average direction for a subset set of items.
#'
#' ## Scaling of Item Vector Arrows
#' Regarding plotting, the `D3mirt()` function returns vector coordinates estimated with and without the MDISC as a scalar for arrow length. If the MDISC is not used, all vector arrows are scaled to unit length.
#' This can help reduce clutter in the graphical output when using `plotD3mirt()`.
#'
#' @return S3 object of class `D3mirt` with lists of \emph{a} and \emph{d} parameters, multidimensional discrimination (MDISC), multidimensional item difficulty (MDIFF), direction cosines and degrees for vector angles, construct lists, and vector coordinates.
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Reckase, M. D. (1985). The Difficulty of Test Items That Measure More Than One Ability. \emph{Applied Psychological Measurement, 9}(4), 401-412–412. https://doi-org.ezp.sub.su.se/10.1177/014662168500900409
#' @references Reckase, M. D., & McKinley, R. L. (1991). The Discriminating Power of Items That Measure More Than One Dimension. \emph{Applied Psychological Measurement, 15}(4), 361-373–373. https://doi-org.ezp.sub.su.se/10.1177/014662169101500407
#' @references Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. \emph{Psychometrika 34}, 1–97. https://doi.org/10.1007/BF03372160
#'
#' @examples
#' \dontrun{
#' # Preparation: Fitting a three-dimensional graded response model with orthogonal factors
#' # Example below uses a scale with 10 items, in total, named I_01...I_10
#' # Item I_05 and item I_10 have been selected to identify the model
#' # The DMIRT model specification implies that all items (1-10) load on all three factors (F1 to F3)
#' # The START and FIXED commands are used to locate the orthogonal axis in the model
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
#' # Assign data frame with factor loadings (located in the first three columns)
#' # And difficulty parameters (columns 4-7) from mod1 with mirt::coef and $'items'[,1:7]))
#' d <- data.frame(mirt::coef(mod1, simplify=TRUE)$'items'[,1:7])
#'
#' # Call to `D3mirt()`, including optional nested lists for two constructs
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- D3mirt(d, c)
#' }
#' @export
D3mirt <- function(x, constructs = NULL){
  # Warning for format
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
