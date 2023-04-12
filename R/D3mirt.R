#' 3D DMIRT Modeling
#'
#' @description Descriptive multidimensional item response theory modeling (DMIRT; Reckase, 2009, 1985, Reckase & McKinley, 1991), for dichotomous and polytomous items restricted to three dimensions.
#'
#' @param x Data frame with rows for items and columns for model parameters. The number of columns must be more than or equal to 4, i.e., three columns for slope (\emph{a}) parameters and at least one column for difficulty (\emph{d}) parameters.
#' @param constructs Optional. Nested lists with integer values indicating construct. Default is `constructs = NULL`.
#'
#' @importFrom mirt mirt
#'
#' @details The `D3mirt()`function takes in a data frame of factor slopes and difficulty parameters from a three-dimensional graded
#' response model (GRM; Samejima, 1969), fitted in accordance with descriptive item response theory model specifications described below,
#' and returns an S3 object containing estimates that can be graphed as vector arrows in a three dimensional space with [D3mirt::plotD3mirt].
#'
#' Note, model parameters from the multidimensional GRM must be assessed prior to using the `D3mirt()` function (see examples section or the vignette included in this package).
#' This means that the model must first be identified correctly.
#' The `modid()` function can help with the latter by suggesting what items to use for this step in the process.
#' For more information on model identification, see documentation regarding [D3mirt::modid].
#'
#' # Within Multidimensional Modeling
#' In DMIRT analysis, also called within multidimensional modeling, it is assumed that items in a multidimensional trait space
#' can measure single or multiple latent abilities (Reckase, 2009, 1985, Reckase & McKinley, 1991).
#' The DMIRT method is said to be descriptive because the estimates describes item characteristics when more than one latent dimension is used in the analysis.
#' Under the assumption of within-dimensionality, the two-parameter graded response model is used to extract two-parameter multidimensional item characteristics.
#' These include a single multidimensional discrimination (MDISC) parameter and a multidimensional difficulty (MDIFF) index for each item.
#'
#' Note, `D3mirt` analysis is limited to items that fit the GRM and the number of dimensions can be up to three.
#' However, due to the nature of the model specification, the analysis can handle data with containing less that three dimensions.
#' This since the third axis, the z-axis, is free while only two items must meet the model identification requirements so that the first (x-axis) and the second axis (y-axis) can be located.
#'
#' ## Constructs
#' The user has the option of including constructs in the estimation, by creating one or more nested lists that indicate what items belong to what construct (see the examples section).
#' From this, the `D3mirt()` function calculates direction cosines for the constructs by adding and normalizing the direction cosines for the items contained in each construct list.
#' The construct vector arrows can contribute to the analysis by visualizing and assesing the the average direction for a subset set of items.
#' Note, the length of the construct vector arrows is arbitrary.
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
#' # Load data
#' data("anes08_09offwaves")
#' x <- anes08_09offwaves
#' x <- x[,3:22] # Remove columns for age and gender
#'
#' # Fit a three-dimensional graded response model with orthogonal factors
#' # Example below uses Likert items from the built-in data set "anes08_09offwaves"
#' # Item W7Q3 and item W7Q20 was selected with `modid()`
#' # The model specification set all items in the data set (1-20)
#' # to load on all three factors (F1-F3)
#' # The START and FIXED commands are used on the two items to identify the DMIRT model
#' spec <- ' F1 = 1-20
#'            F2 = 1-20
#'            F3 = 1-20
#'
#'            START=(W7Q3,a2,0)
#'            START=(W7Q3,a3,0)
#'
#'            START=(W7Q20,a3,0)
#'
#'            FIXED=(W7Q3,a2)
#'            FIXED=(W7Q3,a3)
#'
#'            FIXED=(W7Q20,a3) '
#'
#'
#' mod1 <- mirt::mirt(x,
#'                    spec,
#'                    itemtype = 'graded',
#'                    SE = TRUE,
#'                    method = 'QMCEM')
#'
#' # Assign a data frame with factor loadings (located in the first three columns in mod1),
#' # and difficulty parameters (columns 4-8 in mod1) with mirt::coef and $'items'[,1:8]))
#' d <- data.frame(mirt::coef(mod1,
#'                            simplify=TRUE)$'items'[,1:8])
#'
#'
#' # Call D3mirt() with data frame d
#' g <- D3mirt(d)
#' summary(g) # Show summary of results
#'
#' c <- list(list (1,2,3,4),
#'           list(5,7,8,9,10),
#'           list(11,12,13,14,15,15,16,17,18,19,20))
#' g <- D3mirt(d, c)
#' summary(g)
#' @export
D3mirt <- function(x, constructs = NULL){
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
