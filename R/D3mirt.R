#' 3D DMIRT Model Estimation
#'
#' @description Descriptive multidimensional item response theory modeling (DMIRT; Reckase, 2009, 1985, Reckase & McKinley, 1991), for dichotomous and polytomous items restricted to three dimensions.
#'
#' @param x Data frame with rows for items and columns for model parameters. The number of columns must be more than or equal to 4, i.e., three columns with(\emph{a}) parameters and at least one column for difficulty (\emph{d}) parameters.
#' @param constructs Optional. Nested lists with integer values indicating construct. Default is `constructs = NULL`.
#'
#' @importFrom mirt mirt
#'
#' @details The `D3mirt()`function takes in a data frame of factor slopes and difficulty parameters from a compensatory three-dimensional two-parameter logistic model (2PL) or a graded
#' response model (GRM; Samejima, 1969), fitted in accordance with descriptive item response theory model specifications described below.
#' The function returns an S3 object containing descriptive multidimensional item response theory estimates that can be graphed as vector arrows representing item response functions in a three dimensional space with [D3mirt::plotD3mirt].
#'
#' Note, model parameters from the multidimensional 2PL or GRM must be assessed prior to using the `D3mirt()` function (see examples section below or the vignette included in this package).
#' This means that the model must first be identified (see [D3mirt::modid].
#'
#' # DMIRT Theory
#' In DMIRT analysis, also called within multidimensional modeling, it is assumed that items in a multidimensional ability space can measure single or multiple latent traits (Reckase, 2009, 1985; Reckase & McKinley, 1991).
#' The methodology is a type of data reduction technique that uses a compensatory type of model (Reckase, 2009), i.e., a model that assumes the existence of a multidimensional trait space forming an interconnected latent ability field.
#' This means that if one trait level is low, some other trait in the model can compensate to create the same predictive score.
#' Probability estimates are therefore based on linear combinations of trait scores in the model.
#'
#' Moreover, the DMIRT analysis is said to be descriptive because the results describe the extent to which items in a test are unidimensional,
#' i.e., that the items describe one dimension only, or are within-multidimensional, i.e., that the items describe more than one dimension.
#'
#' The DMIRT estimation process consists of first fitting a compensatory multidimensional two-parameter model so that discrimination and difficulty parameters can be extracted.
#' For `D3mirt`, this implies that a compensatory three-dimensional 2PL or GRM must be fitted.
#' Next, using these parameters the DMIRT computation finds the direction of the highest possible discrimination slope for each item.
#' The direction, in turn, shows what latent trait the item can be said to measure in the multidimensional latent model.
#' The output can be visualized as vector arrows representing item response functions located in a multidimensional space pointing in the direction of the maximum slope.
#' The `D3mirt` can be used to calculate all necessary estimates for this purpose, as well as to plot the results (albeit limited to three dimensions).
#'
#' The most central estimates, in DMIRT analysis are the single multidimensional discrimination (MDISC) parameter, the multidimensional difficulty (MDIFF) parameters, and the directional discrimination (DDISC) parameters (Reckase2009, 1985; Reckase & McKinley, 1991).
#' In addition, if constructs are used in the function call the output will also contain the directional discrimination (DDISC) parameters for the items on each construct vector orientation.
#' This makes it possible to compare item discrimination under the assumption that they measure in the same direction as each constructs.
#'
#' # Constructs
#' The user has the option of including constructs in the estimation, by creating one or more nested lists that indicate what items belong to what construct (see the examples section below).
#' From this, the `D3mirt()` function calculates direction cosines for the constructs by normalizing the direction cosines for the items contained in each construct list.
#* The construct vector arrows can contribute to the analysis by (a) visualizing the average direction for a subset set of items, and (b) showing how combinations of items interrelate at group-level.
#' Note, the length of the construct vector arrows is arbitrary.
#'
#' @return S3 object of class `D3mirt` with lists of \emph{a} and \emph{d} parameters from the 2PL or GRM estimation, multidimensional difficulty (MDIFF), multidimensional discrimination (MDISC), direction cosines and degrees for vector angles, construct lists, and vector coordinates.
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Reckase, M. D. (1985). The Difficulty of Test Items That Measure More Than One Ability. \emph{Applied Psychological Measurement, 9}(4), 401-412–412. https://doi-org.ezp.sub.su.se/10.1177/014662168500900409
#' @references Reckase, M. D., & McKinley, R. L. (1991). The Discriminating Power of Items That Measure More Than One Dimension. \emph{Applied Psychological Measurement, 15}(4), 361-373–373. https://doi-org.ezp.sub.su.se/10.1177/014662169101500407
#' @references Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. \emph{Psychometrika 34}, 1–97. https://doi.org/10.1007/BF03372160
#'
#' @examples
#' \donttest{
#' # Load data
#' data("anes0809offwaves")
#' x <- anes0809offwaves
#' x <- x[,3:22] # Remove columns for age and gender
#'
#' # Fit a three-dimensional graded response model with orthogonal factors
#' # Example below uses Likert items from the built-in data set "anes0809offwaves"
#' # Item W7Q3 and item W7Q20 was selected with `modid()`
#' # The model specification set all items in the data set (1-20)
#' # to load on all three factors (F1-F3)
#' # The START and FIXED commands are used on the two items to identify the DMIRT model
#' spec <- '  F1 = 1-20
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
#' # and difficulty parameters (columns 4-8 in mod1)
#' d <- data.frame(mirt::coef(mod1,
#'                            simplify=TRUE)$'items'[,1:8])
#'
#' # Call D3mirt() with data frame d
#' g <- D3mirt(d)
#' summary(g) # Show summary of results
#'
#' # Call to D3mirt(), including optional nested lists for three constructs
#' # Item W7Q16 is not included in any construct because of model violations
#' # The model violations for the W7Q16 item can be seen when plotting the model
#' c <- list(list (1,2,3,4),
#'           list(5,7,8,9,10),
#'           list(11,12,13,14,15,15,16,17,18,19,20))
#' g <- D3mirt(d, c)
#' summary(g)
#' }
#' @export
D3mirt <- function(x, constructs = NULL){
  if(ncol(x) < 4) stop("Data frame must have at least 4 columns")
  x <- as.matrix(x)
  a <- x[,1:3, drop = FALSE]
  ndiff <- ncol(x)-3
  if (ndiff == 1){
    diff <- x[,4, drop = FALSE]
  } else {
    diff <- x[,(4):(3+ndiff), drop = FALSE]
  }
  mdisc <- sqrt(rowSums(a^2))
  md <- mdisc%*%matrix(rep(1,3), nrow=1, ncol=3)
  dcos <- as.matrix(a/md, ncol = 3)
  theta <- NULL
  for (i in seq(nrow(dcos))){
    c <- dcos[i,1]
    d <- dcos[i,3]
    if (c < 0 && d >= 0){
      t <- 180 + atan(dcos[i,3]/dcos[i,1])*(180/pi)
      theta <- as.matrix(rbind(theta, t), ncol = 1)
    } else if (c < 0 && d < 0){
      t <- -180 + atan(dcos[i,3]/dcos[i,1])*(180/pi)
      theta <- as.matrix(rbind(theta, t), ncol = 1)
    } else {
      t <- atan(dcos[i,3]/dcos[i,1])*(180/pi)
      theta <- as.matrix(rbind(theta, t), ncol = 1)
    }
  }
  phi <- acos(dcos[,2])*(180/pi)
  sph <- cbind(theta, phi)
  vector1 <- NULL
  vector2 <- NULL
  mdiff <- NULL
  for (i in seq_len(ndiff)){
    d <- diff[,i]
    dist <- -d/mdisc
    xyz <- dist*dcos
    uvw1 <- mdisc*dcos+xyz
    uvw2 <- dcos+xyz
    vec1 <- do.call(rbind,list(xyz,uvw1))[order(sequence(sapply(list(xyz,uvw1),nrow))),]
    vec2 <- do.call(rbind,list(xyz,uvw2))[order(sequence(sapply(list(xyz,uvw2),nrow))),]
    vector1 <- as.matrix(rbind(vector1,vec1), ncol = 3)
    vector2 <- as.matrix(rbind(vector2,vec2), ncol = 3)
    mdiff <- as.matrix(cbind(mdiff, dist), ncol = 1)
  }
  if (!is.null(constructs)){
    if(!is.list(constructs)) stop("The constructs must be of type list")
    if(!any(sapply(constructs, class) == "list")) stop("The constructs argument requires nested lists")
    con <- NULL
    csph <- NULL
    ncos <- NULL
    ddisc <- NULL
    for (i in seq_along(constructs)){
      l <- unlist(constructs[i])
      cos <- NULL
      for (i in seq_along(l)){
        n <- l[i]
        m <- dcos[n,]
        cos <- as.matrix(rbind(cos,m), ncol = 3)
      }
      cscos <- matrix(colSums(cos), ncol = 3)
      cdcos <- 1/sqrt(rowSums(cscos^2))*cscos
      maxnorm <- (1.1*max(vector1))*cdcos
      minnorm <- (0.6*min(vector1))*cdcos
      con <- as.matrix(rbind(con,rbind(minnorm, maxnorm)), ncol = 3)
      ncos <- as.matrix(rbind(ncos,cdcos), ncol = 3)
      theta <- NULL
      for (i in seq(nrow(cdcos))){
        c <- cdcos[i,1]
        d <- cdcos[i,3]
        if (c < 0 && d >= 0){
          t <- 180 + atan(cdcos[i,3]/cdcos[i,1])*(180/pi)
          theta <- as.matrix(rbind(theta, t), ncol = 1)
        } else if (c < 0 && d < 0){
          t <- -180 + atan(cdcos[i,3]/cdcos[i,1])*(180/pi)
          theta <- as.matrix(rbind(theta, t), ncol = 1)
        } else {
          t <- atan(cdcos[i,3]/cdcos[i,1])*(180/pi)
          theta <- as.matrix(rbind(theta, t), ncol = 1)
        }
      }
      phi <- acos(cdcos[,2])*(180/pi)
      sphe <- cbind(theta, phi)
      csph <- as.matrix(rbind(csph,sphe), ncol = 2)
      disc <-  apply(a, 1, function(x) x %*% t(cdcos))
      ddisc <- as.matrix(cbind(ddisc, disc), ncol = 1)
    }
  }
  a <- as.data.frame(a)
  for (i in ncol(a)){
    colnames(a) <- paste("a", 1:i, sep = "")
  }
  diff <- as.data.frame(diff)
  for (i in ncol(diff)){
    colnames(diff) <- paste("d", 1:i, sep = "")
  }
  mdiff <- as.data.frame(mdiff)
  for (i in ncol(mdiff)){
    colnames(mdiff) <- paste("D", 1:i, sep = "")
  }
  mdisc <- as.data.frame(mdisc)
  colnames(mdisc) <- c("MDISC")
  dcos <- as.data.frame(dcos)
  colnames(dcos) <- c("D.Cos X", "D.Cos Y", "D.Cos Z")
  sph <- as.data.frame(sph)
  colnames(sph) <- c("Theta", "Phi")
  if (ndiff == 1){
    dir.vec <- vector1
    scal.vec <- vector2
  } else {
    dir.vec <- split.data.frame(vector1, cut(seq_len(nrow(vector1)), ndiff))
    scal.vec <- split.data.frame(vector2, cut(seq_len(nrow(vector2)), ndiff))
  }
  if (!is.null(constructs)){
    ncos <- as.data.frame(ncos)
    colnames(ncos) <- c("C.Cos X","C.Cos Y", "C.Cos Z")
    csph <- as.data.frame(csph)
    colnames(csph) <- c("Theta", "Phi")
    for (i in nrow(csph)){
      rownames(csph) <- paste("C", 1:i, sep = "")
    }
    dddisc <- as.data.frame(ddisc)
    for (i in ncol(ddisc)){
      colnames(ddisc) <- paste("DDISC", 1:i, sep = "")
    }
    D3mirt <- list(loadings = a, diff = diff, mdisc = mdisc, mdiff = mdiff, dir.cos = dcos, spherical = sph, c.dir.cos = ncos , c.spherical = csph, ddisc = ddisc,
                  dir.vec = dir.vec, scal.vec = scal.vec, c = constructs,  c.vec = con)
  } else {
    D3mirt <- list(loadings = a, diff = diff, mdisc = mdisc, mdiff = mdiff, dir.cos = dcos, spherical = sph, diff = diff,
                  dir.vec = dir.vec, scal.vec = scal.vec)
  }
  class(D3mirt) <- "D3mirt"
  D3mirt
}
