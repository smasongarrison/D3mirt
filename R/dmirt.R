dmirt <- function(x, constructs = NULL, max.norm = 1.1, min.norm = 0.6){
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
  colnames(deg) <- c("X.Deg", "Y.Deg", "Z.Deg")
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
    colnames(cdeg) <- c("X.Deg", "Y.Deg", "Z.Deg")
    dmirt <- list(loadings = a, mdisc = mdisc, dir.cos = dcos, degrees = deg, mdiff = mdiff,
                  dir.vec = dir.vec, c = constructs, c.dir.cos = ncos ,c.degrees = cdeg, c.vec = con)
  } else {
    dmirt <- list(loadings = a, mdisc = mdisc, dir.cos = dcos, degrees = deg, mdiff = mdiff,
                  dir.vec = dir.vec, ndiff)
  }
  class(dmirt) = "dmirt"
  dmirt
}
