summary.dmirt <- function(x, digits = 4){
  tab1 <- as.data.frame(x$loadings, drop = FALSE)
  tab2 <- as.data.frame(x$mdiff, drop = FALSE)
  tab3 <- as.data.frame(x$mdisc, drop = FALSE)
  tab1 <- as.data.frame(cbind(tab1, tab2, tab3), drop = FALSE)
  tab4 <- as.data.frame(cbind(x$dir.cos, x$degrees), drop = FALSE)
  if (!is.null(x$c.dir.cos)){
    tab5 <- as.data.frame(cbind(x$c.dir.cos, x$c.degrees), drop = FALSE)
    c <- x$c
    items <- NULL
    for (i in seq_along(c)){
      l <- unlist(c[i])
      items <- list(l)
      for (i in seq_along(l)){
        n <- l[i]
        m <- rownames(tab1[n,])
        items <- list(m)
      }
    }
  }
  if (!is.null(x$c.dir.cos)){
    sum <- list(dmirt.est = round(tab1,digits), dmirt.angles = round(tab4, digits), construct.angles = round(tab5, digits))
  } else{
    sum <- list(dmirt.est = round(tab1,digits), dmirt.angles = round(tab4, digits))
  }
  if (!is.null(x$c.dir.cos)){
    cat(paste("\n3Dmirt object with", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
    for (i in seq_along(c)){
      n <- c[i]
      cat(paste("Construct vector", i, "contains items", paste(c[[i]], collapse = ", "), "\n\n"))
    }
  } else {
    cat(paste("\n3Dmirt object with", nrow(tab1), "items and", ncol(tab2), "levels of difficulty\n\n"))
  }
  sum
}
