plot.dmirt <- function (x, axis.ticks = TRUE, nticks = 8, constructs = FALSE, profiles = NULL, hide = FALSE, items = NULL, item.lab = TRUE, item.names = NULL, construct.lab = NULL, adjustlab = c(0.5, -0.8),
                        diff.level = NULL, background = "white",
                        width.rgl.x = 1040, width.rgl.y= 1040, view = c(15,20, 0.7), axis.fac = 1.2, axis.col = "black", axis.points = "black",
                        points = TRUE, title="", line = -5, x.lab = "X", y.lab="Y", z.lab="Z", show.plane = TRUE, plane.color = "grey80",
                        type = "rotation", col = c("blue", "orange", "red", "purple", "cyan"),
                        arrow.width = 0.6, n = 20, theta = 0.2, barblen = 0.03,
                        c.type = "rotation", c.col = c("black"), c.arrow.width = 0.6,
                        c.n = 20, c.theta = 0.2, c.barblen = 0.03, spheres.r = 0.05,
                        sphere.col = "blue", ellips = TRUE, CI.level = 0.95, ellips.col = "grey80", ellips.alpha = 0.20){
  open3d()
  par3d(windowRect = 50 + c( 0, 0, width.rgl.x, width.rgl.y))
  bg3d(color = background)
  view3d(theta = view[1], phi = view[2], zoom = view[3])
  if (is.null(ncol(x$dir.vec))){
    ax <- x$dir.vec
    low <- as.data.frame(ax[1], drop = FALSE)
    hig <- as.data.frame(ax[length(ax)], drop = FALSE)
    xaxis.min <- min(low[,1])*axis.fac
    xaxis.max <- max(hig[,1])*axis.fac
    yaxis.min <- min(low[,2])*axis.fac
    yaxis.max <- max(hig[,2])*axis.fac
    zaxis.min <- min(low[,3])*axis.fac
    zaxis.max <- max(hig[,3])*axis.fac
  } else{
    ax <- x$dir.vec
    xaxis.min <- min(ax[,1])*axis.fac
    xaxis.max <- max(ax[,1])*axis.fac
    yaxis.min <- min(ax[,2])*axis.fac
    yaxis.max <- max(ax[,2])*axis.fac
    zaxis.min <- min(ax[,3])*axis.fac
    zaxis.max <- max(ax[,3])*axis.fac
  }
  xaxis <- c(-abs(xaxis.min), abs(xaxis.max))
  yaxis <- c(-abs(yaxis.min), abs(yaxis.max))
  zaxis <- c(-abs(zaxis.min), abs(yaxis.max))
  segments3d(xaxis, c(0, 0), c(0, 0), color = axis.col)
  segments3d(c(0, 0), yaxis, c(0, 0), color = axis.col)
  segments3d(c(0, 0), c(0, 0), zaxis, color = axis.col)
  if (axis.ticks == TRUE){
    axis3d('x', pos = c(0, 0, 0), ticks = TRUE, nticks=nticks)
    axis3d('y', pos = c(0, 0, 0), ticks = TRUE, nticks=nticks)
    axis3d('z',pos = c(0, 0, 0), ticks = TRUE, nticks=nticks)
  }
  if (points == TRUE){
    axes <- rbind(c(xaxis[2], 0, 0), c(0, yaxis[2], 0),
                  c(0, 0, zaxis[2]))
    points3d(axes, color = axis.points, size = 3)
  }
  text3d(axes, text = c(x.lab, y.lab, z.lab), color = axis.col,
         adj = c(0.5, -0.8), size = 2)
  title3d(main= title,line= line)
  if (show.plane == TRUE) {
    material3d(color = plane.color)
    xlim <- xaxis/1.5; zlim <- zaxis /1.5
    quads3d( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  }
  if (hide == FALSE){
    vec <- x$dir.vec
    if (!is.null(items)){
      if(any(!items <= nrow(x$loadings))) stop("The items list contains one or more item indicators that are higher than the total number of items")
      if (is.null(diff.level)){
        if (is.null(ncol(vec))){
          for (i in seq_along(items)){
            m <- items[i]*2-1
            sapply(seq_along(vec), function(i){
              arrow3d(vec[[i, drop = FALSE]][m,], vec[[i, drop = FALSE]][m+1,], type = type, col = col[i], width = arrow.width, n = n, theta = theta, barblen = barblen)
            })
          }
        } else {
          m <- items*2-1
          sapply(m, function(x){
            arrow3d(vec[x,], vec[x+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
        }
      } else {
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
        })
      }
    }
    else if (!is.null(diff.level)) {
      if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
      for (i in seq_along(diff.level)){
        d <- diff.level[i]
        v <- as.data.frame(vec[d, drop = FALSE])
        color <- col[d]
        for (i in seq(from = 1, to = nrow(v), by = 2)){
          arrow3d(v[i,], v[i+1,], type = type, col = color, width = arrow.width, n = n, theta = theta, barblen = c.barblen)
        }
      }
    } else {
      if (is.null(ncol(vec))){
        for (i in seq_along(vec)){
          v <- vec[[i]]
          color <- col[i]
          for (i in seq(from = 1, to = nrow(v), by=2)){
            arrow3d(v[i,], v[i+1,], type = c.type, col = color, width = arrow.width, n = n, theta = theta, barblen = barblen)
          }
        }
      } else {
        sapply(seq(from = 1, to = nrow(v), by=2), function(i){
          arrow3d(vec[i,], vec[i+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
      }
    }
    if (item.lab == TRUE && is.null(items)){
      if (is.null(diff.level)){
        if (is.null(item.names)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(inames[i]), color = axis.col,
                   adj = adjustlab, size = 2)
          })
        } else {
          if(!length(item.names) <= nrow(x$loadings)) warning("There are more item labels than items")
          if(length(item.names) < nrow(x$loadings)) warning("There are too few item labels")
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.names[i]), color = axis.col,
                   adj = adjustlab, size = 2)
          } )
        }
      } else {
        inames <- rownames(x$loadings)
        dl <-  x$dir.vec[[diff.level]]
        sapply(seq(nrow(x$mdisc)), function(i){
          text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
                 adj = adjustlab, size = 2)
        })
      }
    }
    if (item.lab == TRUE && !is.null(items)){
      if(any(!items <= nrow(x$loadings))) stop("The items list contains one or more item indicators that are higher than the total number of items")
      if (is.null(diff.level)){
        if (is.null(item.names)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(inames[m]), color = axis.col,
                   adj = adjustlab, size = 2)
          })
        } else {
          if(!length(item.names) <= length(items)) warning("There are more item labels than items in list")
          if(length(item.names) < length(items)) warning("There are too few item labels")
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(item.names[i]), color = axis.col,
                   adj = adjustlab, size = 2)
          })
        }
      } else {
        if (is.null(item.names)){
          dl <-  x$dir.vec[[diff.level]]
          inames <- rownames(x$loadings)
          sapply(seq_along(items), function(i){
            m <- items[i]
            text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(inames[m]), color = axis.col,
                   adj = adjustlab, size = 2)
          })
        } else {
          dl <-  as.data.frame(x$dir.vec[diff.level], drop = FALSE)
          sapply(seq_along(items), function(i){
            m <- items[i]
            text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(item.names[i]), color = axis.col,
                   adj = adjustlab, size = 2)
          })
        }
      }
    }
  }
  if (constructs == TRUE){
    if (is.null(x$constructs)) warning("3D mirt object does not contain any constructs")
    cvec <- x$c.vec
    sapply(seq(from = 1, to = nrow(cvec), by=2), function(x){
      arrow3d(cvec[x,], cvec[x+1,], type = c.type, col = c.col, width = c.arrow.width, n = c.n, theta = c.theta, barblen = c.barblen)
    })
    if (!is.null(construct.lab) && constructs == TRUE){
      if(!length(construct.lab) <= nrow(x$c.vec)) warning("There are more construct labels than constructs")
      clab <-  x$c.vec
      sapply(seq(nrow(x$c.dir.cos)), function(i){
        text3d(clab[(i*2),1],clab[(i*2),2], clab[(i*2),3], text = c(construct.lab[i]), color = axis.col,
               adj = adjustlab, size = 2)
      })
    }
  }
  if (!is.null(profiles)){
    if(!is.data.frame(profiles)) stop("Profiles must be data frame or matrix")
    x <- profiles[,1]
    y <- profiles[,2]
    z <- profiles[,3]
    rgl.spheres(x,y,z, r = spheres.r, color = sphere.col)
    if (ellips == TRUE){
      ellips <- ellipse3d(cov(cbind(x,y,z)),
                          centre=c(mean(x), mean(y), mean(z)), level = CI.level)
      shade3d(ellips, col = ellips.col, alpha = ellips.alpha)
    }
  }
}
