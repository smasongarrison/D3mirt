#' plot.dmirt: Plot function for the D3mirt package
#'
#' @description The `plot.dmirt()`visualize the S3 `dmirt()` object in a 3D theta space. The plot function is based on [rgl] package for data visualization. Output consists of a RGL graphical device that can be exported with dedicated functions (see the examples section).
#' @param x S3 dmirt object
#' @param scale Logical, if item vector arrow length should visualize the MDISC estimates. If set to FALSE, vector arrow length will be uniform by adding the direction cosines times one for every level of multidimensional difficulty. Default is `scale = FALSE`.
#' @param constructs Logical, if construct vector arrows should be plotted. Default set to FALSE
#' @param con.scalars Set of scalars for adjusting construct arrow length. The first positive numeric adjusts the length in the negative direction and the second positive numeric adjusts the length in the positive direction. Default is `con.scalars = c(1,1)`.
#' @param profiles Data frame with coordinates for spheres representing respondents
#' @param hide Logical, if items should be plotted. Default is `hide = FALSE`.
#' @param items Optional. The user can input a list of integers indicating what item vector arrows will be visible while remaining items are only hidden.
#' @param item.lab Logical, if item labels should be plotted. Default is `item.names = TRUE`.
#' @param item.names Optional. The user can provide a list of item names that will override row names extracted from the data frame. Names can be numbers or characters.
#' @param construct.lab Optional. User can provide list of names for constructs.
#' @param adjustlab Vector of adjustment parameters for position of item and construct labels for the `text3d` function. The first value is for horizontal adjustment and the second is for vertical adjustment. Default is `adjustlab = c(0.5, -0.8)`.
#' @param diff.level Optional. If multiple levels of difficulty are used in the model, it is possible to plot a single level of difficulty by imputing its integer value.
#' @param background Set background color for the graphical device, Default is `background = "white"`.
#' @param width.rgl.x Width in the x direction for `par3d()`. Default is `width.rgl.x = 1040`.
#' @param width.rgl.y Width in the y direction for `par3d()`. Default is `width.rgl.y = 1040`.
#' @param view Vector with polar coordinates and zoom factor for the `view3d` function. Default is `view = c(15,20, 0.7)`.
#' @param axis.scalar Scalar factor the length of all three axis in the 3D model. Default is `axis.fac = 1.2`.
#' @param axis.col Color of axis for the `segment3D()`function, default is `axis.col = "Black"`.
#' @param axis.points Color of axis points for the `points3d()` function. Default is `axis.points = "black"`.
#' @param points Logical, if axis from the `points3d()` should have end points. Default is `points = TRUE`.
#' @param axis.ticks Logical, if axis ticks from the `axis3d()`function should be plotted. Default is `axis.ticks = TRUE'.
#' @param nticks Number of ticks for the `axis3d()`function. Default is `nticks = 8`.
#' @param title The main title for the plot plotted with the `title3d()` function.
#' @param line  Title placement for `title3d()`. Default is `line = -5`.
#' @param x.lab Labels for x-axis, Default is `x.lab = "X"`.
#' @param y.lab Labels for y-axis, Default is `y.lab = "Y"`.
#' @param z.lab Labels for y-axis, Default is `z.lab = "Z"`.
#' @param show.plane Logical, if xz-plane should be plotted in graphical device. Default is `show.plane = TRUE`.
#' @param plane.color Color of plane, default is `plane.color = "grey80"`.
#' @param type Type of vector arrow for items, default is `type = "rotation"`. See [rgl::arrow3d] for more options regarding arrow types.
#' @param col Vector of colors representing difficulty levels for items using the `arrow3d()` function. Default is `col = c("black", "grey20", "grey40", "grey60", "grey80")`
#' @param arrow.width Width of vector arrows for `arrow3d()`. Default is `arrow.width = 0.6`.
#' @param n Number of barbs for the vector arrows from the `arrow3d()` function. Default is `n = 20`.
#' @param theta Opening angle of barbs for vector arrows from `arrow3d()`. Default is `theta = 0.2`.
#' @param barblen The length of the barbs for vector arrows from `arrow3d()`. Default is `barblen = 0.03`.
#' @param c.type Type of vector arrow for constructs. See [rgl::arrow3d] for more options regarding arrow types. Default is `c.type = "rotation"`.
#' @param c.col Color for construct vector arrows from `arrow3d()`, default is `c.col = "black"`.
#' @param c.arrow.width Width of construct vector arrows for `arrow3d()`. Default is `c.arrow.width = 0.6`.
#' @param c.n Number of barbs for the construct vector arrows from the `arrow3d()` function. Default is `c.n = 20`.
#' @param c.theta Opening angle of barbs for construct vector arrows from `arrow3d()`. Default is `c.theta = 0.2`.
#' @param c.barblen The length of the barbs for construct vector arrows from `arrow3d()`. Default is `c.barblen = 0.03`.
#' @param spheres.r Radius of sphere for `spheres3d()`. Default is `spheres.r = 0.05`.
#' @param sphere.col Color of sphere `spheres3d()`. Default is `sphere.col = "grey20"`.
#' @param ellips Logical, if spheres should include an ellipsoid outlining an confidence region returned from the `ellipse3d()` function. Default is `ellips = TRUE`.
#' @param CI.level Level of confidence for `ellipse3d()`, default is `CI.level = 0.95`.
#' @param ellips.col Color of the ellipse from `ellipse3d()`. Default is `ellips.col = "grey80"`.
#' @param ellips.alpha Opacity for the confidence region from `ellipse3d()`. Default is `ellips.alpha = 0.20`.
#' @param ... Additional arguments to be passed to RGL or methods.
#'
#' @return RGL graphical device.
#' @import rgl
#' @importFrom stats cov
#' @export
#'
#' @details The RGL device has orthogonal standardized axis centered at 0. The function allows plotting of all items, a selection of items as well as plotting a single item and adding constructs to the graphical output (see examples section). The user can also choose to plot one level of difficulty at a time if multiple levels of difficulty are used in the model. Item names are plotted by default but the user has the option of imputing new names for the items and include names for the constructs.
#'
#' Moreover, the function can also plots respondents on top of the item model represented as spheres located with the help of respondents factors scores providing the necessary coordinates (see [D3mirt::profile] for more details). This allows for profile analysis in which the user can select respondent rows based on some criteria. When analyzing respondent profiles the user has the option of hiding the item vectors to avoid visual cluttering.
#'More specifically, item location shows the items three dimensional level of difficulty. If Likert items are used, each item will have multiple level of
#'difficulty and can therefore be said to show the difficulty range of an item. Moreover, the angle of the vector arrows shows the direction of optimal discrimination in the model. In turn, this indicates what traits, one up to three, the item can be said to describe. Lastly, the length of each vector arrows shows the strength of discrimination, in which longer arrows indicate higher discrimination. Short vector arrows are therefore signs of model violations.
#' @examples
#' \dontrun{
#' # Preparation: Calculate dmirt estimates with constructs
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- dmirt(d, c)
#'
#'
#' # Plot RGL device with constructs visible
#' plot.dmirt(g, constructs = TRUE)
#'
#'
#' # Plot a selection of items from the model
#' plot.dmirt(g, constructs = TRUE, items = c(1,3,4,6,8))
#'
#'
#'
#' # Export RGL device in consol
#' plot.dmirt(g, constructs = TRUE)
#' rglwidget(width = 1040, height = 1040)
#'
#'
#' # Export RGL device to file
#' plot.dmirt(g, constructs = TRUE) # ta bort dmirt?
#' rgl.snapshot('RGLdevice.png', fmt = 'png')
#' }
plot.D3mirt <- function (x, scale = FALSE, constructs = FALSE, con.scalars = c(1,1), profiles = NULL, hide = FALSE, items = NULL, item.lab = TRUE, item.names = NULL, construct.lab = NULL, adjustlab = c(0.5, -0.8),
                        diff.level = NULL, background = "white",
                        width.rgl.x = 1040, width.rgl.y= 1040, view = c(15,20, 0.7), axis.scalar = 1.2, axis.col = "black", axis.points = "black",
                        points = TRUE, axis.ticks = TRUE, nticks = 8, title="", line = -5, x.lab = "X", y.lab="Y", z.lab="Z", show.plane = TRUE, plane.color = "grey80",
                        type = "rotation", col = c("black", "grey20", "grey40", "grey60", "grey80"),
                        arrow.width = 0.6, n = 20, theta = 0.2, barblen = 0.03,
                        c.type = "rotation", c.col = "black", c.arrow.width = 0.6,
                        c.n = 20, c.theta = 0.2, c.barblen = 0.03, spheres.r = 0.05,
                        sphere.col = "grey20", ellips = TRUE, CI.level = 0.95, ellips.col = "grey80", ellips.alpha = 0.20, ...){
  rgl::open3d()
  rgl::par3d(windowRect = 50 + c( 0, 0, width.rgl.x, width.rgl.y))
  rgl::bg3d(color = background)
  rgl::view3d(theta = view[1], phi = view[2], zoom = view[3])
  if (is.null(ncol(x$dir.vec))){
    ax <- x$dir.vec
    low <- as.data.frame(ax[1], drop = FALSE)
    hig <- as.data.frame(ax[length(ax)], drop = FALSE)
    xaxis.min <- min(low[,1])*axis.scalar
    xaxis.max <- max(hig[,1])*axis.scalar
    yaxis.min <- min(low[,2])*axis.scalar
    yaxis.max <- max(hig[,2])*axis.scalar
    zaxis.min <- min(low[,3])*axis.scalar
    zaxis.max <- max(hig[,3])*axis.scalar
  } else{
    ax <- x$dir.vec
    xaxis.min <- min(ax[,1])*axis.scalar
    xaxis.max <- max(ax[,1])*axis.scalar
    yaxis.min <- min(ax[,2])*axis.scalar
    yaxis.max <- max(ax[,2])*axis.scalar
    zaxis.min <- min(ax[,3])*axis.scalar
    zaxis.max <- max(ax[,3])*axis.scalar
  }
  xaxis <- c(-abs(xaxis.min), abs(xaxis.max))
  yaxis <- c(-abs(yaxis.min), abs(yaxis.max))
  zaxis <- c(-abs(zaxis.min), abs(yaxis.max))
  rgl::segments3d(xaxis, c(0, 0), c(0, 0), color = axis.col)
  rgl::segments3d(c(0, 0), yaxis, c(0, 0), color = axis.col)
  rgl::segments3d(c(0, 0), c(0, 0), zaxis, color = axis.col)
  if (axis.ticks == TRUE){
    rgl::axis3d('x', pos = c(0, 0, 0), ticks = TRUE, nticks=nticks)
    rgl::axis3d('y', pos = c(0, 0, 0), ticks = TRUE, nticks=nticks)
    rgl::axis3d('z',pos = c(0, 0, 0), ticks = TRUE, nticks=nticks)
  }
  if (points == TRUE){
    axes <- rbind(c(xaxis[2], 0, 0), c(0, yaxis[2], 0),
                  c(0, 0, zaxis[2]))
    rgl::points3d(axes, color = axis.points, size = 3)
  }
  rgl::text3d(axes, text = c(x.lab, y.lab, z.lab), color = axis.col,
              adj = c(0.5, -0.8), size = 2)
  rgl::title3d(main= title,line= line)
  if (show.plane == TRUE) {
    rgl::material3d(color = plane.color)
    xlim <- xaxis/1.5; zlim <- zaxis /1.5
    rgl::quads3d( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
                  z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  }
  if (hide == FALSE){
    if (scale==FALSE){
    vec <- x$dir.vec
    if (!is.null(items)){ # warning list items is not list
      if(any(!items <= nrow(x$loadings))) stop("The items list contains one or more item indicators that are higher than the total number of items")
      if (is.null(diff.level)){
        if (is.null(ncol(vec))){
          for (i in seq_along(items)){
            m <- items[i]*2-1
            sapply(seq_along(vec), function(i){
              rgl::arrow3d(vec[[i, drop = FALSE]][m,], vec[[i, drop = FALSE]][m+1,], type = type, col = col[i], width = arrow.width, n = n, theta = theta, barblen = barblen)
            })
          }
        } else {
          m <- items*2-1
          sapply(m, function(x){
            rgl::arrow3d(vec[x,], vec[x+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
        }
      } else {
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high") # format warning
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
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
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = color, width = arrow.width, n = n, theta = theta, barblen = c.barblen)
        }
      }
    } else {
      if (is.null(ncol(vec))){
        for (i in seq_along(vec)){
          v <- vec[[i]]
          color <- col[i]
          for (i in seq(from = 1, to = nrow(v), by=2)){
            rgl::arrow3d(v[i,], v[i+1,], type = c.type, col = color, width = arrow.width, n = n, theta = theta, barblen = barblen)
          }
        }
      } else {
        sapply(seq(from = 1, to = nrow(v), by=2), function(i){
          rgl::arrow3d(vec[i,], vec[i+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
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
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(inames[i]), color = axis.col,
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
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.names[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          } )
        }
      } else {
        inames <- rownames(x$loadings)
        dl <-  x$dir.vec[[diff.level]]
        sapply(seq(nrow(x$mdisc)), function(i){
          rgl::text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
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
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(inames[m]), color = axis.col,
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
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(item.names[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        }
      } else {
        if (is.null(item.names)){
          dl <-  x$dir.vec[[diff.level]]
          inames <- rownames(x$loadings)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        } else {
          dl <-  as.data.frame(x$dir.vec[diff.level], drop = FALSE)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(item.names[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        }
      }
    }
  } else { # scale TRUE
    vec <- x$scal.vec
    if (!is.null(items)){ # warning list items is not list
      if(any(!items <= nrow(x$loadings))) stop("The items list contains one or more item indicators that are higher than the total number of items")
      if (is.null(diff.level)){
        if (is.null(ncol(vec))){
          for (i in seq_along(items)){
            m <- items[i]*2-1
            sapply(seq_along(vec), function(i){
              rgl::arrow3d(vec[[i, drop = FALSE]][m,], vec[[i, drop = FALSE]][m+1,], type = type, col = col[i], width = arrow.width, n = n, theta = theta, barblen = barblen)
            })
          }
        } else {
          m <- items*2-1
          sapply(m, function(x){
            rgl::arrow3d(vec[x,], vec[x+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
        }
      } else {
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high") # format warning
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
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
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = color, width = arrow.width, n = n, theta = theta, barblen = c.barblen)
        }
      }
    } else {
      if (is.null(ncol(vec))){
        for (i in seq_along(vec)){
          v <- vec[[i]]
          color <- col[i]
          for (i in seq(from = 1, to = nrow(v), by=2)){
            rgl::arrow3d(v[i,], v[i+1,], type = c.type, col = color, width = arrow.width, n = n, theta = theta, barblen = barblen)
          }
        }
      } else {
        sapply(seq(from = 1, to = nrow(v), by=2), function(i){
          rgl::arrow3d(vec[i,], vec[i+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
      }
    }
    if (item.lab == TRUE && is.null(items)){
      if (is.null(diff.level)){
        if (is.null(item.names)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(inames[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        } else {
          if(!length(item.names) <= nrow(x$loadings)) warning("There are more item labels than items")
          if(length(item.names) < nrow(x$loadings)) warning("There are too few item labels")
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.names[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          } )
        }
      } else {
        inames <- rownames(x$loadings)
        dl <-  x$scal.vec[[diff.level]]
        sapply(seq(nrow(x$mdisc)), function(i){
          rgl::text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
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
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        } else {
          if(!length(item.names) <= length(items)) warning("There are more item labels than items in list")
          if(length(item.names) < length(items)) warning("There are too few item labels")
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(item.names[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        }
      } else {
        if (is.null(item.names)){
          dl <-  x$scal.vec[[diff.level]]
          inames <- rownames(x$loadings)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        } else {
          dl <-  as.data.frame(x$scal.vec[diff.level], drop = FALSE)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(item.names[i]), color = axis.col,
                        adj = adjustlab, size = 2)
          })
        }
      }
    }
  } # hide false
} # hide false
  if (constructs == TRUE){
    if (is.null(x$c.vec)) warning("3D mirt object does not contain any constructs")
    cvec <- x$c.vec
    sapply(seq(from = 1, to = nrow(cvec), by=2), function(x){
      rgl::arrow3d(cvec[x,]*con.scalars[1], cvec[x+1,]*con.scalars[2], type = c.type, col = c.col, width = c.arrow.width, n = c.n, theta = c.theta, barblen = c.barblen)
    })
    if (!is.null(construct.lab) && constructs == TRUE){
      if(!length(construct.lab) <= nrow(x$c.vec)) warning("There are more construct labels than constructs")
      clab <-  x$c.vec*con.scalars[2]
      sapply(seq(nrow(x$c.dir.cos)), function(i){
        rgl::text3d(clab[(i*2),1],clab[(i*2),2], clab[(i*2),3], text = c(construct.lab[i]), color = axis.col,
                    adj = adjustlab, size = 2)
      })
    }
  }
  if (!is.null(profiles)){
    if(!is.data.frame(profiles)) stop("Profiles must be data frame or matrix")
    x <- profiles[,1]
    y <- profiles[,2]
    z <- profiles[,3]
    rgl::spheres3d(x,y,z, radius = spheres.r, color = sphere.col) # double check
    if (ellips == TRUE){
      ellips <- rgl::ellipse3d(cov(cbind(x,y,z)),
                               centre=c(mean(x), mean(y), mean(z)), level = CI.level)
      rgl::shade3d(ellips, col = ellips.col, alpha = ellips.alpha)
    }
  }
}
