#' Graphical Output for D3mirt
#'
#' @description For graphing of objects of class `D3mirt` from the [D3mirt::D3mirt()] function.
#' @param x S3 object of class `D3mirt`.
#' @param scale Logical, if item vector arrow length should visualize the MDISC estimates. If set to FALSE, the vector arrow length will be one unit length. Default is `scale = FALSE`.
#' @param hide Logical, if items should be plotted. Default is `hide = FALSE`.
#' @param diff.level Optional. Plotting of a single level of difficulty indicated by an integer.
#' @param items Optional. The user can input a list of integers indicating what item vector arrows will be visible while the remaining items are hidden.
#' @param item.names Logical, if item labels should be plotted. Default is `item.names = TRUE`.
#' @param item.lab Optional. String vector of item names that will override row names extracted from the data frame.
#' @param constructs Logical, if construct vector arrows should be plotted. Default set to FALSE
#' @param construct.lab Optional. String vector of names for constructs.
#' @param adjust.lab Vector of parameters for the position of item and construct labels for the `text3d` function. The first value is for horizontal adjustment and the second is for vertical adjustment. Default is `adjust.lab = c(0.5, -0.8)`.
#' @param x.lab Labels for x-axis, Default is `x.lab = "X"`.
#' @param y.lab Labels for y-axis, Default is `y.lab = "Y"`.
#' @param z.lab Labels for y-axis, Default is `z.lab = "Z"`.
#' @param title The main title for the graphical device, plotted with the `title3d()` function.
#' @param line  Title placement for `title3d()`. Default is `line = -5`.
#' @param axis.scalar Scalar factors to adjusts the length of the axes (x, y, z) in the 3D model. Default is `axis.scalar = c(1.1,1.1,1.1)`
#' @param axis.col Color of axis for the `segment3D()`function, default is `axis.col = "Black"`.
#' @param axis.points Color of axis points for the `points3d()` function. Default is `axis.points = "black"`.
#' @param points Logical, if axis from `points3d()` have end points. Default is `points = TRUE`.
#' @param axis.ticks Logical, if axis ticks from the `axis3d()` function should be plotted. Default is `axis.ticks = TRUE'.
#' @param nticks Number of ticks for `axis3d()` indicated with integers for the x, y, and z axes. Default is `nticks = c(10,10,10)`.
#' @param width.rgl.x Width in the x direction for `par3d()`. Default is `width.rgl.x = 1040`.
#' @param width.rgl.y Width in the y direction for `par3d()`. Default is `width.rgl.y = 1040`.
#' @param view Vector with polar coordinates and zoom factor for the `view3d` function. Default is `view = c(15,20, 0.7)`.
#' @param show.plane Logical, if xz-plane should be visible in the graphical device. Default is `show.plane = TRUE`.
#' @param plane.color Color of the plane, default is `plane.color = "grey80"`.
#' @param background Set background color for the graphical device, Default is `background = "white"`.
#' @param type Type of vector arrow for items, default is `type = "rotation"`. See [rgl::arrow3d] for more options regarding arrow types.
#' @param col Vector of colors representing difficulty levels for item response functions used in `arrow3d()`. Default is `col = c("black", "grey20", "grey40", "grey60", "grey80")`.
#' @param arrow.width Width of vector arrows for `arrow3d()`. Default is `arrow.width = 0.6`.
#' @param n Number of barbs for the vector arrows from `arrow3d()`. Default is `n = 20`.
#' @param theta Opening angle of barbs for vector arrows from `arrow3d()`. Default is `theta = 0.2`.
#' @param barblen The length of the barbs for vector arrows from `arrow3d()`. Default is `barblen = 0.03`.
#' @param c.scalars Set of scalars for adjusting construct arrow length. The first numeric adjust the length in the negative direction and the second numeric the length in the positive direction. Default is `c.scalars = c(1,1)`.
#' @param c.type Type of vector arrow for constructs. See [rgl::arrow3d] for more options regarding arrow types. Default is `c.type = "rotation"`.
#' @param c.col Color for construct vector arrows from `arrow3d()`, default is `c.col = "black"`.
#' @param c.arrow.width Width of construct vector arrows for `arrow3d()`. Default is `c.arrow.width = 0.6`.
#' @param c.n Number of barbs for the construct vector arrows from the `arrow3d()` function. Default is `c.n = 20`.
#' @param c.theta Opening angle of barbs for construct vector arrows from `arrow3d()`. Default is `c.theta = 0.2`.
#' @param c.barblen The length of the barbs for construct vector arrows from `arrow3d()`. Default is `c.barblen = 0.03`.
#' @param profiles Data frame with coordinates for spheres representing respondent scores. Default is `profiles = NULL`.
#' @param lev Optional. Column with values indicating levels for sphere colors from the `sphere.col` vector. Default is `lev = NULL`.
#' @param spheres.r Radius of the spheres for `spheres3d()`. Default is `spheres.r = 0.05`.
#' @param sphere.col Color vector for `spheres3d()`. Default is `sphere.col = c("black", "grey20", "grey40", "grey60", "grey80")`.
#' @param ellipse Logical, if spheres should include an ellipsoid outlining a confidence region returned from the `ellipse3d()` function. Default is `ellipse = TRUE`.
#' @param CI.level Level of confidence for `ellipse3d()`, default is `CI.level = 0.95`.
#' @param ellipse.col Color of the ellipse from `ellipse3d()`. Default is `ellipse.col = "grey80"`.
#' @param ellipse.alpha Opacity for the confidence region from `ellipse3d()`. Default is `ellipse.alpha = 0.20`.
#' @param ... Additional arguments to be passed to RGL or methods.
#'
#'
#' @import rgl
#' @importFrom stats cov
#'
#' @details The function is based on the [rgl] package for visualization with OpenGL. outputs a three-dimensional interactive RGL device containing the descriptive multidimensional item response theory model with orthogonal standardized axes centered at 0.
#' An open RGL device can be exported to the R console interactive html file or as a still shoots (see examples below).
#' In the case of the latter, the model perspective in the still shoot can be manually adjusted or adjusted by changing the `view` argument for the function.
#'
#' # Plotting options
#' The function allows plotting of all items, a selection of items as well as plotting a single item (see examples section).
#' Items can also be plotted with unit length by setting `scale = TRUE`.
#' In addition, the user also has the option of adding constructs to the graphical output with `constructs = TRUE` (see the documentation for [D3mirt::D3mirt] regarding constructs).
#' Plotting can be limited to showing one level of difficulty with the `diff.level` argument at a time if multiple levels of difficulty are used in the model.
#' Item names are displayed by default, but the user has the option of imputing new names for the items (with `item.lab`) and adding names for the constructs (with `construct.lab`).
#'
#' # Visual Profile Analysis
#' In addition, the plot function can also display respondent scores in the model space, represented as spheres located with the help of factors scores as coordinates in the model.
#' This allows for a type of profile analysis of respondent groups in which respondents' are selected and displayed based on some external criteria (see examples section below).
#' This is done by first extracting respondent factor scores with [mirt::fscores](Chalmers, 2012) and then inducing some selection process that subset respondent rows.
#' The resulting data frame is then imputed in the `profiles` argument.
#'
#'
#' The user also has the option of imputing a grouping variable in the `lev` argument for the profile data that allows for varying the coloring of spheres based on a group variable indicator.
#' The most easy way to achieve this is to use a single Likert-item as the selection criteria.
#' More specifically, if respondent factor scores from the `fscores`function are combined column wise with respondents Likert scores on the criteria item, then `plotD3mirt()` use the `as.factor()`function to coerce the different Likert score options to be factor variables.
#' These factor variables indicate what color to use from the `sphere.col` argument, e.g., respondents who gave a response of 1 on the likert item will be colored by the first color in the color vector, respondents ho gave a response of 2 will be colored by the second color, and so on.
#' If the there are more factors than there are different colors contained in the `sphere.col` vector, then the user most add more color alternatives to the color vector.
#' See examples section for more options on how to perform prfile analysis.
#' A general advice is also to hiding vector arrows with `hide = TRUE` when analyzing respondent profiles to avoid visual cluttering.
#'
#' # Guidelines for Interpretating the Graphical Output
#' In general, vector arrows represent item response functions and the location, angle, and length of the arrows indicate item characteristics (Reckase, 2009).
#' If polytomous items, such as Likert items, are used then each an item will have multiple item response functions that run successively long the same line.
#'
#'
#' The distance of the lower end of the vector arrows away from the origin indicates the an items multidimensional difficulty (MDIFF).
#' For Likert items that hold multiple item response functions, the MDIFF can, therefore, be said to show the multidimensional range of difficulty for an item.
#'
#'
#' The length of the arrow indicates the item's level of multidimensional discrimination (MDISC).
#' Longer arrows indicates high discrimination and shorter arrows indicates lower discrimination.
#'
#'
#' The angle of the vector arrows, seen from the model axes, indicates the direction of maximal slope of discrimination for the particular item response function.
#' In turn, this also indicates what singular traits, located along the orthogonal axes, an item can be said to describe.
#' For instance, an item located at 0° seen from x-axis, and 90° as seen from the y and z-axis, only describes trait x.
#' Such an item is unidimensional because its direction vector of maximal discrimination slope lies parallel and on the x-axis.
#' In contrast, an item located at 45° between all three axes in a three-dimensional model describes all three traits in the model equally well.
#' Such an item is within-multidimensional because its direction vector lies parallel and on the 45° degree line.
#'
#' # Model Violations
#' Since descriptive multidimensional item response theory is based on the multidimensional version of the graded response model (Samejima, 1969), all items must adequately meet the necessary statistical assumptions of this type of model.
#' In other words, this implies that violations of the multidimensional graded response model can be observed visually in the `plotD3mirt()` graphical device.
#' For instance, shorter vector arrows indicate weaker discrimination on level of ability.
#' Moreover, if an item struggles or fail to describe any of the latent variables in the model it can be observed as an extreme stretch of the MDIFF range.
#' This is comparable to observing horizontal trace lines in a unidimensional item response theory model turn horizontal.
#'
#' @return A RGL graphical device.
#' @author Erik Forsberg
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' @references Reckase, M. D. (2009). \emph{Multidimensional Item Response Theory}. Springer.
#' @references Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. \emph{Psychometrika 34}, 1–97. https://doi.org/10.1007/BF03372160
#'
#' @examples
#' \dontrun{
#' # Create S3 object of class D3mirt
#' g <- D3mirt(x)
#'
#' # Create nested lists for constructs
#' c <- list(list(1,3,4,6,8), list(2,5,7,9,10))
#' g <- D3mirt(d, c)
#'
#' # Plot RGL device
#' plotD3mirt(g)
#'
#' # Plot RGL device on one level of difficulty
#' plotD3mirt(g, diff.level = 5)
#'
#' # Plot RGL device with constructs visible and scaled items
#' plotD3mirt(g, scale = TRUE, constructs = TRUE)
#'
#' # Plot a selection of items from the model with named constructs
#' plotD3mirt(g, constructs = TRUE, items = c(1,3,4,6,8), construct.lab = c("I_1", "I_2"))
#'
#' # Profile analysis
#' # Extract respondent factor scores from mod1 with `fscores()` function from [mirt::mirt]
#' library(mirt)
#' f <- data.frame(fscores(mod.32, method="EAP", full.scores = TRUE, full.scores.SE = F, QMC = T))
#'
#' # Attach f to column 10 from data frame x containing empirical scores
#' # Column bind fscores output first
#' y <- data.frame(cbind(f, x[,10]))
#'
#' # Subset data frame y conditioned on values < 2 in column 4
#' z <- subset(y, y[,4] < 2)
#'
#' # Subset data frame y conditioned on values >= 2 and <= 4 in column 4
#' z <- subset(y, y[,4] >= 2 & y[,4] <= 4)
#'
#' # Call plotD3mirt with profiles, hidden items
#' # And 3 levels of sphere coloring ("black" = 1 to "grey60" = 3)
#' plotD3mirt(g, hide = TRUE, profiles = z, lev = z[,4], lev.color = c("black", "grey40", "grey60"))
#'
#' # Same example but with lowest score highlighted with red
#' plotD3mirt(g, hide = TRUE, profiles = z, lev = z[,4], lev.color = c("red", "grey40", "grey60"))
#'
#' # The use of `rep()`makes it possible to create groups by number of colors in color vector
#' # Can be useful on variables with a high amount of indicator levels
#' # Subset data frame y conditioned on age <= 30 in column 10
#' z <- subset(y, y[,4] >= 2 & y[,4] <= 30)
#'
#' # Check number of factor levles with `nlevels()`
#' nlevels(as.factor(z[,4]))
#'
#' # Repetition of colors can be used to create color groups based on the `factor()` output
#' # Assume we want to divide 20 factors in the age variable into 4 color groups
#' # Ordered from young to old
#' z <- z[order(z[,4]), decreasing = FALSE]
#'
#' # Create color vector with 4 color groups, youngest will be colored red
#' colvec <- c(rep("red", 5), rep("orange", 5), rep("violet", 5), rep("cyan", 5))
#'
#' # Call plotD3mirt with profiles on age and with hidden items
#' plotD3mirt(g, hide = TRUE, profiles = z, lev = z[,4], lev.color = colvec)
#'
#' # Export an open RGL device to consol and html
#' s <- scene3d()
#' plotD3mirt(g, constructs = TRUE)
#' rgl::rglwidget(width = 1040, height = 1040)
#'
#' # Export a snap shoot of an open RGL device to file
#' plotD3mirt(g, constructs = TRUE)
#' rgl::rgl.snapshot('RGLdevice.png', fmt = 'png')
#' }
#' @export
plotD3mirt <- function (x, scale = FALSE, hide = FALSE, diff.level = NULL, items = NULL, item.names = TRUE,  item.lab = NULL,
                        constructs = FALSE, construct.lab = NULL, adjust.lab = c(0.5, -0.8),
                        x.lab = "X", y.lab="Y", z.lab="Z", title="", line = -5,
                        axis.scalar = c(1.1,1.1,1.1), axis.col = "black", axis.points = "black",
                        points = TRUE, axis.ticks = TRUE, nticks = c(4,4,4),  width.rgl.x = 1040, width.rgl.y= 1040, view = c(15,20, 0.7),
                        show.plane = TRUE, plane.color = "grey80", background = "white",
                        type = "rotation", col = c("black", "grey20", "grey40", "grey60", "grey80"),
                        arrow.width = 0.6, n = 20, theta = 0.2, barblen = 0.03,
                        c.scalars = c(1,1),
                        c.type = "rotation", c.col = "black", c.arrow.width = 0.6,
                        c.n = 20, c.theta = 0.2, c.barblen = 0.03,
                        profiles = NULL, lev = NULL, sphere.col = c("black", "grey20", "grey40", "grey60", "grey80"), spheres.r = 0.05,
                        ellipse = TRUE, CI.level = 0.95, ellipse.col = "grey80", ellipse.alpha = 0.20, ...){
  if (!isa(x, "D3mirt")) stop("Input object must be of class D3mirt")
  rgl::open3d()
  rgl::par3d(windowRect = 50 + c( 0, 0, width.rgl.x, width.rgl.y))
  rgl::bg3d(color = background)
  rgl::view3d(theta = view[1], phi = view[2], zoom = view[3])
  if (is.null(ncol(x$dir.vec))){
    ax <- x$dir.vec
    low <- as.data.frame(ax[1], drop = FALSE)
    hig <- as.data.frame(ax[length(ax)], drop = FALSE)
    xaxis.min <- min(low[,1])*axis.scalar[1]
    xaxis.max <- max(hig[,1])*axis.scalar[1]
    yaxis.min <- min(low[,2])*axis.scalar[2]
    yaxis.max <- max(hig[,2])*axis.scalar[2]
    zaxis.min <- min(low[,3])*axis.scalar[3]
    zaxis.max <- max(hig[,3])*axis.scalar[3]
  } else {
    ax <- x$dir.vec
    xaxis.min <- min(ax[,1])*axis.scalar[1]
    xaxis.max <- max(ax[,1])*axis.scalar[1]
    yaxis.min <- min(ax[,2])*axis.scalar[2]
    yaxis.max <- max(ax[,2])*axis.scalar[2]
    zaxis.min <- min(ax[,3])*axis.scalar[3]
    zaxis.max <- max(ax[,3])*axis.scalar[3]
  }
  xaxis <- c(-abs(xaxis.min), abs(xaxis.max))
  yaxis <- c(-abs(yaxis.min), abs(yaxis.max))
  zaxis <- c(-abs(zaxis.min), abs(zaxis.max))
  rgl::segments3d(xaxis, c(0, 0), c(0, 0), color = axis.col)
  rgl::segments3d(c(0, 0), yaxis, c(0, 0), color = axis.col)
  rgl::segments3d(c(0, 0), c(0, 0), zaxis, color = axis.col)
  if (axis.ticks == TRUE){
    if(!length(nticks) == 3) stop("The nticks argument must be indicated with three values")
    rgl::axis3d('x', pos = c(0, 0, 0), ticks = TRUE, nticks=nticks[1])
    rgl::axis3d('y', pos = c(0, 0, 0), ticks = TRUE, nticks=nticks[2])
    rgl::axis3d('z',pos = c(0, 0, 0), ticks = TRUE, nticks=nticks[3])
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
    if (scale == FALSE){
    vec <- x$dir.vec
    if (!is.null(items)){
      if(any(!items <= nrow(x$loadings))) stop("The items argument contains one or more item indicators that are higher than the total number of items")
      if (any(duplicated(items))) stop("The items argument has duplicate elements")
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
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
        if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
        })
      }
    }
    else if (!is.null(diff.level)) {
      if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
      if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
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
    if (item.names == TRUE && is.null(items)){
      if (is.null(diff.level)){
        if (is.null(item.lab)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(inames[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          if(!length(item.lab) <= nrow(x$loadings)) warning("There are more item labels than items")
          if(length(item.lab) < nrow(x$loadings)) warning("There are too few item labels")
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          } )
        }
      } else {
        inames <- rownames(x$loadings)
        dl <-  x$dir.vec[[diff.level]]
        sapply(seq(nrow(x$mdisc)), function(i){
          rgl::text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
                      adj = adjust.lab, size = 2)
        })
      }
    }
    if (item.names == TRUE && !is.null(items)){
      if(any(!items <= nrow(x$loadings))) stop("The items list contains one or more item indicators that are higher than the total number of items")
      if (is.null(diff.level)){
        if (is.null(item.lab)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          if(!length(item.lab) <= length(items)) warning("There are more item labels than items in list")
          if(length(item.lab) < length(items)) warning("There are too few item labels")
          if (is.null(ncol(vec))){
            max <-  x$dir.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$dir.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        }
      } else {
        if (is.null(item.lab)){
          dl <-  x$dir.vec[[diff.level]]
          inames <- rownames(x$loadings)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          dl <-  as.data.frame(x$dir.vec[diff.level], drop = FALSE)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        }
      }
    }
  } else {
    vec <- x$scal.vec
    if (!is.null(items)){
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
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
        if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
        })
      }
    }
    else if (!is.null(diff.level)) {
      if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
      if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
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
    if (item.names == TRUE && is.null(items)){
      if (is.null(diff.level)){
        if (is.null(item.lab)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(inames[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          if(!length(item.lab) <= nrow(x$loadings)) warning("There are more item labels than items")
          if(length(item.lab) < nrow(x$loadings)) warning("There are too few item labels")
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          } )
        }
      } else {
        inames <- rownames(x$loadings)
        dl <-  x$scal.vec[[diff.level]]
        sapply(seq(nrow(x$mdisc)), function(i){
          rgl::text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
                      adj = adjust.lab, size = 2)
        })
      }
    }
    if (item.names == TRUE && !is.null(items)){
      if(any(!items <= nrow(x$loadings))) stop("The items list contains one or more item indicators that are higher than the total number of items")
      if (is.null(diff.level)){
        if (is.null(item.lab)){
          inames <- rownames(x$loadings)
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          if(!length(item.lab) <= length(items)) warning("There are more item labels than items in list")
          if(length(item.lab) < length(items)) warning("There are too few item labels")
          if (is.null(ncol(vec))){
            max <-  x$scal.vec[[ncol(x$mdiff)]]
          } else {
            max <-  x$scal.vec
          }
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(max[m*2,1],max[m*2,2], max[m*2,3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        }
      } else {
        if (is.null(item.lab)){
          dl <-  x$scal.vec[[diff.level]]
          inames <- rownames(x$loadings)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(inames[m]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          dl <-  as.data.frame(x$scal.vec[diff.level], drop = FALSE)
          sapply(seq_along(items), function(i){
            m <- items[i]
            rgl::text3d(dl[m*2,1],dl[m*2,2], dl[m*2,3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        }
      }
    }
  }
}
  if (constructs == TRUE){
    if (is.null(x$c.vec)) warning("3D mirt object does not contain any constructs")
    cvec <- x$c.vec
    sapply(seq(from = 1, to = nrow(cvec), by=2), function(x){
      rgl::arrow3d(cvec[x,]*c.scalars[1], cvec[x+1,]*c.scalars[2], type = c.type, col = c.col, width = c.arrow.width, n = c.n, theta = c.theta, barblen = c.barblen)
    })
    if (!is.null(construct.lab) && constructs == TRUE){
      if(!length(construct.lab) <= nrow(x$c.vec)) warning("There are more construct labels than constructs")
      clab <-  x$c.vec*c.scalars[2]
      sapply(seq(nrow(x$c.dir.cos)), function(i){
        rgl::text3d(clab[(i*2),1],clab[(i*2),2], clab[(i*2),3], text = c(construct.lab[i]), color = axis.col,
                    adj = adjust.lab, size = 2)
      })
    }
  }
  if (!is.null(profiles)){
    x <- profiles[,1]
    y <- profiles[,2]
    z <- profiles[,3]
    if (!is.null(lev)){
      grad <- function (lev, sphere.col){
      lev <- as.factor(lev)
      if (nlevels(lev) > length(sphere.col)) stop ("There number of factor levels are more than the number of available sphere colors")
      color <- sphere.col[as.numeric(lev)]
      names(color) <- as.vector(lev)
      color
      }
      rgl::spheres3d(x,y,z, radius = spheres.r, color = grad(lev, sphere.col))
      if (ellipse == TRUE){
        ellipse <- rgl::ellipse3d(cov(cbind(x,y,z)),
                                  centre=c(mean(x), mean(y), mean(z)), level = CI.level)
        rgl::shade3d(ellipse, col = ellipse.col, alpha = ellipse.alpha)
    }
    } else {
    rgl::spheres3d(x,y,z, radius = spheres.r, color = sphere.col[1])
      if (ellipse == TRUE){
        ellipse <- rgl::ellipse3d(cov(cbind(x,y,z)),
                                  centre=c(mean(x), mean(y), mean(z)), level = CI.level)
        rgl::shade3d(ellipse, col = ellipse.col, alpha = ellipse.alpha)
      }
    }
  }
}
