#' Graphical Output for D3mirt()
#'
#' @description For graphing of objects of class `D3mirt` from the [D3mirt::D3mirt()] function using the rgl 3D visualization device system (Adler & Murdoch, 2022).
#' @param x S3 object of class `D3mirt`.
#' @param scale Logical, if item vector arrow length should visualize the MDISC. If set to FALSE, the vector arrow length will be of one unit length. The default is `scale = FALSE`.
#' @param hide Logical, if items should be plotted. The default is `hide = FALSE`.
#' @param diff.level Optional. Plotting of a single level of difficulty indicated by an integer.
#' @param items Optional. The user can input a list of integers indicating what item vector arrows will be visible while the remaining item vector arrows are hidden.
#' @param item.names Logical, if item labels should be plotted. The default is `item.names = TRUE`.
#' @param item.lab Optional. String vector of item names that will override row names extracted from the data frame. Note, row names are not overwritten.
#' Instead, the string vector in `ìtem.lab` prints item labels on the item vector arrows currently displayed following the order of item vector arrows in the graphical output.
#' For example, when plotting in the default mode (plotting all item vectors) the labels will follow the order of the items in the data frame.
#' If a selection of items is plotted with `items`, e.g., `ìtems = c(24,34,25)`, then the item labels will be displayed following the order of the vector in `items` left to right.
#' In this case, item label 1 will be printed on item 24, item label 2 on item 34, and item label 3 on item 25, and so on.
#' @param constructs Logical, if construct vector arrows should be plotted. The default is `constructs = FALSE`.
#' @param construct.lab Optional. String vector of names for constructs, similar to `item.lab`.
#' @param adjust.lab Vector of parameters for the position of the item and construct labels for the `text3d` function.
#' The first value is for horizontal adjustment and the second is for vertical adjustment. The default is `adjust.lab = c(0.5, -0.8)`.
#' @param x.lab Labels for x-axis, the default is `x.lab = "X"`.
#' @param y.lab Labels for y-axis, the default is `y.lab = "Y"`.
#' @param z.lab Labels for y-axis, the default is `z.lab = "Z"`.
#' @param title The main title for the graphical device, plotted with the `title3d()` function. The default is no title.
#' @param line  Title placement for `title3d()`. The default is `line = -5`.
#' @param axis.scalar Scalar multiple for adjusting the length of all axes (x, y, z) in the 3D model proportionally. The default is `axis.scalar = 1.1`.
#' @param axis.length Optional. For adjusting the length of the axis manually by entering a numeric or a numeric vector.
#' For instance, c(3,2,4,3,3,2) indicate axis coordinates x = 3, -x = 3, y = 4, -y = 3, z = 3, -z = 2.
#' Note, a symmetric model can be created easily by adding a single numeric in the `axis.length` argument (e.g., `axis.length = 4`) because the function repeats the last value in the vector to cover all axis points.
#' The default is `axis.length = NULL`.
#' @param axis.col Color of axis for the `segment3D()`function, the default is `axis.col = "black"`.
#' @param axis.points Color of axis points for the `points3d()` function. The default is `axis.points = "black"`.
#' @param points Logical, if axis from `points3d()` should have end points. The default is `points = TRUE`.
#' @param axis.ticks Logical, if axis ticks from the `axis3d()` function should be plotted. The default is `axis.ticks = TRUE`.
#' @param nticks Number of ticks for `axis3d()`.
#' The function repeats the last numeric value in the vector to cover all axis.
#' The user can, therefore, adjust the number of ticks with one numeric value (e.g., `nticks = 6`) or up to three (e.g., `nticks = c(6,4,8)` corresponding to the for the x, y, and z axes respectively.
#' The default is `nticks = 4`.
#' @param width.rgl.x Width in the x direction for `par3d()`. The default is `width.rgl.x = 1040`.
#' @param width.rgl.y Width in the y direction for `par3d()`. The default is `width.rgl.y = 1040`.
#' @param view Vector with polar coordinates and zoom factor for the `view3d` function. The default is `view = c(15,20, 1)`.
#' @param show.plane Logical, if xz-plane should be visible in the graphical device. The default is `show.plane = TRUE`.
#' @param plane.col Color of the plane, the default is `plane.col = "grey80"`.
#' @param background Set background color for the graphical device, the default is `background = "white"`.
#' @param type Type of vector arrow for items, the default is `type = "rotation"`. See [rgl::arrow3d] for more options regarding arrow types.
#' @param col Vector of colors representing difficulty levels for item response functions used in `arrow3d()`. The default is `col = c("black", "grey20", "grey40", "grey60", "grey80")`.
#' @param arrow.width Width of vector arrows for `arrow3d()`. The default is `arrow.width = 0.6`.
#' @param n Number of barbs for the vector arrows from `arrow3d()`. The default is `n = 20`.
#' @param theta Opening angle of barbs for vector arrows from `arrow3d()`. The default is `theta = 0.2`.
#' @param barblen The length of the barbs for vector arrows from `arrow3d()`. The default is `barblen = 0.03`.
#' @param c.scalars Set of scalars for adjusting construct arrow length proportionally.
#' The first numeric adjusts the length in the positive direction and the second numeric the length in the negative direction. The default is `c.scalars = c(1,1)`.
#' @param c.type Type of vector arrow for constructs. See [rgl::arrow3d] for more options regarding arrow types. The default is `c.type = "rotation"`.
#' @param c.col Color of construct vector arrows from `arrow3d()`, the default is `c.col = "black"`.
#' @param c.arrow.width Width of construct vector arrows for `arrow3d()`. The default is `c.arrow.width = 0.6`.
#' @param c.n Number of barbs for the construct vector arrows from the `arrow3d()` function. The default is `c.n = 20`.
#' @param c.theta Opening angle of barbs for construct vector arrows from `arrow3d()`. The default is `c.theta = 0.2`.
#' @param c.barblen The length of the barbs for construct vector arrows from `arrow3d()`. The default is `c.barblen = 0.03`.
#' @param profiles Data frame with coordinates for spheres representing respondent scores. The default is `profiles = NULL`.
#' @param levels Optional. A column with values indicating levels for sphere colors from the `sphere.col` vector. The default is `levels = NULL`.
#' @param spheres.r Radius of the spheres for `spheres3d()`. The default is `spheres.r = 0.05`.
#' @param sphere.col Color vector for `spheres3d()`. The default is `sphere.col = c("black", "grey20", "grey40", "grey60", "grey80")`.
#' @param ci Logical, if spheres should include an ellipsoid outlining a confidence region returned from the `ellipse3d()` function. The default is `ci = FALSE`.
#' @param ci.level Level of confidence for `ellipse3d()`, the default is `ci.level = 0.95`.
#' @param ellipse.col Color of the ellipse from `ellipse3d()`. The default is `ellipse.col = "grey80"`.
#' @param ellipse.alpha Opacity for the confidence region from `ellipse3d()`. The default is `ellipse.alpha = 0.20`.
#' @param ... Additional arguments to be passed to RGL or methods.
#'
#'
#' @import rgl
#' @importFrom stats cov
#' @importFrom mirt fscores
#'
#' @details The function is used for graphing class `D3mirt` S3 objects in three dimensions using the [rgl] package for visualization with OpenGL (Adler & Murdoch, 2022).
#'
#' # Unidimensionality vs. Within-Dimensionality
#' In DMIRT, the angle of the vector arrows indicates what traits, located along the orthogonal axes, an item can be said to describe (Reckase, 2009, 1985, Reckase & McKinley, 1991).
#' For instance, an item located at 0° seen from the x-axis, and 90° as seen from the y and z-axis, only describes trait x.
#' Such an item is unidimensional because its direction vector lies parallel and on the x-axis.
#' In contrast, an item located at 45° between all three axes in a three-dimensional model describes all three traits in the model equally well.
#' Such an item is within-multidimensional with respect to all three latent traits used in the analysis because its direction vector points in the neutral 45° direction in the model.
#'
#' # Model Violations
#' When plotting the `D3mirt` model with `plotD3mirt()`, it is possible to visually observe statistical violations in the graphical output returned.
#' For instance, shorter vector arrows indicate weaker discrimination and therefore also higher amounts of statistical violations.
#' Moreover, if a polytomous item struggles or even fail to describe any of the latent variables in the model, it can often lead to an extreme stretch of the MDIFF range.
#' This is comparable to trace lines turning horizontal in a unidimensional item response theory model.
#'
#' # Plotting Options
#' The plotting function allows plotting of all items, a selection of items as well as plotting a single item.
#' Length of the vector arrows can be set to one unit length across all item vector arrows by setting `scale = TRUE`.
#' This removes the visualization of the MDISC parameter.
#' Note, when scaling items with `scale = TRUE`, the `plotD3mirt()` function does not change the length of the model axis.
#' This often means that the axes of the model need to be adjusted, which can be achieved proportionally with `axis.scalar` or manually with `axis.length`.
#'
#' The user also has the option of adding constructs to the graphical output with `constructs = TRUE` (see the documentation for [D3mirt::D3mirt] or the package vignette regarding constructs).
#' Other options include plotting one level of difficulty at a time with the `diff.level` argument if polytomous items are used in the model.
#' Item row names are displayed by default, but the user has the option of adding new item labels for the items (with `item.lab`), as well as labeling constructs (with `construct.lab`).
#'
#' # Profile Analysis
#' The plot function can display respondent scores in the three-dimensional model space, represented as spheres located with factors scores used as coordinates.
#' This allows for a profile analysis in which respondent rows are separated or selected conditioned on one or more external criteria.
#' To do this, the user must first extract respondent factor scores with [mirt::fscores](Chalmers, 2012) and then use some selection process to separate or subset respondent rows.
#' The resulting data frame is used in the `profiles` argument.
#' If desired, a confidence interval can be added for the respondent scores by setting `ci = TRUE`.
#' A general advice is also to hide vector arrows with `hide = TRUE` when analyzing respondent profiles to avoid visual cluttering.
#' For more on profile analysis (e.g., preparation and examples), see package vignette.
#'
#' # Exporting The RGL Device
#' The returned RGL device can, for example, be exported to the R console and be saved as an interactive html file or as a still shoot (see examples below).
#' In the case of the latter, the model perspective in the still shoot can be manually adjusted by changing the `view` argument for the function.
#'
#' @return A RGL graphical device.
#' @author Erik Forsberg
#' @references Adler, D., & Murdoch, D. (2022). \emph{Rgl: 3d Visualization Using OpenGL} Computer software.
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#'
#' @examples
#' \donttest{
#' # To plot, the data must be prepared with mirt::mirt and the D3mirt() function
#' # Load data
#' data("anes0809offwaves")
#' x <- anes0809offwaves
#' x <- x[,3:22] # Remove columns for age and gender
#'
#' # Fit a three-dimensional graded response model with orthogonal factors
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
#' # Call D3mirt() with data frame d and constructs assigned to c
#' c <- list(list(1,2,3,4),
#'           list(5,7,8,9,10),
#'           list(11,12,13,14,15,15,16,17,18,19,20))
#' g <- D3mirt(d, c)
#' plotD3mirt(g)
#'
#' # Plot RGL device with constructs visible and named
#' plotD3mirt(g,
#'            constructs = TRUE,
#'            construct.lab = c("Fairness", "Conformity", "Compassion"))
#'
#' # Item W7Q16 has location 6 in the data set (gender and age excluded)
#' # The item is plotted together with construct to aid the visual interpretation
#' plotD3mirt(g,
#'            constructs = TRUE,
#'            items = 6,
#'            construct.lab = c("Fairness", "Conformity", "Compassion"))
#'
#' # Plot RGL device on item difficulty level 5
#' plotD3mirt(g,
#'            diff.level = 5)
#'
#' # A selection of Conformity items from the model plotted with constructs
#' plotD3mirt(g,
#'            constructs = TRUE,
#'            items = c(5,7,8,9,10),
#'            construct.lab = c("Fairness", "Conformity", "Compassion"))
#'
#' # Plot RGL device with scaled items and constructs visible and named
#' plotD3mirt(g,
#'            scale = TRUE,
#'            constructs = TRUE,
#'            construct.lab = c("Fairness", "Conformity", "Compassion"))
#'
#' # Profile Analaysis
#' # Extract respondent factor scores from mod1 (see D3mirt()) with mirt::fscores()
#' f <- mirt::fscores(mod1,
#'                    method="EAP",
#'                    full.scores = TRUE,
#'                    full.scores.SE = FALSE, QMC = TRUE)
#'
#' # Attach f to the gender variable (column 2 from anes0809offwaves data set; "W3XGENDER")
#' # Use cbind with fscores() output attached first
#' x <- anes0809offwaves
#' z <- data.frame(cbind(f, x[,2]))
#'
#' # Plot profiles with item vector arrows hidden
#' # Score levels: 1 = Blue ("male") and 2 = Red ("female")
#' plotD3mirt(g, hide = TRUE,
#'            profiles = z,
#'            levels = z[,4],
#'            sphere.col = c("blue", "red"),
#'            x.lab = "Compassion",
#'            y.lab="Conformity",
#'            z.lab="Fairness")
#'
#' # Add a 95% CI to respondent factor scores on <= 30 y.o.
#' # Column bind fscores() with age variable ("W3Xage")
#' y <- data.frame(cbind(f, x[,1]))
#'
#' # Subset data frame y conditioned on age <= 30
#' z1 <- subset(y, y[,4] <= 30)
#'
#' # Use rep() to create a color vector to color groups based on the nlevels() output
#' # z1 has 14 factor levels
#' colvec <- c(rep("red", 14))
#'
#' # Call plotD3mirt with profile data on age with item vector arrows hidden
#' plotD3mirt(g, hide = TRUE,
#'            profiles = z1,
#'            levels = z1[,4],
#'            sphere.col = colvec,
#'            x.lab = "Compassion",
#'            y.lab="Conformity",
#'            z.lab="Fairness",
#'            ci = TRUE,
#'            ci.level = 0.95,
#'            ellipse.col = "orange")
#'
#' # Export an open RGL device to the console to be saved as html or image file
#' plotD3mirt(g,
#'            constructs = TRUE)
#' s <- scene3d()
#' rgl::rglwidget(s,
#'                width = 1040,
#'                height = 1040)
#'
#' # Export a snap shoot of an open RGL device directly to file
#' plotD3mirt(g,
#'            constructs = TRUE)
#' rgl::rgl.snapshot('RGLdevice.png',
#'                     fmt = 'png')
#' }
#' @export
plotD3mirt <- function (x, scale = FALSE, hide = FALSE, diff.level = NULL, items = NULL, item.names = TRUE,  item.lab = NULL,
                        constructs = FALSE, construct.lab = NULL, adjust.lab = c(0.5, -0.8),
                        x.lab = "X", y.lab="Y", z.lab="Z", title="", line = -5,
                        axis.scalar = 1.1, axis.length = NULL, axis.col = "black", axis.points = "black",
                        points = TRUE, axis.ticks = TRUE, nticks = 4,  width.rgl.x = 1040, width.rgl.y= 1040, view = c(15, 20, 0.6),
                        show.plane = TRUE, plane.col = "grey80", background = "white",
                        type = "rotation", col = c("black", "grey20", "grey40", "grey60", "grey80"),
                        arrow.width = 0.6, n = 20, theta = 0.2, barblen = 0.03,
                        c.scalars = c(1,1),
                        c.type = "rotation", c.col = "black", c.arrow.width = 0.6,
                        c.n = 20, c.theta = 0.2, c.barblen = 0.03,
                        profiles = NULL, levels = NULL, sphere.col = c("black", "grey20", "grey40", "grey60", "grey80"), spheres.r = 0.05,
                        ci = FALSE, ci.level = 0.95, ellipse.col = "grey80", ellipse.alpha = 0.20, ...){
  if (!isa(x, "D3mirt")) stop("Input object must be of class D3mirt")
  rgl::open3d()
  rgl::par3d(windowRect = 50 + c( 0, 0, width.rgl.x, width.rgl.y))
  rgl::bg3d(color = background)
  rgl::view3d(theta = view[1], phi = view[2], zoom = view[3])
  if (is.null(axis.length)){
    if (!is.numeric(axis.scalar)) stop("Elements in axis.scalar are not numeric")
    if (length(axis.scalar) > 1) stop ("The axis.scalar vector must be of length one")
    if (is.null(ncol(x$dir.vec))){
    ax <- x$dir.vec
    low <- as.data.frame(ax[1])
    hig <- as.data.frame(ax[length(ax)])
    xaxis.min <- min(low[,1])*axis.scalar
    xaxis.max <- max(hig[,1])*axis.scalar
    yaxis.min <- min(low[,2])*axis.scalar
    yaxis.max <- max(hig[,2])*axis.scalar
    zaxis.min <- min(low[,3])*axis.scalar
    zaxis.max <- max(hig[,3])*axis.scalar
  } else {
    ax <- x$dir.vec
    xaxis.min <- min(ax[,1])*axis.scalar
    xaxis.max <- max(ax[,1])*axis.scalar
    yaxis.min <- min(ax[,2])*axis.scalar
    yaxis.max <- max(ax[,2])*axis.scalar
    zaxis.min <- min(ax[,3])*axis.scalar
    zaxis.max <- max(ax[,3])*axis.scalar
  }
  } else {
    if (!is.numeric(axis.length)) stop("Elements in axis.length are not numeric")
    if (length(axis.length) > 6) warning("The axis.length argument contains too many indicators")
    if (length(axis.length) < 6){
      a <-  rep(axis.length[length(axis.length)], (6-length(axis.length)))
      axis.length <- append(axis.length, a)
    }
    xaxis.max <- axis.length[1]
    xaxis.min <- axis.length[2]
    yaxis.max <- axis.length[3]
    yaxis.min <- axis.length[4]
    zaxis.max <- axis.length[5]
    zaxis.min <- axis.length[6]
  }
  xaxis <- c(-abs(xaxis.min), abs(xaxis.max))
  yaxis <- c(-abs(yaxis.min), abs(yaxis.max))
  zaxis <- c(-abs(zaxis.min), abs(zaxis.max))
  rgl::segments3d(xaxis, c(0, 0), c(0, 0), color = axis.col)
  rgl::segments3d(c(0, 0), yaxis, c(0, 0), color = axis.col)
  rgl::segments3d(c(0, 0), c(0, 0), zaxis, color = axis.col)
  if (axis.ticks == TRUE){
    if (!is.numeric(nticks)) stop("Elements in nticks are not numeric")
    if (length(nticks) > 3) warning("The nticks argument contains too many indicators")
    if (length(nticks) < 3){
      a <-  rep(nticks[length(nticks)], (3-length(nticks)))
      nticks <- append(nticks, a)
      }
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
    rgl::material3d(color = plane.col)
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
          if(ncol(vec) == 1) stop("The data only has one level of difficulty")
          m <- items*2-1
          sapply(m, function(x){
            rgl::arrow3d(vec[x,], vec[x+1,], type = type, col = col[1], width = arrow.width, n = n, theta = theta, barblen = barblen)})
        }
      } else {
        if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
        if(!is.null(ncol(vec))) stop("The data only has one level of difficulty")
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
        })
      }
    }
    else if (!is.null(diff.level)) {
      if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
      if(!is.null(ncol(vec))) stop("The data only has one level of difficulty")
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
        sapply(seq(from = 1, to = nrow(vec), by=2), function(i){
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
        if (is.null(item.lab)){
        inames <- rownames(x$loadings)
        dl <-  x$dir.vec[[diff.level]]
        sapply(seq(nrow(x$mdisc)), function(i){
          rgl::text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
                      adj = adjust.lab, size = 2)
        })
        } else {
          if(!length(item.lab) <= nrow(x$loadings)) warning("There are more item labels than items")
          if(length(item.lab) < nrow(x$loadings)) warning("There are too few item labels")
          max <-  x$dir.vec[[diff.level]]
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          } )
        }
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
          if(!length(item.lab) <= length(items)) warning("There are more item labels than items in the items list")
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
          if(!length(item.lab) <= length(items)) warning("There are more item labels than items in the items list")
          if(length(item.lab) < length(items)) warning("There are too few item labels")
          dl <-  as.data.frame(x$dir.vec[diff.level, drop = FALSE])
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
        if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
        if(!is.null(ncol(vec))) stop("The data only has one level of difficulty")
        if(diff.level > ncol(x$mdiff)) stop("The argument for difficulty level is too high")
        v <- vec[[diff.level]]
        m <- items*2-1
        sapply(m, function(i){
          rgl::arrow3d(v[i,], v[i+1,], type = type, col = col[diff.level], width = arrow.width, n = n, theta = theta, barblen = barblen)
        })
      }
    }
    else if (!is.null(diff.level)) {
      if(!diff.level== round(diff.level)) stop("Difficulty level must be indicated with integer values")
      if(!is.null(ncol(vec))) stop("The data only has one level of difficulty")
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
        sapply(seq(from = 1, to = nrow(vec), by=2), function(i){
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
        if (is.null(item.lab)){
          inames <- rownames(x$loadings)
          dl <-  x$scal.vec[[diff.level]]
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(dl[(i*2),1],dl[(i*2),2], dl[(i*2),3], text = c(inames[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          })
        } else {
          if(!length(item.lab) <= nrow(x$loadings)) warning("There are more item labels than items")
          if(length(item.lab) < nrow(x$loadings)) warning("There are too few item labels")
          max <-  x$scal.vec[[diff.level]]
          sapply(seq(nrow(x$mdisc)), function(i){
            rgl::text3d(max[(i*2),1],max[(i*2),2], max[(i*2),3], text = c(item.lab[i]), color = axis.col,
                        adj = adjust.lab, size = 2)
          } )
        }
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
          if(!length(item.lab) <= length(items)) warning("There are more item labels than items in the items list")
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
          if(!length(item.lab) <= length(items)) warning("There are more item labels than items in the items list")
          if(length(item.lab) < length(items)) warning("There are too few item labels")
          dl <-  as.data.frame(x$scal.vec[diff.level, drop = FALSE])
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
      rgl::arrow3d(cvec[x,]*c.scalars[2], cvec[x+1,]*c.scalars[1], type = c.type, col = c.col, width = c.arrow.width, n = c.n, theta = c.theta, barblen = c.barblen)
    })
    if (!is.null(construct.lab) && constructs == TRUE){
      if(!length(construct.lab) <= nrow(x$c.vec)) warning("There are more construct labels than constructs")
      clab <-  x$c.vec*c.scalars[1]
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
    if (!is.null(levels)){
      grad <- function (levels, sphere.col){
      levels <- as.factor(levels)
      if (nlevels(levels) > length(sphere.col)) stop ("There number of factor levels are more than the number of available sphere colors")
      color <- sphere.col[as.numeric(levels)]
      names(color) <- as.vector(levels)
      color
      }
      rgl::spheres3d(x,y,z, radius = spheres.r, color = grad(levels, sphere.col))
      if (ci == TRUE){
        ellipse <- rgl::ellipse3d(cov(cbind(x,y,z)),
                                  centre=c(mean(x), mean(y), mean(z)), level = ci.level)
        rgl::shade3d(ellipse, col = ellipse.col, alpha = ellipse.alpha)
    }
    } else {
    rgl::spheres3d(x,y,z, radius = spheres.r, color = sphere.col[1])
      if (ci == TRUE){
        ellipse <- rgl::ellipse3d(cov(cbind(x,y,z)),
                                  centre=c(mean(x), mean(y), mean(z)), level = ci.level)
        rgl::shade3d(ellipse, col = ellipse.col, alpha = ellipse.alpha)
      }
    }
  }
}
