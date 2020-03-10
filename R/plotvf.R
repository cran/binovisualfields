#' Plots visual field
#'
#' \code{plotvf} plots a figure of a visual field matrix with sensitivity threshold values
#' @param xs A vector of horizontal coordinates. The length of it must be either 10 for a monocular or 25 for a binocular visual field plot.
#' @param vf A matrix of either a left/right monocular visual field or a binocular visual field sensitivity values
#' @param title A string for the name of the plot.
#' @return A plot of a monocular or binocular visual field.
#' @importFrom graphics box par plot points text
#' @importFrom utils head tail
#' @examples
#' m_xs <- seq(-27, 27, length.out = 10)
#' rghtvf <- matrix(c(
#' NA, NA, NA, 30, 30, 30, 30, NA, NA, NA,
#' NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
#' NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
#' 30, 30, 30, 30, 30, 30, 30, 30, 30, NA,
#' 30, 30, 30, 30, 30, 30, 30,  0, 30, NA,
#' NA, 30, 30, 30, 30, 30, 30, 30, 30, NA,
#' NA, NA, 30, 30, 30, 30, 30, 30, NA, NA,
#' NA, NA, NA, 30, 30, 30, 30, NA, NA, NA
#' ), ncol=10, byrow=TRUE)
#' plotvf(m_xs, rghtvf, title='right visual field')
#' @section Warning:
#' the length of xs and the number of columns of the vf must be the same and with a value of either 10 or 20.
#' @export
plotvf <- function(xs, vf, title = "") {
    if (getOption("warn") > 0) {
        stopifnot(length(xs) %in% c(10, 20),
                  ncol(vf) %in% c(10, 20),
                  length(xs) == ncol(vf))
    }
    oldmar <- graphics::par(no.readonly = TRUE)$mar
    on.exit(graphics::par(mar=oldmar))
    graphics::par(mar = c(2, 2, 2, 2))
    graphics::plot(0, 0, ylim = c(-22, 22), xlim = range(xs), type = "n", main = title)
    ys = seq(21, -21, -6)
    n <- length(xs)
    y <- utils::head(ys, 1)
    for (i in 1:nrow(vf)) {
        for (j in 1:ncol(vf)) {
            if (!is.na(vf[i, j]))
                graphics::points(xs[j], y,
                       pch = 22,
                       cex = 5,
                       col = NA,
                       bg = c(colfunc(35), utils::tail(colfunc(35), 1), utils::tail(colfunc(35),1))[vf[i, j] + 1])
        }
        graphics::text(xs, rep(y, n), round(vf[i, ]), cex = 1, col = sapply(vf[i, ], get_inv_col))
        y <- y - 6
    }
    graphics::box()
}

#' Plots visual field
#'
#' \code{plotvf_2} plots a figure of a binocular visual field matrix with sensitivity threshold values with missing locations
#' @param xs A vector of binocular visual field horizontal coordinates. The length of it must be 20 for a binocular visual field plot
#' @param vf A matrix of a binocular visual field sensitivity values
#' @param vf_norm A matrix a binocular healthy visual field sensitivity values for a specified distance plane
#' @param title A string for the name of the plot
#' @return A plot of a monocular or binocular visual field
#' @importFrom graphics box par plot points text
#' @importFrom utils head tail
#' @examples
#' c_xs <- seq(-57, 57, length.out = 20)
#' cvf <- matrix(c(
#' NA, NA, NA, NA, NA, NA, NA, NA, 30, 20, 20, 30, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, NA, 30, 20,  0,  0, 20, 30, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, 30, 30, 20,  0,  0, 20, 30, 30, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, 30, 30, 30, 20, 20, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA, NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA, NA, NA
#' ), ncol=20, byrow=TRUE)
#' cvf_norm <- matrix(c(
#'  NA, NA, NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, NA, 30, 30, 30, 30, NA, NA, NA, NA, NA, NA, NA, NA
#' ), ncol=20, byrow=TRUE)
#' plotvf_2(c_xs, cvf, cvf_norm, title='integrated visual field')
#' @section Warning:
#' the length of xs and the number of columns of the vf must be the same and with a value of either  25.
#' @export

plotvf_2 <- function(xs, vf, vf_norm, title = "") {
  if (getOption("warn") > 0) {
    stopifnot(length(xs) == 20,
              ncol(vf) == 20,
              ncol(vf_norm) == 20)
  }
  oldmar <- graphics::par(no.readonly = TRUE)$mar
  on.exit(graphics::par(mar=oldmar))
  graphics::par(mar = c(2, 2, 2, 2))
  # plot(0, 0, ylim = c(-22, 22), xlim = range(xs), type = "n", axes = FALSE)
  plot(0, 0, ylim = c(-22, 22), xlim = c(-30, 30), type = "n", main = title)
  ys = seq(21, -21, -6)
  n <- length(xs)
  y <- head(ys, 1)
  for (i in 1:nrow(vf)) {
    for (j in 1:ncol(vf)) {
      if (!is.na(vf[i, j]))
        points(xs[j], y,
               pch = 22,
               cex = 6,
               col = NA,
               bg = c(colfunc(35), tail(colfunc(35), 1), tail(colfunc(35),1))[vf[i, j] + 1])
    }

    text(xs, rep(y, n), round(vf[i, ]), cex = 1, col = sapply(vf[i, ], get_inv_col))
    y <- y - 6
  }
  vf_d <- is.na(vf_norm) != is.na(vf)
  vf_d_n <- ifelse(vf_d=="FALSE", NA, 0)
  yy <- head(ys, 1)
  for (i in 1:nrow(vf_d_n)) {
    for (j in 1:ncol(vf_d_n)) {
      if (!is.na(vf_d_n[i, j]))
        points(xs[j], yy,
               pch = 22,
               cex = 5,
               col = c(colfunc(35), tail(colfunc(35), 1), tail(colfunc(35),1))[vf_d_n[i, j] + 1])
    }

    # text(xs, rep(y, n), round(vf[i, ]), cex = 1, col = sapply(vf[i, ], get_inv_col))
    yy <- yy - 6
  }
}
