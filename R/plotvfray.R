#' Plots binocular visual field rays
#'
#' \code{plotvfray} plots a figure showing how the left and right visual field sensitivity threshold data interact in the simulated binocular visual field.
#' @param leftvf An 8 by 10 matrix of sensitivity threshold data for the left visual field
#' @param rghtvf An 8 by 10 matrix of sensitivity threshold data for the right visual field
#' @param lefttheta A number left eye rotating angle in radian
#' @param righttheta  A number right eye rotating angle in radian
#' @param fixdist A 2 element vector the coordinates of the fixation point in cartesian system in mm.
#' @param distplane A number object distance in mm range from 0 to a maximum of 1500 mm.
#' @importFrom graphics par plot text segments
#' @importFrom stats approx
#' @return A plot of binocular visual field rays from a top view with left eye on the top.
#' @examples
#'
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
#'
#' leftvf <- rghtvf[, 10:1]
#'
#' plotvfray(leftvf, rghtvf, -.05, .05, c(700, 0), 1000)
#'
#' @section Warning:
#' the unit of fixdist, pd and distplane must be in mm.
#' @export
plotvfray <- function(leftvf, rghtvf, lefttheta, righttheta, fixdist, distplane) {
    if (getOption("warn") > 0) {
            stopifnot(nrow(leftvf) == 8, ncol(leftvf) == 10,
                      nrow(rghtvf) == 8, ncol(rghtvf) == 10,
                      is.numeric(lefttheta), length(lefttheta) == 1,
                      is.numeric(righttheta), length(righttheta) == 1,
                      is.numeric(fixdist), length(fixdist) == 2,
                      is.numeric(distplane), distplane <= 1500
                      )
    }
    oldmar <- graphics::par(no.readonly = TRUE)$mar
    on.exit(graphics::par(mar=oldmar))
    graphics::par(mar = c(4, 4, 1.3, 1))
    m_xs <- seq(-27, 27, length.out = 10)
    maxdistance <- 1500
    plen <- 2000
    pd <- .ddivfEnv$pd_gender[2]

    graphics::plot(c(0, 0, 0, fixdist[1]), c(-pd/2, 0, pd/2, fixdist[2]),
         ylim = c(maxdistance, -maxdistance),
         xlim = c(0, maxdistance),
         pch = 19,
         col = c("red", "black", "blue", "green"),
         xlab = "mm",
         ylab = "mm",
         las = 1)
    graphics::text(c(0, 0, 0, fixdist[1]) - 2,
         c(-pd/2, 0, pd/2, fixdist[2]),
         c("L", "P", "R", "F"),
         cex = 0.75)
    plotrix::draw.arc(0, 0, distplane, deg1 = -63, deg2 = 63, col = "violet", lwd = 2)

    for (a in seq(-27, 27, 6)) {
        # left eye
        end_ray <- c(0, -pd/2) + rotate(c(plen, 0), -lefttheta + a/180 * pi)
        # print(end_ray)
        db <- stats::approx(m_xs, leftvf[5,], a)$y
        # db <- ifelse(is.na(db), 0, db)
        # print(db)
        # segments(0, -pd/2, end_ray[1], end_ray[2], col = ifelse(is.na(db), "red", colfunc(35)[db+1]), lty = 1)
        graphics::segments(0, -pd/2, end_ray[1], end_ray[2], col = colfunc(35)[db+1], lty = 1)
        # right eye
        end_ray <- c(0, pd/2) + rotate(c(plen, 0), -righttheta + a/180 * pi)
        # print(end_ray)
        db <- approx(m_xs, rghtvf[5,], a)$y
        # print(db)
        graphics::segments(0, pd/2, end_ray[1], end_ray[2], col = colfunc(35)[db+1], lty = 1)
    }
}

