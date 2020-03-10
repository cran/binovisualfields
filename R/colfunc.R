#' Creates a color scheme for visual field plots
#'
#' \code{colfunc} creates a color scheme visualising the dB values in visual field matrices with darker color corresponding to lower dB values
#' @param n A positive integer specifying the number of color gradients used in visual field plots and is default to 35
#' @return A color mapping function
#' @importFrom grDevices colorRamp rgb
#' @examples
#' colfunc(35)
#' @export
colfunc <- function(n = 35) {
    if (getOption("warn") > 0) {
        stopifnot(length(n) == 1, n > 0, n%%1==0)
    }
    rr <- rev(c(255, 248, 238, 140, 139, 26, 0, 0))
    gg <- rev(c(255, 246, 238, 195, 31, 19, 10, 0))
    bb <- rev(c(255, 201, 152, 90, 53, 50, 19, 0))
    colors <- grDevices::rgb(red = rr, green = gg, blue = bb, maxColorValue = 255, names = paste("gc", 1:length(rr), sep = "."))
    ramp <- grDevices::colorRamp(colors)
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
        grDevices::rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255) else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
}
